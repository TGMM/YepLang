use super::helpers::{
    convert_type_to_metadata, convert_value_to_metadata, BlockType, Compiler, ExpectedExprType,
    ScopeMarker, ScopedVal, ScopedVar, DEFAULT_TYPES,
};
use crate::{
    ast::{
        ArrayVal, Assignment, BExpr, BOp, Block, BoolLiteral, BoolUnaryOp, Destructure, DoWhile,
        Expr, ExternDecl, ExternType, FnCall, FnDecl, For, If, Indexing, NumericLiteral,
        NumericUnaryOp, PrimitiveVal, Return, Stmt, TopBlock, ValueVarType, VarDecl, VarType,
        While,
    },
    compiler::helpers::{FnRetVal, ScopedFunc},
};
use inkwell::{
    module::Linkage,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use std::{collections::VecDeque, mem::transmute, path::Path};

const MAIN_FN_NAME: &str = "main";

pub fn convert_to_type_enum<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    vvt: &ValueVarType,
) -> BasicTypeEnum<'ctx> {
    let ctx = compiler.context;
    let basic_vtype: Option<BasicTypeEnum> = match &vvt.vtype {
        VarType::I8 | VarType::U8 => ctx.i8_type().as_basic_type_enum().into(),
        VarType::I16 | VarType::U16 => ctx.i16_type().as_basic_type_enum().into(),
        VarType::I32 | VarType::U32 => ctx.i32_type().as_basic_type_enum().into(),
        VarType::I64 | VarType::U64 => ctx.i64_type().as_basic_type_enum().into(),
        VarType::I128 | VarType::U128 => ctx.i128_type().as_basic_type_enum().into(),
        VarType::F32 => ctx.f32_type().as_basic_type_enum().into(),
        VarType::F64 => ctx.f64_type().as_basic_type_enum().into(),
        VarType::Boolean => ctx.bool_type().as_basic_type_enum().into(),
        other => panic!("{} is not a valid basic value", other),
    };

    let mut final_type = basic_vtype.unwrap();
    for dim in vvt.array_dimensions.iter().rev() {
        final_type = final_type.array_type(*dim).as_basic_type_enum()
    }
    for _ in 0..vvt.pointer_nesting_level {
        final_type = final_type
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum()
    }

    final_type
}

pub fn declare_variable<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
    var_name: String,
    value: ScopedVar<'ctx>,
) {
    if let Some(redeclared_var) = compiler.curr_scope_vars.remove(&var_name) {
        // Var was already declared so we push it to the stack
        // to recover it later
        compiler
            .scope_stack
            .push(ScopeMarker::Var(var_name.clone(), Some(redeclared_var)))
    } else {
        // Var was not declared so we push it to the stack
        // to delete it later
        compiler
            .scope_stack
            .push(ScopeMarker::Var(var_name.clone(), None))
    }

    compiler
        .curr_scope_vars
        .insert(var_name, ScopedVal::Var(value));
}

pub fn codegen_top_block(compiler: &mut Compiler, top_block: TopBlock) {
    let fn_type = compiler.context.i32_type().fn_type(&[], false);
    let fun = compiler
        .module
        .add_function(MAIN_FN_NAME, fn_type, Some(Linkage::External));
    let entry_basic_block = compiler.context.append_basic_block(fun, "entry");
    compiler.builder.position_at_end(entry_basic_block);
    compiler.basic_block_stack.push(entry_basic_block);

    // Codegen all statements
    codegen_block(compiler, top_block.0, BlockType::GLOBAL);

    let int_zero = compiler.context.i32_type().const_zero();
    compiler.builder.build_return(Some(&int_zero));
}

fn codegen_block(compiler: &mut Compiler, block: Block, mut block_type: BlockType) {
    // Entering a block means the block type is not global anymore
    block_type.remove(BlockType::GLOBAL);
    block_type.insert(BlockType::LOCAL);

    if !(block_type == BlockType::FUNC) {
        compiler.scope_stack.push(ScopeMarker::ScopeBegin);
    }

    for stmt in block.stmts {
        codegen_stmt(compiler, stmt, block_type);
    }

    // Now we swap
    while let Some(sm) = compiler.scope_stack.pop() {
        // Scope has ended, exit loop
        match sm {
            ScopeMarker::ScopeBegin => break,
            ScopeMarker::Var(id, Some(var_val)) => {
                // Var existed before, we return it to it's previous value
                compiler.curr_scope_vars.remove(&id);
                compiler.curr_scope_vars.insert(id, var_val);
            }
            ScopeMarker::Var(id, None) => {
                // Var didn't exist before, we just remove it from var map
                compiler.curr_scope_vars.remove(&id);
            }
        }
    }
}

fn codegen_stmt(compiler: &mut Compiler, stmt: Stmt, block_type: BlockType) {
    match stmt {
        Stmt::Assignment(assignment) => codegen_assignment(compiler, assignment),
        Stmt::Expr(expr) => _ = codegen_rhs_expr(compiler, expr, None),
        Stmt::ClassDecl(_) => todo!(),
        Stmt::FnDecl(fn_decl) => codegen_fn_decl(compiler, fn_decl, block_type),
        Stmt::For(for_) => codegen_for(compiler, for_, block_type),
        Stmt::While(while_) => codegen_while(compiler, while_, block_type),
        Stmt::DoWhile(do_while) => codegen_do_while(compiler, do_while, block_type),
        Stmt::If(if_) => {
            codegen_if(compiler, if_, block_type);
        }
        Stmt::Block(block) => codegen_block(compiler, block, block_type),
        Stmt::VarDecl(var_decl) => codegen_var_decl(compiler, var_decl, block_type),
        Stmt::ExternDecl(extern_decl) => codegen_extern_decl(compiler, extern_decl),
        Stmt::Return(return_) => codegen_return(compiler, return_, block_type),
    }
}

pub fn codegen_return(compiler: &Compiler, return_: Return, block_type: BlockType) {
    let ret_expr = return_.0;
    let fn_ret_val = compiler.curr_func_ret_val.clone();

    if ret_expr.is_some() && fn_ret_val.is_none() {
        panic!("Void functions can't return values");
    }

    // Return is void
    if ret_expr.is_none() {
        // Returning void on a non-void function
        if fn_ret_val.is_some() {
            panic!("Non-void functions must return a value");
        }

        if block_type.contains(BlockType::FUNC) {
            compiler.builder.build_return(None);
        } else {
            panic!("Return statements must be inside a function");
        }
    }

    let ret_expr = ret_expr.unwrap();

    let FnRetVal {
        val: ret_val_ptr,
        vtype: expected_ret_type,
        ret_bb,
    } = fn_ret_val.unwrap();

    let (ret_val, ret_type) = codegen_rhs_expr(compiler, ret_expr, Some(&expected_ret_type))
        .expect("Invalid value for function return");

    let expected_ret_type: ValueVarType = expected_ret_type;
    if ret_type != expected_ret_type {
        panic!(
            "Invalid type for returned value, expected {:#?}, got {:#?}",
            expected_ret_type, ret_type
        );
    }

    if block_type.contains(BlockType::FUNC) {
        compiler.builder.build_store(ret_val_ptr, ret_val);
        compiler.builder.build_unconditional_branch(ret_bb);
    } else {
        panic!("Return statements must be inside a function");
    }
}

pub fn codegen_if(compiler: &mut Compiler, if_: If, mut block_type: BlockType) {
    block_type.insert(BlockType::IF);

    let parent_block = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let then_block = compiler.context.append_basic_block(parent_block, "then");
    let else_block = compiler.context.append_basic_block(parent_block, "else");
    let merge_block = compiler.context.append_basic_block(parent_block, "ifcont");

    let if_expr = codegen_rhs_expr(
        compiler,
        if_.if_expr,
        Some(&ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }),
    )
    .expect("Invalid expression for if condtition")
    .0
    .into_int_value();

    // If
    compiler
        .builder
        .build_conditional_branch(if_expr, then_block, else_block);

    // Then
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, if_.if_block, block_type);
    compiler.builder.build_unconditional_branch(merge_block);

    // Else
    compiler.builder.position_at_end(else_block);
    if let Some(else_b) = if_.else_b {
        codegen_block(compiler, else_b, block_type);
    }
    compiler.builder.build_unconditional_branch(merge_block);

    compiler.builder.position_at_end(merge_block);
}

pub fn codegen_while(compiler: &mut Compiler, while_: While, mut block_type: BlockType) {
    block_type.insert(BlockType::WHILE);

    let parent_block = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let comp_block = compiler
        .context
        .append_basic_block(parent_block, "while_cond");
    let then_block = compiler
        .context
        .append_basic_block(parent_block, "while_then");
    let merge_block = compiler
        .context
        .append_basic_block(parent_block, "while_cont");

    // While
    compiler.builder.build_unconditional_branch(comp_block);
    compiler.builder.position_at_end(comp_block);
    let while_expr = codegen_rhs_expr(
        compiler,
        while_.while_cond,
        Some(&ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }),
    )
    .expect("Invalid expression for while condtition")
    .0
    .into_int_value();
    compiler
        .builder
        .build_conditional_branch(while_expr, then_block, merge_block);

    // Then
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, while_.block, block_type);
    compiler.builder.build_unconditional_branch(comp_block);

    compiler.builder.position_at_end(merge_block);
}

pub fn codegen_do_while(compiler: &mut Compiler, do_while: DoWhile, mut block_type: BlockType) {
    block_type.insert(BlockType::WHILE);

    let parent_block = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let then_block = compiler
        .context
        .append_basic_block(parent_block, "while_then");
    let comp_block = compiler
        .context
        .append_basic_block(parent_block, "while_cond");
    let merge_block = compiler
        .context
        .append_basic_block(parent_block, "while_cont");

    // Do
    compiler.builder.build_unconditional_branch(then_block);
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, do_while.do_block, block_type);
    compiler.builder.build_unconditional_branch(comp_block);

    // While
    compiler.builder.position_at_end(comp_block);
    let while_expr = codegen_rhs_expr(
        compiler,
        do_while.while_cond,
        Some(&ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }),
    )
    .expect("Invalid expression for do while condtition")
    .0
    .into_int_value();
    compiler
        .builder
        .build_conditional_branch(while_expr, then_block, merge_block);

    // Cont
    compiler.builder.position_at_end(merge_block);
}

pub fn codegen_for(compiler: &mut Compiler, for_: For, mut block_type: BlockType) {
    block_type.insert(BlockType::WHILE);

    let parent_block = compiler
        .builder
        .get_insert_block()
        .unwrap()
        .get_parent()
        .unwrap();

    let decl_block = compiler
        .context
        .append_basic_block(parent_block, "for_decl");
    let comp_block = compiler
        .context
        .append_basic_block(parent_block, "for_cond");
    let postfix_block = compiler
        .context
        .append_basic_block(parent_block, "for_postfix");
    let then_block = compiler
        .context
        .append_basic_block(parent_block, "for_then");
    let merge_block = compiler
        .context
        .append_basic_block(parent_block, "for_cont");

    // Decl stmt
    compiler.builder.build_unconditional_branch(decl_block);
    compiler.builder.position_at_end(decl_block);
    if let Some(stmt) = for_.decl_stmt {
        codegen_stmt(compiler, *stmt, block_type);
    }
    compiler.builder.build_unconditional_branch(comp_block);

    // Postfix stmt
    compiler.builder.position_at_end(postfix_block);
    if let Some(stmt) = for_.postfix_stmt {
        codegen_stmt(compiler, *stmt, block_type);
    }
    compiler.builder.build_unconditional_branch(comp_block);

    // Block
    compiler.builder.position_at_end(then_block);
    codegen_block(compiler, for_.block, block_type);
    compiler.builder.build_unconditional_branch(postfix_block);

    // Cond
    compiler.builder.position_at_end(comp_block);
    let cond_expr = if let Some(cmp_expr) = for_.cmp_expr {
        codegen_rhs_expr(
            compiler,
            cmp_expr,
            Some(&ValueVarType {
                vtype: VarType::Boolean,
                array_dimensions: VecDeque::new(),
                pointer_nesting_level: 0,
            }),
        )
        .expect("Invalid condtition expression in for loop")
        .0
        .into_int_value()
    } else {
        // Default value is true
        compiler.context.bool_type().const_int(1, false)
    };

    // Execution
    compiler
        .builder
        .build_conditional_branch(cond_expr, then_block, merge_block);

    // Cont
    compiler.builder.position_at_end(merge_block);
}

pub fn codegen_fn_decl(compiler: &mut Compiler, fn_decl: FnDecl, block_type: BlockType) {
    let fn_name = fn_decl.fn_id.0;
    let param_types = fn_decl
        .args
        .iter()
        .map(|(d, vvt)| {
            if !matches!(d, Destructure::Id(_)) {
                panic!("Destructuring of arguments is not supported yet");
            }
            convert_type_to_metadata(convert_to_type_enum(compiler, vvt))
        })
        .collect::<Vec<_>>();

    let ret_type = fn_decl.ret_type.clone().unwrap_or(ValueVarType {
        vtype: VarType::Void,
        array_dimensions: VecDeque::new(),
        pointer_nesting_level: 0,
    });
    let basic_ret_type = convert_to_type_enum(compiler, &ret_type);
    let fn_type = basic_ret_type.fn_type(&param_types, false);
    let linkage = if matches!(block_type, BlockType::GLOBAL) {
        Linkage::External
    } else {
        Linkage::Internal
    };
    let fun = compiler
        .module
        .add_function(&fn_name, fn_type, Some(linkage));

    assert_eq!(fun.count_params() as usize, fn_decl.args.len());

    let basic_block_name = format!("{}_bb", fn_name);
    let fn_basic_block = compiler.context.append_basic_block(fun, &basic_block_name);
    compiler.builder.position_at_end(fn_basic_block);
    compiler.basic_block_stack.push(fn_basic_block);

    let ret_basic_block_name = format!("{}_ret_bb", fn_name);
    let ret_basic_block = compiler
        .context
        .append_basic_block(fun, &ret_basic_block_name);
    // We save previous return types
    if let Some(curr_fn) = compiler.curr_func_ret_val.take() {
        compiler.func_ret_val_stack.push(curr_fn);
    }
    // We store the new return type in case there is some
    if let Some(ret_type) = fn_decl.ret_type {
        let basic_type = convert_to_type_enum(compiler, &ret_type);

        compiler.curr_func_ret_val = Some(FnRetVal {
            val: compiler.builder.build_alloca(basic_type, "fn_ret_val"),
            vtype: ret_type,
            ret_bb: ret_basic_block,
        });
    };

    // TODO: The current function call searches for it on
    // the module instead of the var map
    compiler.curr_scope_vars.insert(
        fn_name,
        ScopedVal::Fn(ScopedFunc {
            ptr_val: fun,
            ret_type,
        }),
    );

    compiler.scope_stack.push(ScopeMarker::ScopeBegin);
    for ((arg_destructure, arg_type), arg_val) in fn_decl.args.into_iter().zip(fun.get_param_iter())
    {
        let ty = convert_to_type_enum(compiler, &arg_type);
        match arg_destructure {
            Destructure::Id(id) => {
                let arg_ptr = compiler.builder.build_alloca(ty, &id.0);
                compiler.builder.build_store(arg_ptr, arg_val);
                declare_variable(
                    compiler,
                    id.0,
                    ScopedVar {
                        ptr_val: arg_ptr,
                        var_type: arg_type,
                    },
                );

                arg_ptr
            }
            _ => todo!(),
        };
    }

    codegen_block(compiler, fn_decl.block, BlockType::FUNC);

    // Building the return
    // Every block must end with a br statement, so we jump to the return block
    compiler.builder.build_unconditional_branch(ret_basic_block);
    // We need a reference to the previous block, we get that here
    // This could be either the function block or the if_cont block
    let prev_bb = compiler.builder.get_insert_block().unwrap();
    // We move the return after the if_cont
    ret_basic_block.move_after(prev_bb).unwrap();

    // We jump at the end of the return
    compiler.builder.position_at_end(ret_basic_block);

    let ret_val_load = if let Some(ret_val) = compiler.curr_func_ret_val.take() {
        let ret_val_ptr = ret_val.val;
        let ret_val_ty = convert_to_type_enum(compiler, &ret_val.vtype);
        let ret_val_load = compiler
            .builder
            .build_load(ret_val_ty, ret_val_ptr, "ret_val_load");

        Some(ret_val_load)
    } else {
        None
    };

    compiler
        .builder
        .build_return(ret_val_load.as_ref().map(|ptr| ptr as &dyn BasicValue));

    // Return from function, get back to main basic block
    compiler.basic_block_stack.pop();
    let prev_basic_block = compiler
        .basic_block_stack
        .last()
        .expect("Unexpected error: Basic block stack is empty");
    compiler.builder.position_at_end(*prev_basic_block);

    // We store the new return type
    compiler.curr_func_ret_val = compiler.func_ret_val_stack.pop();
}

pub fn codegen_extern_decl(compiler: &mut Compiler, extern_decl: ExternDecl) {
    let fn_name = &extern_decl.fn_id.0;
    let ret_type = convert_to_type_enum(compiler, &extern_decl.ret_type);

    let mut is_var_args = false;
    let mut param_types = vec![];
    for arg_type in extern_decl.arg_types {
        match arg_type {
            ExternType::Type(vvt) => {
                let ty = convert_to_type_enum(compiler, &vvt);
                let metadata = convert_type_to_metadata(ty);
                param_types.push(metadata);
            }
            // TODO: This should only apply if the var_args is last
            ExternType::Spread => is_var_args = true,
        }
    }

    let fn_type = ret_type.fn_type(&param_types, is_var_args);
    let fun = compiler
        .module
        .add_function(fn_name, fn_type, Some(Linkage::External));

    compiler.curr_scope_vars.insert(
        fn_name.to_string(),
        ScopedVal::Fn(ScopedFunc {
            ptr_val: fun,
            ret_type: extern_decl.ret_type,
        }),
    );
}

pub fn codegen_assignment(compiler: &Compiler, assignment: Assignment) {
    // TODO: The BOP doesn't work
    if assignment.bop.is_some() {
        panic!("Binary operators on assignments aren't supported yet");
    }

    let assignee_ptr = codegen_lhs_expr(compiler, assignment.assignee_expr, None).0;
    let new_val = codegen_rhs_expr(compiler, assignment.assigned_expr, None).unwrap();

    compiler.builder.build_store(assignee_ptr, new_val.0);
}

pub fn codegen_lhs_indexing<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    indexing: Indexing,
    expected_type: Option<&ValueVarType>,
) -> Result<(PointerValue<'ctx>, ValueVarType), String> {
    let (idxd, idxd_type) = codegen_lhs_expr(compiler, indexing.indexed, None);

    if let Some(et) = expected_type {
        if et != &idxd_type {
            panic!("Assigning array of one type to array of another");
        }
    }

    // Array dimensions are u32
    let u32_type = DEFAULT_TYPES.get(&VarType::U32).unwrap();
    let (idxr, idxr_type) = codegen_rhs_expr(compiler, indexing.indexer, Some(u32_type))?;

    if idxr_type != *u32_type {
        panic!("Indexing is only supported for arrays, array indexes must be of type u32");
    }

    let idxr: inkwell::values::IntValue = idxr.into_int_value();

    let element_type = {
        let mut et = idxd_type.clone();
        et.array_dimensions.pop_front();

        et
    };
    let target_data = compiler.target_data.get().unwrap();
    let ptr_size_type = compiler.context.ptr_sized_int_type(target_data, None);
    let zero_ptr = ptr_size_type.const_zero();

    let idxr_ptr = compiler
        .builder
        .build_int_cast(idxr, ptr_size_type, "ptr_size_cast");

    let arr_ty = convert_to_type_enum(compiler, &idxd_type);
    let ret_val_ptr = unsafe {
        compiler
            .builder
            .build_gep(arr_ty, idxd, &[zero_ptr, idxr_ptr], "indexing")
    };

    Ok((ret_val_ptr, element_type))
}

/// Codegen an expression when it's used on the left-hand side of an operation.
pub fn codegen_lhs_expr<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    expr: Expr,
    expected_type: Option<&ValueVarType>,
) -> (PointerValue<'ctx>, ValueVarType) {
    match expr {
        Expr::ParenExpr(None, expr) => codegen_lhs_expr(compiler, *expr, expected_type),
        Expr::Indexing(indexing) => {
            codegen_lhs_indexing(compiler, *indexing, expected_type).unwrap()
        }
        Expr::MemberAccess(_) => todo!(),
        Expr::Id(var_id) => {
            let var_ptr = compiler
                .curr_scope_vars
                .get(&var_id.0)
                // TODO: This should be an user facing error
                // and not a panic
                .expect("Undeclared variable or function");

            match var_ptr {
                ScopedVal::Var(v) => (v.ptr_val, v.var_type.clone()),
                ScopedVal::Fn(_) => todo!(),
            }
        }
        _ => panic!("Left-hand side of assignment can't be ..."),
    }
}

pub fn codegen_bexpr<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    bexpr: BExpr,
    expected_type: ExpectedExprType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), String> {
    let ExpectedExprType {
        expected_lhs_type,
        expected_rhs_type,
        expected_ret_type,
    } = expected_type;

    let BExpr { lhs, op, rhs } = bexpr;
    let lhs = codegen_rhs_expr(compiler, lhs, expected_lhs_type)?;
    let rhs = codegen_rhs_expr(compiler, rhs, expected_rhs_type)?;

    let (lhs_type, rhs_type) = (lhs.1, rhs.1);
    if lhs_type.array_dimensions.len() > 0 || rhs_type.array_dimensions.len() > 0 {
        if lhs_type.array_dimensions.len() != rhs_type.array_dimensions.len() {
            return Err(
                "Operations between different array nesting levels are not supported".to_string(),
            );
        }

        match op {
            BOp::Eq => {
                return Err("Comparing two arrays for equality is not yet supported".to_string());
            }
            unsupported_op => {
                let error = format!(
                    "{} operation is not supported for array types",
                    unsupported_op,
                );

                return Err(error);
            }
        }
    }

    if lhs_type.pointer_nesting_level > 0 || rhs_type.pointer_nesting_level > 0 {
        match op {
            BOp::Eq => {
                return Err("Comparing two pointers for equality is not yet supported".to_string());
            }
            unsupported_op => {
                let error = format!(
                    "{} operation is not supported for pointer types",
                    unsupported_op,
                );

                return Err(error);
            }
        }
    }

    if lhs_type.vtype != rhs_type.vtype {
        return Err(format!(
            "Invalid binary operation between two different types: {} and {}",
            lhs_type.vtype, rhs_type.vtype
        ));
    }

    let operand_type = lhs_type.vtype;
    // Primitive BOp
    let basic_val = match operand_type {
        VarType::I8
        | VarType::U8
        | VarType::I16
        | VarType::U16
        | VarType::I32
        | VarType::U32
        | VarType::I64
        | VarType::U64
        | VarType::I128
        | VarType::U128 => {
            let lhs = lhs.0.into_int_value();
            let rhs = rhs.0.into_int_value();
            let b = compiler.builder;

            let bop_res = match op {
                BOp::Add => b.build_int_add(lhs, rhs, "int_add"),
                BOp::Sub => b.build_int_sub(lhs, rhs, "int_sub"),
                BOp::Mul => b.build_int_mul(lhs, rhs, "int_mul"),
                BOp::Div if operand_type.is_signed() => {
                    b.build_int_signed_div(lhs, rhs, "int_sdiv")
                }
                BOp::Div => b.build_int_unsigned_div(lhs, rhs, "int_udiv"),
                BOp::Mod if operand_type.is_signed() => {
                    b.build_int_signed_rem(lhs, rhs, "int_srem")
                }
                BOp::Mod => b.build_int_unsigned_rem(lhs, rhs, "int_urem"),
                BOp::Pow => return Err(format!("The {} operator is not yet supported", BOp::Pow)),
                BOp::Gt if operand_type.is_signed() => {
                    b.build_int_compare(IntPredicate::SGT, lhs, rhs, "int_sgt_cmp")
                }
                BOp::Gt => b.build_int_compare(IntPredicate::UGT, lhs, rhs, "int_ugt_cmp"),
                BOp::Gte if operand_type.is_signed() => {
                    b.build_int_compare(IntPredicate::SGE, lhs, rhs, "int_sge_cmp")
                }
                BOp::Gte => b.build_int_compare(IntPredicate::UGE, lhs, rhs, "int_uge_cmp"),
                BOp::Lt if operand_type.is_signed() => {
                    b.build_int_compare(IntPredicate::SLT, lhs, rhs, "int_slt_cmp")
                }
                BOp::Lt => b.build_int_compare(IntPredicate::ULT, lhs, rhs, "int_ult_cmp"),
                BOp::Lte if operand_type.is_signed() => {
                    b.build_int_compare(IntPredicate::SLE, lhs, rhs, "int_sle_cmp")
                }
                BOp::Lte => b.build_int_compare(IntPredicate::ULE, lhs, rhs, "int_ule_cmp"),
                BOp::Ne => b.build_int_compare(IntPredicate::NE, lhs, rhs, "int_ne_cmp"),
                BOp::Eq => b.build_int_compare(IntPredicate::EQ, lhs, rhs, "int_eq_cmp"),
            };

            bop_res.as_basic_value_enum()
        }
        VarType::F32 | VarType::F64 => {
            let lhs = lhs.0.into_float_value();
            let rhs = rhs.0.into_float_value();
            let b = compiler.builder;

            let bop_res = match op {
                BOp::Add => b.build_float_add(lhs, rhs, "flt_add").as_basic_value_enum(),
                BOp::Sub => b.build_float_sub(lhs, rhs, "flt_sub").as_basic_value_enum(),
                BOp::Mul => b.build_float_mul(lhs, rhs, "flt_mul").as_basic_value_enum(),
                BOp::Div => b.build_float_div(lhs, rhs, "flt_div").as_basic_value_enum(),
                BOp::Mod => b.build_float_rem(lhs, rhs, "flt_mod").as_basic_value_enum(),
                BOp::Pow => return Err(format!("The {} operator is not yet supported", BOp::Pow)),
                BOp::Gt => b
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "flt_gt_cmp")
                    .as_basic_value_enum(),
                BOp::Gte => b
                    .build_float_compare(FloatPredicate::OGE, lhs, rhs, "flt_ge_cmp")
                    .as_basic_value_enum(),
                BOp::Lt => b
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "flt_lt_cmp")
                    .as_basic_value_enum(),
                BOp::Lte => b
                    .build_float_compare(FloatPredicate::OLE, lhs, rhs, "flt_le_cmp")
                    .as_basic_value_enum(),
                BOp::Ne => b
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "flt_ne_cmp")
                    .as_basic_value_enum(),
                BOp::Eq => b
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "flt_eq_cmp")
                    .as_basic_value_enum(),
            };

            bop_res
        }
        VarType::Void => panic!("Binary operations between void types are invalid"),
        VarType::Boolean => {
            // Bool values are i1, so they are ints
            let lhs = lhs.0.into_int_value();
            let rhs = rhs.0.into_int_value();
            let b = compiler.builder;

            let bop_res = match op {
                BOp::Ne => b.build_int_compare(IntPredicate::NE, lhs, rhs, "bool_eq_cmp"),
                BOp::Eq => b.build_int_compare(IntPredicate::EQ, lhs, rhs, "bool_eq_cmp"),
                unsupported_operation => panic!(
                    "Unsupported operation {} between two booleans",
                    unsupported_operation
                ),
            };

            bop_res.as_basic_value_enum()
        }
        VarType::Char => todo!(),
        VarType::String => todo!(),
        VarType::Custom(_) => panic!("Classes are not yet supported"),
    };

    let expr_type = if !op.is_cmp() {
        rhs_type
    } else {
        ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }
    };

    if let Some(expected_ret_type) = expected_ret_type {
        if expected_ret_type != &expr_type {
            panic!(
                "Expected {:#?} as result of the expression, found {:#?}",
                &expected_ret_type, &expr_type
            );
        }
    }

    Ok((basic_val, expr_type))
}

pub fn codegen_rhs_indexing<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    indexing: Indexing,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), String> {
    let (idxd, idxd_type) = codegen_lhs_expr(compiler, indexing.indexed, None);

    if let Some(et) = expected_type {
        if et != &idxd_type {
            panic!("Assigning array of one type to array of another");
        }
    }

    // Array dimensions are u32
    let u32_type = DEFAULT_TYPES.get(&VarType::U32).unwrap();
    let (idxr, idxr_type) = codegen_rhs_expr(compiler, indexing.indexer, Some(u32_type))?;

    if idxr_type != *u32_type {
        panic!("Indexing is only supported for arrays, array indexes must be of type u32");
    }

    let idxr: inkwell::values::IntValue = idxr.into_int_value();

    let element_type = {
        let mut et = idxd_type.clone();
        et.array_dimensions.pop_front();

        et
    };
    let element_b_type = convert_to_type_enum(compiler, &element_type);

    let target_data = compiler.target_data.get().unwrap();
    let ptr_size_type = compiler.context.ptr_sized_int_type(target_data, None);
    let zero_ptr = ptr_size_type.const_zero();

    let idxr_ptr = compiler
        .builder
        .build_int_cast(idxr, ptr_size_type, "ptr_size_cast");

    let arr_ty = convert_to_type_enum(compiler, &idxd_type);
    let ret_val_ptr = unsafe {
        compiler
            .builder
            .build_gep(arr_ty, idxd, &[zero_ptr, idxr_ptr], "indexing")
    };
    let ret_val = compiler
        .builder
        .build_load(element_b_type, ret_val_ptr, "load_array_val");

    Ok((ret_val, element_type))
}

/// Codegen an expression when it's used on the right-hand side of an operation.
/// * `expected_type` - The expected result type of the expression.
/// Only useful if we're coming from an assignment or the var decl has an explicit type,
/// otherwise inferred to defaults
pub fn codegen_rhs_expr<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    expr: Expr,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), String> {
    match expr {
        // TODO: Handle expr unary operator
        Expr::ParenExpr(_, expr) => codegen_rhs_expr(compiler, *expr, expected_type),
        Expr::BinaryExpr(bexpr) => codegen_bexpr(
            compiler,
            *bexpr,
            ExpectedExprType {
                expected_lhs_type: None,
                expected_rhs_type: None,
                expected_ret_type: expected_type,
            },
        ),
        Expr::PrimitiveVal(primitive_val) => {
            let pv = codegen_primitive_val(compiler, primitive_val, expected_type);
            Ok(pv)
        }
        Expr::FnCall(fn_call) => codegen_fn_call(compiler, *fn_call),
        Expr::Indexing(indexing) => codegen_rhs_indexing(compiler, *indexing, expected_type),
        Expr::MemberAccess(_) => todo!(),
        Expr::Id(var_id) => {
            let var_name = &var_id.0;
            let scoped_val = compiler
                .curr_scope_vars
                .get(var_name)
                .ok_or("Undeclared variable")?;
            let scoped_var = scoped_val
                .clone()
                .into_var()
                .map_err(|_| "Using function pointer as a value is not supported".to_string())?;

            let instruction_name = format!("load_{}", var_id.0);

            let ptr_val = scoped_var.ptr_val;
            let var_val;

            let is_var_const = ptr_val.is_const();
            if let Some(global_var) = compiler.module.get_global(var_name) &&
               let Some(initializer) = global_var.get_initializer() &&
               is_var_const {
                var_val = initializer;
            } else {
                let var_type = convert_to_type_enum(compiler, &scoped_var.var_type);
                var_val = compiler
                    .builder
                    .build_load(var_type, scoped_var.ptr_val, &instruction_name);
            }

            Ok((var_val, scoped_var.var_type))
        }
    }
}

pub fn codegen_fn_call<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    fn_call: FnCall,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), String> {
    let fn_name = match fn_call.fn_expr {
        Expr::Id(id) => id.0,
        _ => return Err("Functions as values are not yet supported".to_string()),
    };
    // TODO: Handle this error user-side
    let function = compiler
        .curr_scope_vars
        .get(&fn_name)
        .ok_or(format!("Function {} does not exist", &fn_name))
        .unwrap()
        .clone()
        .into_fn()
        .unwrap();
    let function_ptr = function.ptr_val;
    let func_ret_ty = function.ret_type;

    let mut args = vec![];
    for arg_expr in fn_call.args {
        // TODO: Match argument expressions to expected types
        let (arg_val, arg_type) = codegen_rhs_expr(compiler, arg_expr, None)?;
        let arg_metadata: BasicMetadataValueEnum = convert_value_to_metadata(arg_val);
        args.push(arg_metadata);
    }

    let instruction_name = format!("call_{}", fn_name);
    let call = compiler
        .builder
        .build_call(function_ptr, &args, &instruction_name);
    let ret_val = call.try_as_basic_value();

    ret_val
        .left()
        .map(|rv| (rv, func_ret_ty))
        .ok_or("Fn call returned void".to_string())
}

pub fn codegen_int_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    int_str: &str,
    uop: Option<NumericUnaryOp>,
    mut expected_type: Option<&ValueVarType>,
) -> (BasicValueEnum<'ctx>, ValueVarType) {
    let int_type;
    let mut u64_val_res: u64;
    let mut unsigned = false;

    if let Some(expected_type_ref) = expected_type {
        if expected_type_ref.array_dimensions.len() > 0 {
            panic!("Unexpected assignment of number to array type");
        }
        if expected_type_ref.pointer_nesting_level > 0 {
            panic!("Unexpected assignment of number to pointer type. Explicit address assignment is not supported.");
        }

        match &expected_type_ref.vtype {
            VarType::I8 => {
                int_type = compiler.context.i8_type();
                let int_val = int_str.parse::<i8>().unwrap();
                let i64_val: i64 = int_val.into();
                u64_val_res = unsafe { transmute(i64_val) };
            }
            VarType::U8 => {
                int_type = compiler.context.i8_type();
                let int_val = int_str.parse::<u8>().unwrap();
                let u64_val: u64 = int_val.into();
                unsigned = true;
                u64_val_res = unsafe { transmute(u64_val) };
            }
            VarType::I16 => {
                int_type = compiler.context.i16_type();
                let int_val = int_str.parse::<i16>().unwrap();
                let i64_val: i64 = int_val.into();
                u64_val_res = unsafe { transmute(i64_val) };
            }
            VarType::U16 => {
                int_type = compiler.context.i16_type();
                let int_val = int_str.parse::<u16>().unwrap();
                let u64_val: u64 = int_val.into();
                unsigned = true;
                u64_val_res = unsafe { transmute(u64_val) };
            }
            VarType::U32 => {
                int_type = compiler.context.i32_type();
                let int_val = int_str.parse::<u32>().unwrap();
                let u64_val: u64 = int_val.into();
                unsigned = true;
                u64_val_res = unsafe { transmute(u64_val) };
            }
            VarType::I64 => {
                int_type = compiler.context.i64_type();
                let int_val = int_str.parse::<i64>().unwrap();
                u64_val_res = unsafe { transmute(int_val) };
            }
            VarType::U64 => {
                int_type = compiler.context.i64_type();
                let int_val = int_str.parse::<u64>().unwrap();
                unsigned = true;
                u64_val_res = int_val;
            }
            VarType::I128 | VarType::U128 => {
                panic!("128 bit integers are not supported.")
            }
            // Since default value is i32, it's the last case
            VarType::I32 | _ => {
                int_type = compiler.context.i32_type();
                let int_val = int_str.parse::<i32>().unwrap();
                let i64_val: i64 = int_val.into();
                u64_val_res = unsafe { transmute(i64_val) };

                let _ = expected_type.insert(&DEFAULT_TYPES.get(&VarType::I32).unwrap());
            }
        }
    } else {
        // Default value is i32
        int_type = compiler.context.i32_type();
        let int_val = int_str.parse::<i32>().unwrap();
        let i64_val: i64 = int_val.into();
        u64_val_res = unsafe { transmute(i64_val) };

        let _ = expected_type.insert(&DEFAULT_TYPES.get(&VarType::I32).unwrap());
    }

    if let Some(uop) = uop {
        match uop {
            NumericUnaryOp::Minus => {
                if unsigned {
                    panic!("Can't apply the minus unary operator to an unsigned integer")
                }
                let mut i64_val: i64 = unsafe { transmute(u64_val_res) };
                i64_val = -i64_val;
                u64_val_res = unsafe { transmute(i64_val) }
            }
            _ => {}
        }
    }

    let basic_val = int_type.const_int(u64_val_res, false).as_basic_value_enum();

    (basic_val, expected_type.unwrap().clone())
}

pub fn codegen_float_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    float_str: &str,
    uop: Option<NumericUnaryOp>,
    expected_type: Option<&ValueVarType>,
) -> (BasicValueEnum<'ctx>, ValueVarType) {
    let float_type;
    let mut f64_val_res: f64;

    if let Some(expected_type) = expected_type {
        if expected_type.array_dimensions.len() > 0 {
            panic!("Unexpected assignment of number to array type");
        }
        if expected_type.pointer_nesting_level > 0 {
            panic!("Unexpected assignment of number to pointer type. Explicit address assignment is not supported.");
        }

        match &expected_type.vtype {
            VarType::F32 => {
                float_type = compiler.context.f32_type();
                let float_val = float_str.parse::<f32>().unwrap();
                f64_val_res = float_val.into();
            }
            VarType::F64 => {
                float_type = compiler.context.f64_type();
                let float_val = float_str.parse::<f64>().unwrap();
                f64_val_res = float_val.into();
            }
            ty => panic!("Invalid int assignment to {}", ty),
        }
    } else {
        // Default value is f32
        float_type = compiler.context.f32_type();
        let float_val = float_str.parse::<f32>().unwrap();
        f64_val_res = float_val.into();
    }

    if let Some(uop) = uop {
        match uop {
            NumericUnaryOp::Minus => {
                f64_val_res = -f64_val_res;
            }
            NumericUnaryOp::Plus => {}
        }
    }

    let basic_val = float_type.const_float(f64_val_res).as_basic_value_enum();

    (
        basic_val,
        expected_type
            .unwrap_or(&ValueVarType {
                vtype: VarType::F32,
                array_dimensions: VecDeque::new(),
                pointer_nesting_level: 0,
            })
            .clone(),
    )
}

pub fn codegen_bool_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    bool_literal: BoolLiteral,
    buop: Option<BoolUnaryOp>,
    expected_type: Option<&ValueVarType>,
) -> (BasicValueEnum<'ctx>, ValueVarType) {
    let bool_type = compiler.context.bool_type();
    let mut bool_val_res: bool;

    if let Some(expected_type) = expected_type {
        if expected_type.array_dimensions.len() > 0 {
            panic!("Unexpected assignment of number to array type");
        }
        if expected_type.pointer_nesting_level > 0 {
            panic!("Unexpected assignment of number to pointer type. Explicit address assignment is not supported.");
        }

        match &expected_type.vtype {
            VarType::Boolean => {
                bool_val_res = bool_literal.0;
            }
            ty => panic!("Invalid int assignment to {}", ty),
        }
    } else {
        bool_val_res = bool_literal.0;
    }

    if let Some(buop) = buop {
        match buop {
            BoolUnaryOp::Not => {
                bool_val_res = !bool_val_res;
            }
        }
    }

    let u64_val: u64 = bool_val_res.into();
    let basic_val = bool_type.const_int(u64_val, false).as_basic_value_enum();

    (
        basic_val,
        expected_type
            .unwrap_or(&ValueVarType {
                vtype: VarType::Boolean,
                array_dimensions: VecDeque::new(),
                pointer_nesting_level: 0,
            })
            .clone(),
    )
}

pub fn codegen_arr_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    arr_val: ArrayVal,
    expected_type: Option<&ValueVarType>,
) -> (BasicValueEnum<'ctx>, ValueVarType) {
    let exprs = arr_val.0;
    let exprs_len: u32 = exprs
        .len()
        .try_into()
        .expect("Array dimensions should fit inside an u32");

    if exprs.is_empty() && expected_type.is_none() {
        panic!("Can't infer type of empty array");
    }
    if let Some(et) = expected_type {
        let expected_dim = et.array_dimensions[0];

        if expected_dim != exprs_len {
            panic!(
                "Array declared with {} elements, but assigned value only has {} elements",
                expected_dim, exprs_len
            );
        }
    }

    let mut element_type = expected_type.map(|et| {
        // Same type but of individual element
        let mut element_type = et.clone();
        element_type.array_dimensions.pop_front();

        element_type
    });

    let mut vals = vec![];
    for expr in exprs {
        let (val, ty_) = codegen_rhs_expr(compiler, expr, element_type.as_ref()).unwrap();
        element_type.get_or_insert(ty_);
        vals.push(val);
    }

    // By this point we should have a type
    let mut resulting_type = element_type.unwrap();
    let element_b_type = convert_to_type_enum(compiler, &resulting_type);
    let array_val = match element_b_type {
        BasicTypeEnum::ArrayType(at) => {
            let array_vals = vals
                .into_iter()
                .map(|v| v.into_array_value())
                .collect::<Vec<_>>();
            at.const_array(&array_vals)
        }
        BasicTypeEnum::FloatType(ft) => {
            let float_vals = vals
                .into_iter()
                .map(|v| v.into_float_value())
                .collect::<Vec<_>>();
            ft.const_array(&float_vals)
        }
        BasicTypeEnum::IntType(it) => {
            let int_vals = vals
                .into_iter()
                .map(|v| v.into_int_value())
                .collect::<Vec<_>>();
            it.const_array(&int_vals)
        }
        BasicTypeEnum::PointerType(pt) => {
            let pt_vals = vals
                .into_iter()
                .map(|v| v.into_pointer_value())
                .collect::<Vec<_>>();
            pt.const_array(&pt_vals)
        }
        BasicTypeEnum::StructType(st) => {
            let struct_vals = vals
                .into_iter()
                .map(|v| v.into_struct_value())
                .collect::<Vec<_>>();
            st.const_array(&struct_vals)
        }
        BasicTypeEnum::VectorType(vt) => {
            let vector_vals = vals
                .into_iter()
                .map(|v| v.into_vector_value())
                .collect::<Vec<_>>();
            vt.const_array(&vector_vals)
        }
    };

    // If we had an inferred type we convert it to an array type
    resulting_type.array_dimensions.push_front(exprs_len);

    (array_val.as_basic_value_enum(), resulting_type)
}

pub fn codegen_primitive_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    primitive_val: PrimitiveVal,
    expected_type: Option<&ValueVarType>,
) -> (BasicValueEnum<'ctx>, ValueVarType) {
    let (bv, vvt) = match primitive_val {
        PrimitiveVal::Number(uop, numeric_literal) => match numeric_literal {
            NumericLiteral::Int(i) => codegen_int_val(compiler, i, uop, expected_type),
            NumericLiteral::Float(f) => codegen_float_val(compiler, f, uop, expected_type),
        },
        PrimitiveVal::Boolean(buop, bool_literal) => {
            codegen_bool_val(compiler, bool_literal, buop, expected_type)
        }
        PrimitiveVal::Char(_) => todo!(),
        PrimitiveVal::String(string) => {
            let str_val = compiler
                .builder
                .build_global_string_ptr(&string, "globstr")
                .as_basic_value_enum();

            (
                str_val,
                ValueVarType {
                    vtype: VarType::String,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0,
                },
            )
        }
        PrimitiveVal::Array(arr_val) => codegen_arr_val(compiler, arr_val, expected_type),
        PrimitiveVal::Struct(_) => todo!(),
    };

    (bv, vvt)
}

fn codegen_var_decl<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
    var_decl: VarDecl,
    block_type: BlockType,
) {
    // TODO: Handle scope specifier
    for decl_as in var_decl.decl_assignments {
        // TODO: Handle destructures instead of only ids
        let id = match decl_as.destructure {
            Destructure::Id(id) => id,
            _ => panic!("Destructures are not supported yet"),
        };

        let initial_expr_val = decl_as.expr.map(|initial_expr| {
            codegen_rhs_expr(compiler, initial_expr, decl_as.var_type.as_ref()).unwrap()
        });
        let (initial_val, inferred_var_type) = match initial_expr_val {
            Some((bve, vvt)) => (Some(bve), Some(vvt)),
            None => (None, None),
        };

        let type_;
        let var_type;
        if let Some(explicit_var_type) = decl_as.var_type {
            type_ = convert_to_type_enum(compiler, &explicit_var_type);
            var_type = explicit_var_type;
        } else if let Some(inferred_var_type) = inferred_var_type {
            type_ = convert_to_type_enum(compiler, &inferred_var_type);
            var_type = inferred_var_type;
        } else {
            panic!("Variables must have an explicit type or an initial value");
        }

        if matches!(block_type, BlockType::GLOBAL) {
            let new_global = compiler.module.add_global(type_, None, &id.0);

            if let Some(initial_val) = initial_val {
                new_global.set_initializer(&initial_val);
            } else {
                let default_value = type_.const_zero();
                new_global.set_initializer(&default_value);
            }

            let global_ptr = new_global.as_pointer_value();
            compiler.curr_scope_vars.insert(
                id.0,
                ScopedVal::Var(ScopedVar {
                    ptr_val: global_ptr,
                    var_type,
                }),
            );

            return;
        }

        let new_local = compiler.builder.build_alloca(type_, &id.0);

        if let Some(initial_val) = initial_val {
            compiler.builder.build_store(new_local, initial_val);
        }

        declare_variable(
            compiler,
            id.0,
            ScopedVar {
                ptr_val: new_local,
                var_type,
            },
        );
    }
}

pub fn compile_to_x86<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
    top_block: TopBlock,
    path: &str,
    file_name: &str,
) -> String {
    Target::initialize_x86(&InitializationConfig::default());
    let triple = TargetTriple::create("x86_64-pc-windows-msvc");
    let target = Target::from_triple(&triple).unwrap();
    let cpu = "generic";
    let features = "";
    let target_machine = target
        .create_target_machine(
            &triple,
            cpu,
            features,
            OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    compiler.module.set_triple(&triple);
    compiler
        .module
        .set_data_layout(&target_machine.get_target_data().get_data_layout());

    let _ = compiler.target_data.set(target_machine.get_target_data());
    codegen_top_block(compiler, top_block);

    let out_path = format!("{path}\\{file_name}");
    compiler
        .module
        .print_to_file(&format!("{out_path}.ll"))
        .unwrap();
    target_machine
        .write_to_file(
            compiler.module,
            FileType::Object,
            Path::new(&format!("{out_path}.o")),
        )
        .unwrap();
    target_machine
        .write_to_file(
            compiler.module,
            FileType::Assembly,
            Path::new(&format!("{out_path}.asm")),
        )
        .unwrap();

    out_path
}
