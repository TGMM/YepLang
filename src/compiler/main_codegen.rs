use super::{
    class_codegen::{codegen_fn_decl, codegen_fn_def, codegen_return},
    control_flow_codegen::{codegen_do_while, codegen_for, codegen_if, codegen_while},
    expr_codegen::{codegen_bexpr, codegen_lhs_expr, codegen_rhs_expr},
    ffi_codegen::codegen_extern_decl,
    helpers::{
        convert_type_to_metadata, convert_value_to_metadata, BlockType, Compiler, CompilerError,
        ExpectedExprType, RuntimeErrors, ScopedVal, ScopedVar, YepTarget,
    },
};
use crate::{
    ast::{Assignment, BExpr, Block, Destructure, Stmt, TopBlock, ValueVarType, VarDecl, VarType},
    parser::main_parser::parse,
};
use inkwell::{
    context::Context,
    module::Linkage,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, FunctionValue},
    AddressSpace, OptimizationLevel,
};
use rustc_hash::FxHashMap;
use std::path::Path;

const MAIN_FN_NAME: &str = "main";

pub fn convert_to_type_enum<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    vvt: &ValueVarType,
) -> Result<BasicTypeEnum<'ctx>, CompilerError> {
    let ctx = compiler.context;
    let basic_vtype: Result<BasicTypeEnum, CompilerError> = match &vvt.vtype {
        VarType::I8 | VarType::U8 => Ok(ctx.i8_type().as_basic_type_enum()),
        VarType::I16 | VarType::U16 => Ok(ctx.i16_type().as_basic_type_enum()),
        VarType::I32 | VarType::U32 => Ok(ctx.i32_type().as_basic_type_enum()),
        VarType::I64 | VarType::U64 => Ok(ctx.i64_type().as_basic_type_enum()),
        VarType::I128 | VarType::U128 => Ok(ctx.i128_type().as_basic_type_enum()),
        VarType::F32 => Ok(ctx.f32_type().as_basic_type_enum()),
        VarType::F64 => Ok(ctx.f64_type().as_basic_type_enum()),
        VarType::Boolean => Ok(ctx.bool_type().as_basic_type_enum()),
        other => Err(format!("{} is not a valid basic value", other)),
    };

    let mut final_type = basic_vtype?;
    for dim in vvt.array_dimensions.iter().rev() {
        final_type = final_type.array_type(*dim).as_basic_type_enum()
    }
    for _ in 0..vvt.pointer_nesting_level {
        final_type = final_type
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum()
    }

    Ok(final_type)
}

pub fn declare_scoped_val<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
    var_name: String,
    value: ScopedVal<'ctx>,
) -> Result<(), CompilerError> {
    let curr_scope_mut = compiler.get_curr_scope_mut()?;
    if curr_scope_mut.get(&var_name).is_some() {
        return Err(format!(
            "Cannot redeclare block-scoped variable or function '{}'.",
            var_name
        ));
    }

    curr_scope_mut.insert(var_name, value);

    Ok(())
}

pub fn codegen_nostd_panic_fn<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
) -> Result<FunctionValue<'ctx>, CompilerError> {
    let panic_fn_ty = compiler.context.void_type().fn_type(&[], false);
    let panic_fn = compiler
        .module
        // TODO: Get this name from another place so it's not duplicated
        .add_function("__yep_panic_handler", panic_fn_ty, Some(Linkage::External));
    let panic_fn_bb = compiler.context.append_basic_block(panic_fn, "panic");
    compiler.builder.position_at_end(panic_fn_bb);

    let donothing_fn_ty = compiler.context.void_type().fn_type(&[], false);
    let donothing_fn = compiler
        .module
        .add_function("llvm.donothing", donothing_fn_ty, None);

    compiler.builder.build_call(donothing_fn, &[], "nop");

    // Infinite loop
    compiler.builder.build_unconditional_branch(panic_fn_bb);

    Ok(panic_fn)
}

pub fn codegen_libc_panic_fn<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
) -> Result<FunctionValue<'ctx>, CompilerError> {
    // TODO: This only works on environments with a libc
    let bmt_i32 = convert_type_to_metadata(compiler.context.i32_type().as_basic_type_enum());
    let exit_fn_ty = compiler.context.void_type().fn_type(&[bmt_i32], false);
    let exit_fn = compiler
        .module
        .add_function("exit", exit_fn_ty, Some(Linkage::External));

    let bmt_i8ptr = convert_type_to_metadata(
        compiler
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
    );
    let printf_fn_ty = compiler.context.i32_type().fn_type(&[bmt_i8ptr], true);
    let printf_fn = compiler
        .module
        .add_function("printf", printf_fn_ty, Some(Linkage::External));

    // TODO: This function should only be declared once on another module
    let panic_fn_ty = compiler.context.void_type().fn_type(&[], false);
    let panic_fn = compiler
        .module
        // TODO: Get this name from another place so it's not duplicated
        .add_function("__yep_panic_handler", panic_fn_ty, Some(Linkage::External));
    let panic_fn_bb = compiler.context.append_basic_block(panic_fn, "panic");
    compiler.builder.position_at_end(panic_fn_bb);

    let exit_code =
        BasicMetadataValueEnum::IntValue(compiler.context.i32_type().const_int(1, false));
    // Allocate error
    let error_msg = "Error\0";
    let error_msg_val = compiler
        .builder
        .build_global_string_ptr(error_msg, "error_msg_str");
    // Print error
    let error_msg_arg = convert_value_to_metadata(error_msg_val.as_basic_value_enum());
    compiler
        .builder
        .build_call(printf_fn, &[error_msg_arg], "call_panic_printf");
    // Exit program
    compiler
        .builder
        .build_call(exit_fn, &[exit_code], "call_panic_exit");
    compiler.builder.build_return(None);

    Ok(panic_fn)
}

pub fn codegen_top_block(
    compiler: &mut Compiler,
    top_block: TopBlock,
) -> Result<(), CompilerError> {
    let fn_type = compiler.context.i32_type().fn_type(&[], false);
    let fun = compiler
        .module
        .add_function(MAIN_FN_NAME, fn_type, Some(Linkage::External));
    let entry_basic_block = compiler.context.append_basic_block(fun, "entry");
    compiler.basic_block_stack.push(entry_basic_block);

    let panic_fn = codegen_libc_panic_fn(compiler)?;
    // Codegen out of bounds error block
    let out_of_bounds_bb = compiler
        .context
        .append_basic_block(fun, "out_of_bounds_err");
    compiler.builder.position_at_end(out_of_bounds_bb);
    // Platform-dependend run-time error
    compiler.builder.build_call(panic_fn, &[], "call_panic");
    compiler.builder.build_unreachable();
    _ = compiler.runtime_errors.oob.insert(out_of_bounds_bb);

    compiler.builder.position_at_end(entry_basic_block);
    // Codegen all statements
    codegen_block(compiler, top_block.0, BlockType::GLOBAL)?;

    let main_ret_ty = compiler.context.i32_type();
    let int_zero = main_ret_ty.const_zero();
    compiler.builder.build_return(Some(&int_zero));

    Ok(())
}

pub fn codegen_block(
    compiler: &mut Compiler,
    block: Block,
    block_type: BlockType,
) -> Result<(), CompilerError> {
    // When we enter a block, we push a new variable scope
    if !(block_type == BlockType::FUNC) {
        compiler.var_scopes.push(FxHashMap::default());
    }

    // Function forward-declaration
    for fn_def in block.stmts.iter() {
        match fn_def {
            Stmt::FnDef(fn_def) => {
                codegen_fn_decl(compiler, &fn_def.fn_signature, block_type)?;
            }
            _ => {}
        };
    }

    for stmt in block.stmts {
        codegen_stmt(compiler, stmt, block_type)?;
    }

    // Then after we're done with it, we pop it
    compiler.var_scopes.pop();

    Ok(())
}

pub fn codegen_stmt(
    compiler: &mut Compiler,
    stmt: Stmt,
    mut block_type: BlockType,
) -> Result<(), CompilerError> {
    match stmt {
        Stmt::Assignment(assignment) => codegen_assignment(compiler, assignment, block_type),
        Stmt::Expr(expr) => codegen_rhs_expr(compiler, expr, None, block_type).map(|(_, _)| ()),
        Stmt::ClassDecl(_) => todo!(),
        Stmt::FnDef(fn_def) => codegen_fn_def(compiler, fn_def, block_type),
        Stmt::For(for_) => codegen_for(compiler, for_, block_type),
        Stmt::While(while_) => codegen_while(compiler, while_, block_type),
        Stmt::DoWhile(do_while) => codegen_do_while(compiler, do_while, block_type),
        Stmt::If(if_) => codegen_if(compiler, if_, block_type),
        Stmt::Block(block) => {
            // Entering a block means the block type is not global anymore
            block_type.remove(BlockType::GLOBAL);
            block_type.insert(BlockType::LOCAL);
            codegen_block(compiler, block, block_type)
        }
        Stmt::VarDecl(var_decl) => codegen_var_decl(compiler, var_decl, block_type),
        Stmt::ExternDecl(extern_decl) => codegen_extern_decl(compiler, extern_decl),
        Stmt::Return(return_) => codegen_return(compiler, return_, block_type),
    }
}

fn codegen_var_decl<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
    var_decl: VarDecl,
    block_type: BlockType,
) -> Result<(), CompilerError> {
    // TODO: Handle scope specifier
    for decl_as in var_decl.decl_assignments {
        // TODO: Handle destructures instead of only ids
        let id = match decl_as.destructure {
            Destructure::Id(id) => {
                if id.0.as_str() == "this" {
                    return Err(
                        "'this' is a reserved keyword and can't be used as a variable name"
                            .to_string(),
                    );
                }

                id
            }
            _ => return Err("Destructures are not supported yet".to_string()),
        };

        let initial_expr_val = decl_as.expr.map(|initial_expr| {
            codegen_rhs_expr(
                compiler,
                initial_expr,
                decl_as.var_type.as_ref(),
                block_type,
            )
        });

        let (initial_val, inferred_var_type) = match initial_expr_val {
            Some(Ok((bve, vvt))) => (Some(bve), Some(vvt)),
            Some(Err(err)) => return Err(err),
            None => (None, None),
        };

        let type_;
        let var_type;
        if let Some(explicit_var_type) = decl_as.var_type {
            type_ = convert_to_type_enum(compiler, &explicit_var_type)?;

            // Type checking
            if let Some(ivt) = inferred_var_type && ivt != explicit_var_type {
                return Err(format!(
                    "Can't assign a value of {} to a variable of type {}",
                    ivt, explicit_var_type
                ));
            }

            var_type = explicit_var_type;
        } else if let Some(inferred_var_type) = inferred_var_type {
            type_ = convert_to_type_enum(compiler, &inferred_var_type)?;
            var_type = inferred_var_type;
        } else {
            return Err("Variables must have an explicit type or an initial value".to_string());
        }

        // Global variable
        if matches!(block_type, BlockType::GLOBAL) {
            let new_global = compiler.module.add_global(type_, None, &id.0);

            if let Some(initial_val) = initial_val {
                // Return error if initial_val comes from an instruction
                // That likely means that it's not constant
                if let Some(_) = initial_val.as_instruction_value() {
                    return Err("Initializer element is not a compile-time constant".to_string());
                }

                new_global.set_initializer(&initial_val);
            } else {
                let default_value = type_.const_zero();
                new_global.set_initializer(&default_value);
            }

            let global_ptr = new_global.as_pointer_value();

            declare_scoped_val(
                compiler,
                id.0,
                ScopedVal::Var(ScopedVar {
                    ptr_val: global_ptr,
                    var_type,
                }),
            )?;

            return Ok(());
        }

        let new_local = compiler.builder.build_alloca(type_, &id.0);

        if let Some(initial_val) = initial_val {
            compiler.builder.build_store(new_local, initial_val);
        }

        // Only applies to local variables
        declare_scoped_val(
            compiler,
            id.0,
            ScopedVal::Var(ScopedVar {
                ptr_val: new_local,
                var_type,
            }),
        )?;
    }

    Ok(())
}

pub fn codegen_assignment(
    compiler: &Compiler,
    assignment: Assignment,
    block_type: BlockType,
) -> Result<(), CompilerError> {
    let (assignee_ptr, expected_type) =
        codegen_lhs_expr(compiler, assignment.assignee_expr.clone(), None, block_type)?;

    let new_val = if let Some(bop) = assignment.bop {
        let bexpr = BExpr {
            lhs: assignment.assignee_expr,
            op: bop,
            rhs: assignment.assigned_expr,
        };

        let (bop_val, bop_type) = codegen_bexpr(
            compiler,
            bexpr,
            ExpectedExprType {
                expected_lhs_type: Some(&expected_type),
                expected_rhs_type: None,
                expected_ret_type: Some(&expected_type),
            },
            block_type,
        )
        .unwrap();

        if expected_type != bop_type {
            return Err(format!(
                "Invalid types, expected {} but got {}",
                expected_type, bop_type
            ));
        }

        bop_val
    } else {
        let (new_val, new_val_type) =
            codegen_rhs_expr(compiler, assignment.assigned_expr, None, block_type).unwrap();

        if expected_type != new_val_type {
            return Err(format!(
                "Invalid types, expected {} but got {}",
                expected_type, new_val_type
            ));
        }

        new_val
    };

    compiler.builder.build_store(assignee_ptr, new_val);
    Ok(())
}

pub fn compile<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
    top_block: TopBlock,
    path: &str,
    file_name: &str,
    target: YepTarget,
    should_compile_extras: bool,
) -> Result<String, CompilerError> {
    Target::initialize_all(&InitializationConfig::default());

    let triple = TargetTriple::create(&target.target_triple);
    let target = Target::from_triple(&triple).map_err(|err| err.to_string())?;
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
    let data_layout = target_machine.get_target_data().get_data_layout();
    compiler.module.set_data_layout(&data_layout);
    _ = compiler.data_layout.insert(data_layout);

    codegen_top_block(compiler, top_block)?;

    let out_path = Path::new(path)
        .join(file_name)
        .to_str()
        .ok_or("Invalid directory")?
        .to_string();
    compiler
        .module
        .print_to_file(&format!("{out_path}.ll"))
        .unwrap();
    if should_compile_extras {
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
    }

    Ok(out_path)
}

pub fn compile_yep(
    input: &'static str,
    path: &str,
    out_name: &str,
    target: YepTarget,
) -> Result<(), CompilerError> {
    let top_block = parse(input, "input.file").ok_or("Invalid code".to_string())?;

    let context = Context::create();
    let module = context.create_module("TODO_file_name");
    let builder = context.create_builder();

    let fpm = PassManager::create(&module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    let mut compiler = Compiler {
        builder: &builder,
        module: &module,
        context: &context,
        fpm: &fpm,
        var_scopes: Vec::new(),
        basic_block_stack: Vec::new(),
        curr_func_ret_val: None,
        func_ret_val_stack: vec![],
        data_layout: None,
        runtime_errors: RuntimeErrors { oob: None },
    };

    compile(&mut compiler, top_block, path, out_name, target, false)?;

    Ok(())
}
