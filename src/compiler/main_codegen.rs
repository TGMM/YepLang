use super::{
    class_codegen::{codegen_fn_decl, codegen_fn_def, codegen_return},
    control_flow_codegen::{codegen_do_while, codegen_for, codegen_if, codegen_while},
    expr_codegen::{codegen_bexpr, codegen_lhs_expr, codegen_rhs_expr},
    ffi_codegen::codegen_extern_decl,
    helpers::{
        convert_type_to_metadata, BlockType, Compiler, CompilerError, ErrorMessages,
        ExpectedExprType, ScopedVal, ScopedVar, YepTarget,
    },
};
use crate::{
    ast::{Assignment, BExpr, Block, Destructure, Stmt, TopBlock, ValueVarType, VarDecl, VarType},
    parser::main_parser::parse,
};
use chumsky::container::Seq;
use inkwell::{
    context::Context,
    module::Linkage,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum, FunctionType},
    values::{BasicMetadataValueEnum, BasicValue, FunctionValue, PointerValue},
    AddressSpace, OptimizationLevel,
};
use rustc_hash::FxHashMap;
use std::{collections::VecDeque, path::Path};

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
        VarType::String => Ok(ctx
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum()),
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

pub fn initialize_oob_message<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
) -> PointerValue<'ctx> {
    compiler.err_msgs.out_of_bounds.unwrap_or_else(|| {
        compiler
            .builder
            .build_global_string_ptr(
                "Error: Index out of bounds, array of length %d was indexed with %d.",
                "oob_err",
            )
            .as_pointer_value()
    })
}

pub fn codegen_nostd_panic_fn<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
) -> Result<FunctionValue<'ctx>, CompilerError> {
    let panic_fn_ty = compiler.context.void_type().fn_type(&[], false);
    // TODO: Get this name from another place so it's not duplicated
    let panic_fn =
        compiler
            .module
            .add_function("__yep_panic_handler", panic_fn_ty, Some(Linkage::External));

    let entry_fn_bb = compiler.context.append_basic_block(panic_fn, "entry");
    let panic_fn_bb = compiler.context.append_basic_block(panic_fn, "panic");

    // Entry block
    compiler.builder.position_at_end(entry_fn_bb);
    compiler.builder.build_unconditional_branch(panic_fn_bb);

    // Panic block
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

pub fn add_function_to_module<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    fn_name: &str,
    fn_type: FunctionType<'ctx>,
    linkage: Option<Linkage>,
) -> Result<FunctionValue<'ctx>, CompilerError> {
    if fn_name.starts_with("__yep_") {
        return Err(format!(
            "The {} prefix is reserved for internal compiler use and not allowed in global variables or functions.",
            "__yep_"
        ));
    }

    let fun = if let Some(fun) = compiler.module.get_function(&fn_name) {
        let line_one = format!(
            "The function declaration of '{}' does not correspond with a previous existing one.",
            fn_name
        );
        let line_two = "This might be because the extern'd function is a libc defined function.";
        let line_three =
            "The compiler internally links to some libc functions for run-time error handling.";

        if fn_type != fun.get_type() {
            return Err(format!("{}\n{}\n{}", line_one, line_two, line_three));
        }

        fun
    } else {
        let fun = compiler.module.add_function(fn_name, fn_type, linkage);

        fun
    };

    Ok(fun)
}

pub fn link_to_printf<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
) -> Result<FunctionValue<'ctx>, CompilerError> {
    let bmt_i8ptr = compiler
        .context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .as_basic_type_enum();
    let printf_fn_ty = compiler
        .context
        .i32_type()
        .fn_type(&[bmt_i8ptr.into()], true);

    let printf_fn =
        add_function_to_module(compiler, "printf", printf_fn_ty, Some(Linkage::External))?;

    Ok(printf_fn)
}

pub fn codegen_libc_panic_fn<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
) -> Result<FunctionValue<'ctx>, CompilerError> {
    // TODO: This only works on environments with a libc
    let bmt_i32 = convert_type_to_metadata(compiler.context.i32_type().as_basic_type_enum());
    let exit_fn_ty = compiler.context.void_type().fn_type(&[bmt_i32], false);
    let exit_fn = add_function_to_module(compiler, "exit", exit_fn_ty, Some(Linkage::External))?;

    // TODO: This function should only be declared once on another module
    let panic_fn_ty = compiler.context.void_type().fn_type(&[], false);
    // TODO: Get this name from another place so it's not duplicated
    let panic_fn =
        compiler
            .module
            .add_function("__yep_panic_handler", panic_fn_ty, Some(Linkage::External));

    let panic_fn_bb = compiler.context.append_basic_block(panic_fn, "entry");
    compiler.builder.position_at_end(panic_fn_bb);

    let exit_code =
        BasicMetadataValueEnum::IntValue(compiler.context.i32_type().const_int(1, false));
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
    yep_target: YepTarget,
) -> Result<(), CompilerError> {
    let fn_type = compiler.context.i32_type().fn_type(&[], false);
    let fun = compiler
        .module
        .add_function(MAIN_FN_NAME, fn_type, Some(Linkage::External));
    let entry_basic_block = compiler.context.append_basic_block(fun, "entry");
    compiler.basic_block_stack.push(entry_basic_block);

    let panic_fn = if yep_target.nostd {
        codegen_nostd_panic_fn(compiler)
    } else {
        codegen_libc_panic_fn(compiler)
    }?;
    _ = compiler.panic_fn.insert(panic_fn);

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
    if block_type != BlockType::FUNC || block_type.contains(BlockType::FOR) {
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
                if id.id_str.as_str() == "this" {
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
            if id.id_str.starts_with("__yep_") {
                return Err(format!(
                    "The {} prefix is reserved for internal compiler use and not allowed in global variables or functions.",
                    "__yep_"
                ));
            }
            let new_global = compiler.module.add_global(type_, None, &id.id_str);

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
                id.id_str,
                ScopedVal::Var(ScopedVar {
                    ptr_val: global_ptr,
                    var_type,
                }),
            )?;

            return Ok(());
        }

        let new_local = compiler.builder.build_alloca(type_, &id.id_str);

        if let Some(initial_val) = initial_val {
            compiler.builder.build_store(new_local, initial_val);
        }

        // Only applies to local variables
        declare_scoped_val(
            compiler,
            id.id_str,
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
                expected_rhs_type: Some(&expected_type),
                expected_ret_type: Some(&expected_type),
            },
            block_type,
        )?;

        if expected_type != bop_type {
            return Err(format!(
                "Invalid types, expected {} but got {}",
                expected_type, bop_type
            ));
        }

        bop_val
    } else {
        let core_type = ValueVarType {
            vtype: expected_type.vtype.clone(),
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        };

        let (new_val, new_val_type) = codegen_rhs_expr(
            compiler,
            assignment.assigned_expr,
            Some(&core_type),
            block_type,
        )
        .unwrap();

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
    yep_target: YepTarget,
    should_compile_extras: bool,
) -> Result<String, CompilerError> {
    Target::initialize_all(&InitializationConfig::default());

    let triple = TargetTriple::create(&yep_target.target_triple);
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

    codegen_top_block(compiler, top_block, yep_target)?;

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
        panic_fn: None,
        err_msgs: ErrorMessages {
            out_of_bounds: None,
        },
    };

    compile(&mut compiler, top_block, path, out_name, target, false)?;

    Ok(())
}
