use super::{
    class_codegen::{codegen_fn_decl, codegen_return},
    control_flow_codegen::{codegen_do_while, codegen_for, codegen_if, codegen_while},
    expr_codegen::{codegen_lhs_expr, codegen_rhs_expr},
    ffi_codegen::codegen_extern_decl,
    helpers::{BlockType, Compiler, ScopeMarker, ScopedVal, ScopedVar},
};
use crate::{
    ast::{Assignment, Block, Destructure, Stmt, TopBlock, ValueVarType, VarDecl, VarType},
    parser::main_parser::parse,
};
use inkwell::{
    context::Context,
    module::Linkage,
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum},
    AddressSpace, OptimizationLevel,
};
use std::{cell::OnceCell, collections::HashMap, path::Path};

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

pub fn codegen_block(compiler: &mut Compiler, block: Block, mut block_type: BlockType) {
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

pub fn codegen_stmt(compiler: &mut Compiler, stmt: Stmt, block_type: BlockType) {
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

pub fn codegen_assignment(compiler: &Compiler, assignment: Assignment) {
    // TODO: The BOP doesn't work
    if assignment.bop.is_some() {
        panic!("Binary operators on assignments aren't supported yet");
    }

    let assignee_ptr = codegen_lhs_expr(compiler, assignment.assignee_expr, None).0;
    let new_val = codegen_rhs_expr(compiler, assignment.assigned_expr, None).unwrap();

    compiler.builder.build_store(assignee_ptr, new_val.0);
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

pub fn compile_yep(input: &'static str, path: &'static str, out_name: &'static str) {
    let top_block = parse(input, "input.file").expect("Invalid code");

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
        curr_scope_vars: HashMap::new(),
        basic_block_stack: Vec::new(),
        scope_stack: Vec::new(),
        curr_func_ret_val: None,
        func_ret_val_stack: vec![],
        target_data: OnceCell::new(),
    };

    compile_to_x86(&mut compiler, top_block, path, out_name);
}
