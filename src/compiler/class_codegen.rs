use super::{
    expr_codegen::codegen_rhs_expr,
    helpers::{convert_type_to_metadata, BlockType, Compiler, ScopeMarker, ScopedVal, ScopedVar},
};
use crate::{
    ast::{Destructure, FnDecl, Return, ValueVarType, VarType},
    compiler::{
        helpers::{FnRetVal, ScopedFunc},
        main_codegen::{codegen_block, convert_to_type_enum, declare_variable},
    },
};
use inkwell::{module::Linkage, types::BasicType, values::BasicValue};
use std::collections::VecDeque;

pub fn codegen_return(
    compiler: &Compiler,
    return_: Return,
    block_type: BlockType,
) -> Result<(), String> {
    let ret_expr = return_.0;
    let fn_ret_val = compiler.curr_func_ret_val.clone();

    if ret_expr.is_some() && fn_ret_val.is_none() {
        return Err("Void functions can't return values".to_string());
    }

    // Return is void
    if ret_expr.is_none() {
        // Returning void on a non-void function
        if fn_ret_val.is_some() {
            return Err("Non-void functions must return a value".to_string());
        }

        if block_type.contains(BlockType::FUNC) {
            compiler.builder.build_return(None);
        } else {
            return Err("Return statements must be inside a function".to_string());
        }
    }

    let ret_expr = ret_expr.unwrap();

    let FnRetVal {
        val: ret_val_ptr,
        vtype: expected_ret_type,
        ret_bb,
    } = fn_ret_val.unwrap();

    let (ret_val, ret_type) = codegen_rhs_expr(compiler, ret_expr, Some(&expected_ret_type))
        .map_err(|_| "Invalid value for function return".to_string())?;

    let expected_ret_type: ValueVarType = expected_ret_type;
    if ret_type != expected_ret_type {
        return Err(format!(
            "Invalid type for returned value, expected {}, got {}",
            expected_ret_type, ret_type
        ));
    }

    if block_type.contains(BlockType::FUNC) {
        compiler.builder.build_store(ret_val_ptr, ret_val);
        compiler.builder.build_unconditional_branch(ret_bb);
    } else {
        return Err("Return statements must be inside a function".to_string());
    }

    Ok(())
}

pub fn codegen_fn_decl(
    compiler: &mut Compiler,
    fn_decl: FnDecl,
    block_type: BlockType,
) -> Result<(), String> {
    let fn_name = fn_decl.fn_id.0;
    let param_types = fn_decl
        .args
        .iter()
        .map(|(d, vvt)| {
            if !matches!(d, Destructure::Id(_)) {
                return Err("Destructuring of arguments is not supported yet".to_string());
            }

            Ok(convert_type_to_metadata(convert_to_type_enum(
                compiler, vvt,
            )?))
        })
        .collect::<Result<Vec<_>, String>>()?;

    let ret_type = fn_decl.ret_type.clone().unwrap_or(ValueVarType {
        vtype: VarType::Void,
        array_dimensions: VecDeque::new(),
        pointer_nesting_level: 0,
    });
    let basic_ret_type = convert_to_type_enum(compiler, &ret_type)?;
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
        let basic_type = convert_to_type_enum(compiler, &ret_type)?;

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
        let ty = convert_to_type_enum(compiler, &arg_type)?;
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

    codegen_block(compiler, fn_decl.block, BlockType::FUNC)?;

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
        let ret_val_ty = convert_to_type_enum(compiler, &ret_val.vtype)?;
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

    Ok(())
}
