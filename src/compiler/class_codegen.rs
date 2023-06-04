use super::{
    expr_codegen::codegen_rhs_expr,
    helpers::{convert_type_to_metadata, BlockType, Compiler, CompilerError, ScopedVal, ScopedVar},
    main_codegen::add_function_to_module,
};
use crate::{
    ast::{
        Block, Destructure, ExternType, FnDef, FnScope, FnSignature, FnType, InlineLlvmIr, Return,
        Stmt, ValueVarType, VarType,
    },
    compiler::{
        helpers::{FnRetVal, ScopedFunc},
        main_codegen::{codegen_block, convert_to_type_enum, declare_scoped_val},
    },
    spanned_ast::GetSpan,
};
use inkwell::{
    module::{Linkage, Module},
    support::to_c_str,
    types::{BasicMetadataTypeEnum, BasicType},
    values::{BasicValue, FunctionValue, InstructionOpcode},
};
use llvm_sys::assembly::LLVMParseAssemblyString;
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

pub fn codegen_return(
    compiler: &Compiler,
    return_: Return,
    block_type: BlockType,
) -> Result<(), CompilerError> {
    let ret_span = return_.get_span();
    let ret_expr = return_.ret_val;
    let fn_ret_val = compiler.curr_func_ret_val.clone();

    if ret_expr.is_some() && fn_ret_val.is_none() {
        return Err(CompilerError {
            reason: "Void functions can't return values".to_string(),
            span: Some(ret_span),
        });
    }

    // Return is void
    if ret_expr.is_none() {
        // Returning void on a non-void function
        if fn_ret_val.is_some() {
            return Err(CompilerError {
                reason: "Non-void functions must return a value".to_string(),
                span: Some(ret_span),
            });
        }

        if block_type.contains(BlockType::FUNC) {
            compiler.builder.build_return(None);
        } else {
            return Err(CompilerError {
                reason: "Return statements must be inside a function".to_string(),
                span: Some(ret_span),
            });
        }
    }

    let ret_expr = ret_expr.unwrap();

    let FnRetVal {
        val: ret_val_ptr,
        vtype: expected_ret_type,
        ret_bb,
    } = fn_ret_val.unwrap();

    let (ret_val, ret_type) =
        codegen_rhs_expr(compiler, ret_expr, Some(&expected_ret_type), block_type).map_err(
            |err| CompilerError {
                reason: format!("Invalid value for function return: {}", err.reason),
                span: Some(ret_span),
            },
        )?;

    let expected_ret_type: ValueVarType = expected_ret_type;
    if ret_type != expected_ret_type {
        return Err(CompilerError {
            reason: format!(
                "Invalid type for returned value, expected {}, got {}",
                expected_ret_type, ret_type
            ),
            span: Some(ret_span),
        });
    }

    if block_type.contains(BlockType::FUNC) {
        compiler.builder.build_store(ret_val_ptr, ret_val);
        compiler.builder.build_unconditional_branch(ret_bb);
    } else {
        return Err(CompilerError {
            reason: "Return statements must be inside a function".to_string(),
            span: Some(ret_span),
        });
    }

    Ok(())
}

pub fn codegen_fn_decl<'input, 'ctx>(
    compiler: &mut Compiler<'input, 'ctx>,
    fn_signature: &FnSignature,
    block_type: BlockType,
) -> Result<FunctionValue<'ctx>, CompilerError> {
    let fn_id_span = fn_signature.fn_id.get_span();
    let fn_name = fn_signature.fn_id.id_str.as_str();

    let arg_types = fn_signature
        .args
        .iter()
        .map(|(d, vvt)| {
            if !matches!(d, Destructure::Id(_)) {
                return Err(CompilerError {
                    reason: "Destructuring of arguments is not supported yet".to_string(),
                    span: Some(d.get_span()),
                });
            }

            Ok(vvt)
        })
        .collect::<Result<Vec<_>, CompilerError>>()?;
    let param_types = arg_types
        .iter()
        .map(|vvt| {
            Ok(convert_type_to_metadata(convert_to_type_enum(
                compiler, &vvt.node,
            )?))
        })
        .collect::<Result<Vec<BasicMetadataTypeEnum>, CompilerError>>()?;

    let ret_type = fn_signature
        .ret_type
        .clone()
        .map(|svvt| svvt.node)
        .unwrap_or(ValueVarType {
            vtype: VarType::Void,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        });

    let fn_type = if ret_type.vtype == VarType::Void {
        compiler.context.void_type().fn_type(&param_types, false)
    } else {
        convert_to_type_enum(compiler, &ret_type)?.fn_type(&param_types, false)
    };
    let linkage = if matches!(block_type, BlockType::GLOBAL) {
        Linkage::External
    } else {
        Linkage::Internal
    };
    let fun = add_function_to_module(compiler, &fn_name, fn_type, Some(linkage))?;

    let arg_types = arg_types
        .into_iter()
        .map(|vvt| ExternType::Type(vvt.clone()))
        .collect::<Vec<_>>();

    declare_scoped_val(
        compiler,
        fn_name.to_string(),
        ScopedVal::Fn(ScopedFunc {
            ptr_val: fun,
            arg_types,
            ret_type,
        }),
        Some(fn_id_span),
    )?;

    Ok(fun)
}

pub fn codegen_fn_def(
    compiler: &mut Compiler,
    fn_def: FnDef,
    block_type: BlockType,
) -> Result<(), CompilerError> {
    let fn_signature = fn_def.fn_signature;
    match (fn_def.fn_scope, fn_def.fn_type) {
        (FnScope::Function, FnType::Native(fn_block)) => {
            codegen_native_fn(compiler, fn_signature, fn_block, block_type)
        }
        (FnScope::Function, FnType::InlineLlvmIr(llvm_ir)) => {
            codegen_llvm_fn(compiler, fn_signature, llvm_ir, block_type)
        }
        (FnScope::Method, FnType::Native(_)) => todo!(),
        (FnScope::Method, FnType::InlineLlvmIr(_)) => todo!(),
    }
}

pub fn codegen_llvm_fn(
    compiler: &mut Compiler,
    fn_signature: FnSignature,
    llvm_ir_block: InlineLlvmIr,
    block_type: BlockType,
) -> Result<(), CompilerError> {
    let FnSignature {
        fn_id,
        args,
        ret_type,
    } = fn_signature;

    let InlineLlvmIr {
        lbracket: _,
        ir: llvm_ir,
        rbracket: _,
    } = llvm_ir_block;

    let id_args = args
        .clone()
        .into_iter()
        .map(|(arg, arg_type)| match arg {
            Destructure::Id(id) => Ok((id, arg_type)),
            _ => {
                return Err(CompilerError {
                    reason: "Destructuring is not supported inside LLVM IR".to_string(),
                    span: Some(arg.get_span()),
                });
            }
        })
        .collect::<Result<Vec<_>, CompilerError>>()?;

    let args_str = id_args
        .into_iter()
        .map(|(id, vvt)| {
            let ty = convert_to_type_enum(compiler, &vvt.node)?.to_string();
            let trimmed_ty = ty.trim_matches('"');
            Ok(format!("{} %{}", trimmed_ty, id.id_str))
        })
        .collect::<Result<Vec<_>, CompilerError>>()?
        .join(", ");

    let ret_type_str = ret_type
        .as_ref()
        .map(|vvt| vvt.to_string())
        .unwrap_or("void".to_string());

    let fn_id_span = fn_id.get_span();
    let fn_name = fn_id.id_str;
    let fun_def = format!(
        "define {} @{}({}) {{{}}}",
        ret_type_str, fn_name, args_str, llvm_ir
    );

    let ir_str = to_c_str(&fun_def);
    let data_layout = compiler.data_layout.as_ref().ok_or(CompilerError {
        reason: "Can't compile inline LLVM IR without a data layout".to_string(),
        span: None,
    })?;
    let data_layout_str = to_c_str(data_layout.as_str().to_str().unwrap());
    let m = unsafe {
        LLVMParseAssemblyString(
            ir_str.as_ptr(),
            ir_str.to_bytes().len(),
            data_layout_str.as_ptr(),
            data_layout_str.to_bytes().len(),
            compiler.context.as_mut_ptr(),
        )
    };

    // Link newly created inline module
    let inline_module = unsafe { Module::new(m) };
    compiler
        .module
        .link_in_module(inline_module)
        .map_err(|err| CompilerError {
            reason: format!("Error linking inline LLVM IR: {}", err),
            span: None,
        })?;

    let arg_vvtypes = args
        .iter()
        .map(|(d, vvt)| {
            if !matches!(d, Destructure::Id(_)) {
                return Err(CompilerError {
                    reason: "Destructuring of arguments is not supported yet".to_string(),
                    span: Some(d.get_span()),
                });
            }

            Ok(vvt)
        })
        .collect::<Result<Vec<_>, CompilerError>>()?;
    let arg_extypes = arg_vvtypes
        .into_iter()
        .map(|vvt| ExternType::Type(vvt.clone()))
        .collect::<Vec<_>>();

    let ret_type = ret_type.map(|svvt| svvt.node).unwrap_or(ValueVarType {
        vtype: VarType::Void,
        array_dimensions: VecDeque::new(),
        pointer_nesting_level: 0,
    });

    let fn_ptr = compiler
        .module
        .get_function(&fn_name)
        .expect("Could not find LLVM inlined function");
    declare_scoped_val(
        compiler,
        fn_name,
        ScopedVal::Fn(ScopedFunc {
            ptr_val: fn_ptr,
            arg_types: arg_extypes,
            ret_type,
        }),
        Some(fn_id_span),
    )?;

    Ok(())
}

pub fn codegen_native_fn(
    compiler: &mut Compiler,
    fn_signature: FnSignature,
    fn_block: Block,
    block_type: BlockType,
) -> Result<(), CompilerError> {
    let fn_name = fn_signature.fn_id.id_str;
    let fun = compiler
        .get_scoped_val(&fn_name, block_type, None)?
        .as_fn()
        .ok_or(CompilerError {
            reason: "ICE: Couldn't get forward-declared function".to_string(),
            span: None,
        })?
        .ptr_val;

    assert_eq!(fun.count_params() as usize, fn_signature.args.len());

    // Return check
    if let Some(ref ret_type) = fn_signature.ret_type && !ret_type.node.is_void() {
        let has_return = fn_block
            .stmts
            .iter()
            .find(|stmt| matches!(stmt, Stmt::Return(_)))
            .is_some();

        if !has_return {
            let err = "Non-void functions must have at least 1 top-level (as in, not nested) return statement.";
            return Err(CompilerError { reason: err.to_string(), span: Some(fn_block.get_span()) });
        }
    }

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
    if let Some(ret_type) = fn_signature.ret_type {
        let ret_type = ret_type.node;
        let basic_type = convert_to_type_enum(compiler, &ret_type)?;

        compiler.curr_func_ret_val = Some(FnRetVal {
            val: compiler.builder.build_alloca(basic_type, "fn_ret_val"),
            vtype: ret_type,
            ret_bb: ret_basic_block,
        });
    };

    // Add function block
    compiler.var_scopes.push(FxHashMap::default());

    // Codegen arguments
    for ((arg_destructure, arg_type), arg_val) in
        fn_signature.args.into_iter().zip(fun.get_param_iter())
    {
        let arg_type = arg_type.node;
        let ty = convert_to_type_enum(compiler, &arg_type)?;
        match arg_destructure {
            Destructure::Id(id) => {
                let id_span = id.get_span();
                arg_val.set_name(id.id_str.as_str());
                let arg_ptr = compiler
                    .builder
                    .build_alloca(ty, &format!("local_{}", id.id_str.as_str()));
                compiler.builder.build_store(arg_ptr, arg_val);
                declare_scoped_val(
                    compiler,
                    id.id_str,
                    ScopedVal::Var(ScopedVar {
                        ptr_val: arg_ptr,
                        var_type: arg_type,
                    }),
                    Some(id_span),
                )?;

                arg_ptr
            }
            _ => {
                return Err(CompilerError {
                    reason: format!("Destructures in arguments are not supported yet."),
                    span: None,
                })
            }
        };
    }

    codegen_block(compiler, fn_block, block_type.union(BlockType::FUNC))?;

    // We need a reference to the previous block, we get that here
    // This could be either the function block or the if_cont block
    let prev_bb = compiler.builder.get_insert_block().unwrap();

    // Building the return
    // Every block must end with a br statement, so we jump to the return block
    // If the previous instruction is an unconditional branch, then we don't build another
    if let Some(instr) = prev_bb.get_last_instruction()
        && instr.get_opcode() == InstructionOpcode::Br
    {} else {
        compiler.builder.build_unconditional_branch(ret_basic_block);
    }

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
