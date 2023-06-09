use super::{
    helpers::{
        convert_value_to_metadata, create_default_type, semantic_cube, BlockType, Compiler,
        CompilerError, ExpectedExprType, ScopedVal,
    },
    main_codegen::{convert_to_type_enum, initialize_oob_message, link_to_printf},
    primitive_codegen::codegen_primitive_val,
};
use crate::{
    ast::{BExpr, BOp, Casting, Expr, ExternType, FnCall, Indexing, ValueVarType, VarType},
    spanned_ast::GetSpan,
};
use inkwell::{
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, InstructionOpcode, IntValue,
        PointerValue,
    },
    FloatPredicate, IntPredicate,
};
use std::collections::VecDeque;

pub fn codegen_fn_call<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    fn_call: FnCall,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let fn_call_span = fn_call.get_span();
    let fn_id = match fn_call.fn_expr {
        Expr::Id(id) => id,
        _ => {
            return Err(CompilerError {
                reason: "Functions as values are not yet supported".to_string(),
                span: Some(fn_call_span),
            })
        }
    };
    let fn_id_span = fn_id.get_span();
    let fn_name = fn_id.id_str;
    let function = compiler
        .get_scoped_val(&fn_name, block_type, Some(fn_id_span))?
        .clone()
        .into_fn()
        .map_err(|_| CompilerError {
            reason: format!("Attempting to call {}, which is not a function", fn_name),
            span: Some(fn_id_span),
        })?;
    let function_ptr = function.ptr_val;
    let func_ret_ty = function.ret_type;

    let mut arg_types_iter: Box<dyn Iterator<Item = &ExternType>> =
        Box::new(function.arg_types.iter());
    if function.arg_types.last() == Some(&ExternType::Spread) {
        arg_types_iter =
            Box::new(arg_types_iter.chain(std::iter::once(&ExternType::Spread).cycle()));
    }

    let mut args = vec![];
    for (arg_expr, arg_expected_type) in fn_call.args.into_iter().zip(arg_types_iter) {
        let arg_expr_span = arg_expr.get_span();
        let (mut arg_val, arg_type) = codegen_rhs_expr(compiler, arg_expr, None, block_type)?;

        // Variadic argument promotions
        if ExternType::Spread == *arg_expected_type {
            if !arg_type.array_dimensions.is_empty() {
                let err = "Arrays should not be passed to functions with variadic arguments, cast it to a pointer instead";
                return Err(CompilerError {
                    reason: err.to_string(),
                    span: Some(arg_expr_span),
                });
            }

            match arg_val {
                BasicValueEnum::IntValue(int_arg_val) => 'promotion: {
                    let default_type = VarType::I32;
                    // We only promote values smaller than i32
                    if arg_type.vtype >= default_type {
                        break 'promotion;
                    }

                    let promotion_type = compiler.context.i32_type();

                    arg_val = compiler
                        .builder
                        .build_int_cast(int_arg_val, promotion_type, "int_promotion")
                        .as_basic_value_enum();
                }
                BasicValueEnum::FloatValue(float_arg_val) => {
                    let promotion_type = compiler.context.f64_type();
                    if float_arg_val.get_type() != promotion_type {
                        arg_val = compiler
                            .builder
                            .build_float_cast(float_arg_val, promotion_type, "float_promotion")
                            .as_basic_value_enum();
                    }
                }
                _ => {}
            }
        }

        if let ExternType::Type(expected_vvt) = arg_expected_type {
            if expected_vvt.node != arg_type {
                return Err(CompilerError {
                    reason: format!(
                        "Incorrect type for argument, expected {} got {}",
                        expected_vvt, arg_type
                    ),
                    span: Some(arg_expr_span),
                });
            }
        }

        let arg_metadata: BasicMetadataValueEnum = convert_value_to_metadata(arg_val);
        args.push(arg_metadata);
    }

    let instruction_name = format!("call_{}", fn_name);
    let call = compiler
        .builder
        .build_call(function_ptr, &args, &instruction_name);
    let ret_val = call.try_as_basic_value();

    // This default value is just there to ensure we
    // can call void functions.
    // It's never actually codegen'd
    let default_value = compiler
        .context
        .i8_type()
        .const_zero()
        .as_basic_value_enum();
    Ok((ret_val.left().unwrap_or(default_value), func_ret_ty))
}

/// Codegen an expression when it's used on the left-hand side of an operation.
pub fn codegen_lhs_expr<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    expr: Expr,
    expected_type: Option<&ValueVarType>,
    block_type: BlockType,
) -> Result<(PointerValue<'ctx>, ValueVarType), CompilerError> {
    match expr {
        Expr::ParenExpr(None, expr) => codegen_lhs_expr(compiler, *expr, expected_type, block_type),
        Expr::Indexing(indexing) => {
            codegen_lhs_indexing(compiler, *indexing, expected_type, block_type)
        }
        Expr::MemberAccess(_) => todo!(),
        Expr::Id(var_id) => {
            let var_ptr =
                compiler.get_scoped_val(&var_id.id_str, block_type, Some(var_id.get_span()))?;

            match var_ptr {
                ScopedVal::Var(v) => Ok((v.ptr_val, v.var_type.clone())),
                ScopedVal::Fn(_) => todo!(),
            }
        }
        _ => Err(CompilerError {
            reason: "A left-hand side expression can't be ...".to_string(),
            span: Some(expr.get_span()),
        }),
    }
}

/// Codegen an expression when it's used on the right-hand side of an operation.
/// * `expected_type` - The expected result type of the expression.
/// Only useful if we're coming from an assignment or the var decl has an explicit type,
/// otherwise inferred to defaults
pub fn codegen_rhs_expr<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    expr: Expr,
    expected_type: Option<&ValueVarType>,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    match expr {
        // TODO: Handle expr unary operator
        Expr::ParenExpr(_, expr) => codegen_rhs_expr(compiler, *expr, expected_type, block_type),
        Expr::BinaryExpr(bexpr) => codegen_bexpr(
            compiler,
            *bexpr,
            ExpectedExprType {
                expected_lhs_type: expected_type,
                expected_rhs_type: expected_type,
                expected_ret_type: expected_type,
            },
            block_type,
        ),
        Expr::PrimitiveVal(primitive_val) => {
            codegen_primitive_val(compiler, primitive_val.node, expected_type, block_type)
        }
        Expr::FnCall(fn_call) => codegen_fn_call(compiler, *fn_call, block_type),
        Expr::Indexing(indexing) => {
            codegen_rhs_indexing(compiler, *indexing, expected_type, block_type)
        }
        Expr::MemberAccess(_) => todo!(),
        Expr::Id(var_id) => {
            let var_name = var_id.id_str.as_str();
            let scoped_val =
                compiler.get_scoped_val(var_name, block_type, Some(var_id.get_span()))?;
            let scoped_var = scoped_val
                .clone()
                .into_var()
                .map_err(|_| "Using function pointer as a value is not supported".to_string())?;

            let instruction_name = format!("load_{}", var_id.id_str);

            let var_type = convert_to_type_enum(compiler, &scoped_var.var_type)?;
            let var_val =
                compiler
                    .builder
                    .build_load(var_type, scoped_var.ptr_val, &instruction_name);

            Ok((var_val, scoped_var.var_type))
        }
        Expr::Cast(casting) => codegen_rhs_casting(compiler, *casting, block_type),
        Expr::Referencing(referencing_expr) => {
            codegen_referencing(compiler, *referencing_expr, block_type)
        }
        Expr::Dereferencing(dereferencing_expr) => {
            codegen_dereferencing(compiler, *dereferencing_expr, block_type)
        }
    }
}

pub fn codegen_referencing<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    referencing_expr: Expr<'input>,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let expr_span = referencing_expr.get_span();
    match referencing_expr {
        Expr::Id(id) => {
            let var_name = id.id_str;
            let id_span = id.span;
            let scoped_val =
                compiler.get_scoped_val(var_name.as_str(), block_type, Some(id_span))?;

            match scoped_val {
                ScopedVal::Var(var) => {
                    let mut ref_ty = var.var_type.clone();
                    ref_ty.pointer_nesting_level += 1;
                    return Ok((var.ptr_val.as_basic_value_enum(), ref_ty));
                }
                ScopedVal::Fn(_) => Err(CompilerError {
                    reason: "Can't reference a function".to_string(),
                    span: Some(expr_span),
                }),
            }
        }
        _ => Err(CompilerError {
            reason: "Referencing is only supported for variables".to_string(),
            span: Some(expr_span),
        }),
    }
}

pub fn codegen_dereferencing<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    dereferencing_expr: Expr<'input>,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let expr_span = dereferencing_expr.get_span();
    match dereferencing_expr {
        Expr::Id(id) => {
            let var_name = id.id_str;
            let id_span = id.span;
            let scoped_val =
                compiler.get_scoped_val(var_name.as_str(), block_type, Some(id_span))?;

            match scoped_val {
                ScopedVal::Var(var) => {
                    if var.var_type.pointer_nesting_level == 0 {
                        return Err(CompilerError {
                            reason: "Only pointers can be de-referenced".to_string(),
                            span: Some(expr_span),
                        });
                    }

                    let mut deref_ty = var.var_type.clone();
                    deref_ty.pointer_nesting_level -= 1;

                    let deref_bt = convert_to_type_enum(compiler, &deref_ty)?;
                    let const_zero = compiler.context.i32_type().const_zero();
                    let var_gep = unsafe {
                        compiler.builder.build_in_bounds_gep(
                            deref_bt,
                            var.ptr_val,
                            &[const_zero],
                            "dereference_gep",
                        )
                    };
                    let var_val =
                        compiler
                            .builder
                            .build_load(deref_bt, var_gep, "dereference_load");
                    return Ok((var_val.as_basic_value_enum(), deref_ty));
                }
                ScopedVal::Fn(_) => Err(CompilerError {
                    reason: "Can't reference a function".to_string(),
                    span: Some(expr_span),
                }),
            }
        }
        _ => Err(CompilerError {
            reason: "De-referencing is only supported for variables".to_string(),
            span: Some(expr_span),
        }),
    }
}

pub fn codegen_rhs_casting<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    casting: Casting,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let casting_span = casting.get_span();
    let Casting {
        casted,
        as_kw: _,
        cast_type: cast_to_type,
    } = casting;
    let (cast_from_val, cast_from_type) =
        codegen_rhs_expr(compiler, casted.clone(), None, block_type)?;

    let cast_to_type = cast_to_type.node;

    // Non matching dimensions
    if cast_to_type.array_dimensions != cast_from_type.array_dimensions
    // Allow casting from array to ptr
    // Note that the actual cast logic is done below since we need the 
    // cast_to_val variable set
        && cast_to_type.pointer_nesting_level == 0
    {
        return Err(CompilerError {
            reason: "Cast mismatch: Array dimensions do not match".to_string(),
            span: Some(casting_span),
        });
    } else if !cast_to_type.array_dimensions.is_empty()
        && !cast_from_type.array_dimensions.is_empty()
    {
        return Err(CompilerError {
            reason: "Casting arrays is not yet supported".to_string(),
            span: Some(casting_span),
        });
    }

    if cast_to_type.vtype == VarType::String {
        return Err(CompilerError {
            reason: "Casting to string is not allowed".to_string(),
            span: Some(casting_span),
        });
    }

    let mut cast_to_val = cast_from_val;
    let cast_to_b_type = convert_to_type_enum(compiler, &cast_to_type)?;

    // Allow string to ptr
    if cast_from_type.vtype == VarType::String && cast_to_type.pointer_nesting_level > 0 {
        if cast_to_type.pointer_nesting_level > 1 || cast_to_type.vtype != VarType::I8 {
            println!(
                "WARNING: String can only be safely cast to an *i8, casting to a {}",
                cast_to_type
            );

            cast_to_val =
                compiler
                    .builder
                    .build_bitcast(cast_from_val, cast_to_b_type, "str_to_ptr_bitcast");
        }

        return Ok((cast_to_val, cast_to_type));
    }

    let cast_from_b_type = cast_from_val.get_type();

    // If types are the same don't do anything
    if cast_to_b_type == cast_from_b_type
    // Allow casting from ptr to ptr, we also don't do anything since
    // ptr doesn't have a type in LLVM16
        || cast_to_type.pointer_nesting_level > 0 && cast_from_type.pointer_nesting_level > 0
    {
        return Ok((cast_from_val, cast_to_type));
    }

    // Allow casting from arr to ptr
    if cast_to_b_type.is_pointer_type() && cast_from_b_type.is_array_type() {
        // Casting from array requires the pointer instead of the value
        let (cast_from_val, _) =
            codegen_lhs_expr(compiler, casted, None, block_type).map_err(|_| {
                "Casting from a const array is not supported. Please store it in a variable first"
                    .to_string()
            })?;

        cast_to_val =
            compiler
                .builder
                .build_bitcast(cast_from_val, cast_to_b_type, "arr_to_ptr_bitcast");

        return Ok((cast_to_val, cast_to_type));
    }

    // None of the above was true, thus it's an invalid cast
    // if one of the types is a pointer or an array
    if cast_from_type.pointer_nesting_level > 0 || cast_to_type.pointer_nesting_level > 0 {
        return Err(CompilerError {
            reason: format!("Invalid cast from {} to {}", cast_from_type, cast_to_type),
            span: Some(casting_span),
        });
    }

    let cast_to_vtype = &cast_to_type.vtype;
    let cast_from_vtype = &cast_from_type.vtype;

    // Both ints
    if cast_from_vtype.is_int() && cast_to_vtype.is_int() {
        let cast_from_int_val = cast_from_val.into_int_value();
        let cast_to_int_type = cast_to_b_type.into_int_type();

        // Same number of bits just means the sign is different
        // But that gets handled by LLVM with our VType
        if cast_from_vtype.get_int_val() != cast_to_vtype.get_int_val() {
            cast_to_val = compiler
                .builder
                .build_int_cast(cast_from_int_val, cast_to_int_type, "int_cast")
                .as_basic_value_enum();
        }

        return Ok((cast_to_val, cast_to_type));
    }

    // Both floats
    if cast_from_vtype.is_float() && cast_to_vtype.is_float() {
        let cast_from_float_val = cast_from_val.into_float_value();
        let cast_to_float_type = cast_to_b_type.into_float_type();

        // Same as above
        if cast_from_vtype.get_float_val() != cast_to_vtype.get_float_val() {
            cast_to_val = compiler
                .builder
                .build_float_cast(cast_from_float_val, cast_to_float_type, "float_cast")
                .as_basic_value_enum();
        }

        return Ok((cast_to_val, cast_to_type));
    }

    // Float to int
    if cast_from_vtype.is_float() && cast_to_vtype.is_int() {
        let cast_from_float_val = cast_from_val.into_float_value();
        let cast_to_int_type = cast_to_b_type.into_int_type();

        let op = if cast_to_type.vtype.is_signed() {
            InstructionOpcode::FPToSI
        } else {
            InstructionOpcode::FPToUI
        };

        cast_to_val = compiler
            .builder
            .build_cast(
                op,
                cast_from_float_val,
                cast_to_int_type,
                "float_to_int_cast",
            )
            .as_basic_value_enum();

        return Ok((cast_to_val, cast_to_type));
    }

    // Int to float
    if cast_from_vtype.is_int() && cast_to_vtype.is_float() {
        let cast_from_int_val = cast_from_val.into_int_value();
        let cast_to_float_type = cast_to_b_type.into_float_type();

        let op = if cast_from_type.vtype.is_signed() {
            InstructionOpcode::SIToFP
        } else {
            InstructionOpcode::UIToFP
        };

        cast_to_val = compiler
            .builder
            .build_cast(
                op,
                cast_from_int_val,
                cast_to_float_type,
                "int_to_float_cast",
            )
            .as_basic_value_enum();

        return Ok((cast_to_val, cast_to_type));
    }

    // TODO: Struct types
    // TODO: Ptr to arr?

    Err(CompilerError {
        reason: format!("Invalid cast from {} to {}", cast_from_type, cast_to_type),
        span: Some(casting_span),
    })
}

pub fn codegen_bounds_check(
    compiler: &Compiler,
    idxr: IntValue,
    idxd_type: &ValueVarType,
) -> Result<(), CompilerError> {
    let arr_dim = *idxd_type
        .array_dimensions
        .front()
        .ok_or("Indexing a non-array value")?;
    let arr_dim_val = compiler.context.i32_type().const_int(arr_dim.into(), false);

    // Current block
    let curr_bb = compiler.builder.get_insert_block().unwrap();
    // Current fn
    let curr_fn = curr_bb.get_parent().unwrap();

    let success_bb = compiler
        .context
        .append_basic_block(curr_fn, "bounds_check_success");

    let oob_bb = compiler
        .context
        .append_basic_block(curr_fn, "bounds_check_fail");

    let cmp = compiler.builder.build_int_compare(
        IntPredicate::ULT,
        idxr,
        arr_dim_val,
        "bounds_check_cmp",
    );
    compiler
        .builder
        .build_conditional_branch(cmp, success_bb, oob_bb);

    // Failure block
    compiler.builder.position_at_end(oob_bb);
    let panic_fn = compiler
        .panic_fn
        .ok_or("ICE: Invalid panic function".to_string())?;

    let error_msg = initialize_oob_message(compiler);
    let printf = link_to_printf(compiler)?;
    compiler.builder.build_call(
        printf,
        &[error_msg.into(), idxr.into(), arr_dim_val.into()],
        "oob_printf_call",
    );

    compiler.builder.build_call(panic_fn, &[], "oob_panic_call");
    compiler.builder.build_unreachable();

    // Success block
    compiler.builder.position_at_end(success_bb);

    Ok(())
}

pub fn codegen_lhs_indexing<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    indexing: Indexing,
    expected_type: Option<&ValueVarType>,
    block_type: BlockType,
) -> Result<(PointerValue<'ctx>, ValueVarType), CompilerError> {
    let indexer_span = indexing.indexer.get_span();
    let (idxd, idxd_type) = codegen_lhs_expr(compiler, indexing.indexed, None, block_type)?;

    if let Some(exp_ty) = expected_type {
        if !exp_ty.array_dimensions.is_empty() && exp_ty.vtype != idxd_type.vtype {
            return Err(CompilerError {
                reason: format!(
                    "Assigning array of type {} to array of type {}",
                    idxd_type, exp_ty
                ),
                span: None,
            });
        }
    }

    // Array dimensions are u32
    let u32_type = &create_default_type(VarType::U32);
    let (idxr, idxr_type) =
        codegen_rhs_expr(compiler, indexing.indexer, Some(u32_type), block_type)?;

    if idxr_type != *u32_type {
        let line_one = "Indexing is only supported for arrays, array indexes must be of type u32.";
        let line_two = "If index type is numeric, try casting (as u32)";

        return Err(CompilerError {
            reason: format!("{} {}", line_one, line_two),
            span: Some(indexer_span),
        });
    }

    let idxr: inkwell::values::IntValue = idxr.into_int_value();

    let element_type = {
        let mut et = idxd_type.clone();
        et.array_dimensions.pop_front();

        et
    };

    codegen_bounds_check(compiler, idxr, &idxd_type)?;

    let zero_ptr = compiler.context.i32_type().const_zero();
    let arr_ty = convert_to_type_enum(compiler, &idxd_type)?;
    let ret_val_ptr = unsafe {
        compiler
            .builder
            .build_in_bounds_gep(arr_ty, idxd, &[zero_ptr, idxr], "indexing")
    };

    Ok((ret_val_ptr, element_type))
}

pub fn codegen_rhs_indexing<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    indexing: Indexing,
    expected_type: Option<&ValueVarType>,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let indexer_span: chumsky::span::SimpleSpan = indexing.indexer.get_span();
    let (idxd, idxd_type) = codegen_lhs_expr(compiler, indexing.indexed, None, block_type)?;

    if let Some(exp_ty) = expected_type {
        if !exp_ty.array_dimensions.is_empty() && exp_ty.vtype != idxd_type.vtype {
            return Err(CompilerError {
                reason: format!(
                    "Assigning array of type {} to array of type {}",
                    idxd_type, exp_ty
                ),
                span: None,
            });
        }
    }

    // Array dimensions are u32
    let u32_type = &create_default_type(VarType::U32);
    let (idxr, idxr_type) =
        codegen_rhs_expr(compiler, indexing.indexer, Some(u32_type), block_type)?;

    if idxr_type != *u32_type {
        let line_one = "Indexing is only supported for arrays, array indexes must be of type u32.";
        let line_two = "If index type is numeric, try casting (as u32)";

        return Err(CompilerError {
            reason: format!("{} {}", line_one, line_two),
            span: Some(indexer_span),
        });
    }

    let idxr: IntValue = idxr.into_int_value();

    let element_type = {
        let mut et = idxd_type.clone();
        et.array_dimensions.pop_front();

        et
    };
    let element_b_type = convert_to_type_enum(compiler, &element_type)?;

    codegen_bounds_check(compiler, idxr, &idxd_type)?;

    // Success block instructions
    let zero_ptr = compiler.context.i32_type().const_zero();
    let arr_ty = convert_to_type_enum(compiler, &idxd_type)?;

    let ret_val_ptr = unsafe {
        compiler
            .builder
            .build_in_bounds_gep(arr_ty, idxd, &[zero_ptr, idxr], "indexing")
    };
    let ret_val = compiler
        .builder
        .build_load(element_b_type, ret_val_ptr, "load_array_val");

    Ok((ret_val, element_type))
}

pub fn codegen_bexpr<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    bexpr: BExpr,
    expected_type: ExpectedExprType,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let ExpectedExprType {
        expected_lhs_type,
        expected_rhs_type,
        expected_ret_type,
    } = expected_type;

    let bexpr_span = bexpr.get_span();
    let BExpr { lhs, op, rhs } = bexpr;
    let (lhs_val, lhs_type) = codegen_rhs_expr(compiler, lhs, expected_lhs_type, block_type)?;
    let (rhs_val, rhs_type) = codegen_rhs_expr(compiler, rhs, expected_rhs_type, block_type)?;

    if lhs_type.array_dimensions.len() > 0 || rhs_type.array_dimensions.len() > 0 {
        if lhs_type.array_dimensions.len() != rhs_type.array_dimensions.len() {
            return Err(CompilerError {
                reason: "Operations between different array nesting levels are not supported"
                    .to_string(),
                span: Some(bexpr_span),
            });
        }

        match op.node {
            BOp::CmpEq => {
                return Err(CompilerError {
                    reason: "Comparing two arrays for equality is not yet supported".to_string(),
                    span: Some(bexpr_span),
                });
            }
            unsupported_op => {
                let error = format!(
                    "{} operation is not supported for array types",
                    unsupported_op,
                );

                return Err(CompilerError {
                    reason: error,
                    span: Some(bexpr_span),
                });
            }
        }
    }

    if lhs_type.pointer_nesting_level > 0 || rhs_type.pointer_nesting_level > 0 {
        match op.node {
            BOp::CmpEq => {
                return Err(CompilerError {
                    reason: "Comparing two pointers for equality is not yet supported".to_string(),
                    span: Some(bexpr_span),
                });
            }
            unsupported_op => {
                let error = format!(
                    "{} operation is not supported for pointer types",
                    unsupported_op,
                );

                return Err(CompilerError {
                    reason: error,
                    span: Some(bexpr_span),
                });
            }
        }
    }

    let expr_type: ValueVarType = if let Some(et) = expected_ret_type.cloned() {
        if matches!(et.vtype, VarType::Boolean) {
            semantic_cube(&lhs_type, &rhs_type)?
        } else {
            et
        }
    } else {
        semantic_cube(&lhs_type, &rhs_type)?
    };

    if lhs_val.get_type() != rhs_val.get_type() {
        return Err(CompilerError { reason: format!(
            "Incompatible types {} and {} for operation '{}'. If both types are numeric, try casting one side of the operation",
            lhs_type, rhs_type, op
        ), span: Some(bexpr_span), });
    }

    let operand_type = expr_type.vtype.clone();
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
            let mut lhs = lhs_val.into_int_value();
            let mut rhs = rhs_val.into_int_value();

            let b = compiler.builder;
            let cast_type = convert_to_type_enum(compiler, &expr_type)?.into_int_type();
            if lhs_type != expr_type {
                lhs = b.build_int_cast(lhs, cast_type, "int_cast");
            }
            if rhs_type != expr_type {
                rhs = b.build_int_cast(rhs, cast_type, "int_cast");
            }

            let bop_res = match op.node {
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
                BOp::Pow => {
                    return Err(CompilerError {
                        reason: format!("The {} operator is not yet supported", BOp::Pow),
                        span: Some(bexpr_span),
                    })
                }
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
                BOp::CmpEq => b.build_int_compare(IntPredicate::EQ, lhs, rhs, "int_eq_cmp"),
                BOp::And | BOp::Or => {
                    return Err(CompilerError {
                        reason: "Invalid && or || comparison for int value".to_string(),
                        span: Some(bexpr_span),
                    })
                }
            };

            bop_res.as_basic_value_enum()
        }
        VarType::F32 | VarType::F64 => {
            let mut lhs = lhs_val.into_float_value();
            let mut rhs = rhs_val.into_float_value();

            let b = compiler.builder;
            let cast_type = convert_to_type_enum(compiler, &expr_type)?.into_float_type();
            if lhs_type != expr_type {
                lhs = b.build_float_cast(lhs, cast_type, "float_cast");
            }
            if rhs_type != expr_type {
                rhs = b.build_float_cast(rhs, cast_type, "float_cast");
            }

            let bop_res = match op.node {
                BOp::Add => b.build_float_add(lhs, rhs, "flt_add").as_basic_value_enum(),
                BOp::Sub => b.build_float_sub(lhs, rhs, "flt_sub").as_basic_value_enum(),
                BOp::Mul => b.build_float_mul(lhs, rhs, "flt_mul").as_basic_value_enum(),
                BOp::Div => b.build_float_div(lhs, rhs, "flt_div").as_basic_value_enum(),
                BOp::Mod => b.build_float_rem(lhs, rhs, "flt_mod").as_basic_value_enum(),
                BOp::Pow => {
                    return Err(CompilerError {
                        reason: format!("The {} operator is not yet supported", BOp::Pow),
                        span: Some(bexpr_span),
                    })
                }
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
                BOp::CmpEq => b
                    .build_float_compare(FloatPredicate::OEQ, lhs, rhs, "flt_eq_cmp")
                    .as_basic_value_enum(),
                BOp::And | BOp::Or => {
                    return Err(CompilerError {
                        reason: "Invalid && or || comparison for float value".to_string(),
                        span: Some(bexpr_span),
                    })
                }
            };

            bop_res
        }
        VarType::Void => {
            return Err(CompilerError {
                reason: "Binary operations between void types are invalid".to_string(),
                span: Some(bexpr_span),
            })
        }
        VarType::Boolean => {
            // Bool values are i1, so they are ints
            let lhs = lhs_val.into_int_value();
            let rhs = rhs_val.into_int_value();
            let b = compiler.builder;

            let bop_res = match op.node {
                BOp::Ne => b.build_int_compare(IntPredicate::NE, lhs, rhs, "bool_eq_cmp"),
                BOp::CmpEq => b.build_int_compare(IntPredicate::EQ, lhs, rhs, "bool_eq_cmp"),
                BOp::And => b.build_and(lhs, rhs, "bool_and"),
                BOp::Or => b.build_or(lhs, rhs, "bool_or"),
                unsupported_operation => {
                    return Err(CompilerError {
                        reason: format!(
                            "Unsupported operation {} between two booleans",
                            unsupported_operation
                        ),
                        span: Some(bexpr_span),
                    })
                }
            };

            bop_res.as_basic_value_enum()
        }
        VarType::Char => todo!(),
        VarType::String => todo!(),
        VarType::Custom(_) => {
            return Err(CompilerError {
                reason: "Classes are not yet supported".to_string(),
                span: None,
            })
        }
    };

    let cmp_expr_type = if !op.node.is_cmp() {
        expr_type
    } else {
        ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        }
    };

    Ok((basic_val, cmp_expr_type))
}
