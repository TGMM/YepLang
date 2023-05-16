use std::collections::VecDeque;

use inkwell::{
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
    FloatPredicate, IntPredicate,
};

use super::{
    helpers::{
        convert_value_to_metadata, semantic_cube, Compiler, ExpectedExprType, ScopedVal,
        DEFAULT_TYPES,
    },
    main_codegen::convert_to_type_enum,
    primitive_codegen::codegen_primitive_val,
};
use crate::ast::{BExpr, BOp, Expr, FnCall, Indexing, ValueVarType, VarType};

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

    let expr_type = semantic_cube(&lhs_type, &rhs_type)?;
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
            let mut lhs = lhs.0.into_int_value();
            let mut rhs = rhs.0.into_int_value();

            let b = compiler.builder;
            let cast_type = convert_to_type_enum(compiler, &expr_type).into_int_type();
            if lhs_type != expr_type {
                lhs = b.build_int_cast(lhs, cast_type, "int_cast");
            }
            if rhs_type != expr_type {
                rhs = b.build_int_cast(rhs, cast_type, "int_cast");
            }

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
            let mut lhs = lhs.0.into_float_value();
            let mut rhs = rhs.0.into_float_value();

            let b = compiler.builder;
            let cast_type = convert_to_type_enum(compiler, &expr_type).into_float_type();
            if lhs_type != expr_type {
                lhs = b.build_float_cast(lhs, cast_type, "float_cast");
            }
            if rhs_type != expr_type {
                rhs = b.build_float_cast(rhs, cast_type, "float_cast");
            }

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
                "Expected {} as result of the expression, found {}",
                &expected_ret_type, &expr_type
            );
        }
    }

    Ok((basic_val, expr_type))
}
