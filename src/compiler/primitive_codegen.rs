use super::{
    expr_codegen::codegen_rhs_expr,
    helpers::{create_default_type, BlockType, Compiler, CompilerError},
    main_codegen::convert_to_type_enum,
};
use crate::{
    ast::{
        ArrayVal, BoolLiteral, BoolUnaryOp, NumericLiteral, NumericUnaryOp, PrimitiveVal,
        ValueVarType, VarType,
    },
    spanned_ast::SpannedAstNode,
};
use inkwell::{
    types::{BasicTypeEnum, FloatType, IntType},
    values::{ArrayValue, BasicValue, BasicValueEnum},
};
use std::{collections::VecDeque, mem::transmute};

pub fn codegen_int_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    int_str: &str,
    uop: Option<SpannedAstNode<NumericUnaryOp>>,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let (mut int_val, int_type, var_type): (u64, IntType, ValueVarType) = match expected_type {
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::I8 => {
            let i8_type = compiler.context.i8_type();
            let i8_val = int_str.parse::<i8>().unwrap();
            let i64_val: i64 = i8_val.into();
            let u64_val = unsafe { transmute(i64_val) };

            (u64_val, i8_type, create_default_type(vtype.clone()))
        }
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::U8 => {
            let u8_type = compiler.context.i8_type();
            let u8_val = int_str.parse::<u8>().unwrap();
            let i64_val: i64 = u8_val.into();
            let u64_val = unsafe { transmute(i64_val) };

            (u64_val, u8_type, create_default_type(vtype.clone()))
        }
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::I16 => {
            let i16_type = compiler.context.i16_type();
            let i16_val = int_str.parse::<i16>().unwrap();
            let i64_val: i64 = i16_val.into();
            let u64_val = unsafe { transmute(i64_val) };

            (u64_val, i16_type, create_default_type(vtype.clone()))
        }
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::U16 => {
            let i16_type = compiler.context.i16_type();
            let i16_val = int_str.parse::<i16>().unwrap();
            let i64_val: i64 = i16_val.into();
            let u64_val = unsafe { transmute(i64_val) };

            (u64_val, i16_type, create_default_type(vtype.clone()))
        }
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::U32 => {
            let u32_type = compiler.context.i32_type();
            let u32_val = int_str.parse::<u32>().unwrap();
            let i64_val: i64 = u32_val.into();
            let u64_val = unsafe { transmute(i64_val) };

            (u64_val, u32_type, create_default_type(vtype.clone()))
        }
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::I64 => {
            let i64_type = compiler.context.i64_type();
            let i64_val = int_str.parse::<i64>().unwrap();
            let u64_val = unsafe { transmute(i64_val) };

            (u64_val, i64_type, create_default_type(vtype.clone()))
        }
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::U64 => {
            let u64_type = compiler.context.i64_type();
            let u64_val = int_str.parse::<u64>().unwrap();

            (u64_val, u64_type, create_default_type(vtype.clone()))
        }
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::I128 || vtype == &VarType::U128 => {
            return Err("128 bit integers are not supported".to_string());
        }
        _ => {
            // Default value is i32
            let i32_type = compiler.context.i32_type();
            let i32_val = int_str.parse::<i32>().unwrap();
            let i64_val: i64 = i32_val.into();
            let u64_val = unsafe { transmute(i64_val) };

            (u64_val, i32_type, create_default_type(VarType::I32))
        }
    };

    let unsigned = !var_type.vtype.is_signed();
    if matches!(
        uop,
        Some(SpannedAstNode {
            node: NumericUnaryOp::Minus,
            span: _
        })
    ) {
        if unsigned {
            return Err("Can't apply the minus unary operator to an unsigned integer".to_string());
        }
        let mut i64_val: i64 = unsafe { transmute(int_val) };
        i64_val = -i64_val;
        int_val = unsafe { transmute(i64_val) }
    }

    let basic_val = int_type.const_int(int_val, false).as_basic_value_enum();

    Ok((basic_val, var_type))
}

pub fn codegen_float_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    float_str: &str,
    uop: Option<SpannedAstNode<NumericUnaryOp>>,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let (mut double_val, float_type, var_type): (f64, FloatType, ValueVarType) = match expected_type
    {
        Some(ValueVarType {
            vtype,
            array_dimensions: _,
            pointer_nesting_level: _,
        }) if vtype == &VarType::F64 => {
            let float_type = compiler.context.f64_type();
            let double_val = float_str.parse::<f64>().unwrap();
            (
                double_val.into(),
                float_type,
                ValueVarType {
                    vtype: VarType::F64,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0,
                },
            )
        }
        _ => {
            let float_type = compiler.context.f32_type();
            let float_val = float_str.parse::<f32>().unwrap();
            let double_val: f64 = float_val.into();
            (
                double_val,
                float_type,
                ValueVarType {
                    vtype: VarType::F32,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0,
                },
            )
        }
    };

    if let Some(uop) = uop {
        match uop.node {
            NumericUnaryOp::Minus => {
                double_val = -double_val;
            }
            NumericUnaryOp::Plus => {}
        }
    }

    let basic_val = float_type.const_float(double_val).as_basic_value_enum();
    Ok((basic_val, var_type))
}

pub fn codegen_bool_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    bool_literal: BoolLiteral,
    buop: Option<SpannedAstNode<BoolUnaryOp>>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let bool_type = compiler.context.bool_type();
    let mut bool_val = bool_literal.0;

    if let Some(buop) = buop {
        match buop.node {
            BoolUnaryOp::Not => {
                bool_val = !bool_val;
            }
        }
    }

    let u64_val: u64 = bool_val.into();
    let basic_val = bool_type.const_int(u64_val, false).as_basic_value_enum();

    Ok((
        basic_val,
        ValueVarType {
            vtype: VarType::Boolean,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        },
    ))
}

pub fn codegen_arr_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    arr_val: ArrayVal,
    expected_type: Option<&ValueVarType>,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let exprs = arr_val.0;
    let exprs_len: u32 = exprs
        .len()
        .try_into()
        .expect("Array dimensions should fit inside an u32");

    if exprs.is_empty() && expected_type.is_none() {
        return Err("Can't infer type of empty array".to_string());
    }
    if let Some(et) = expected_type {
        let expected_dim = *et
            .array_dimensions
            .get(0)
            .ok_or(format!("Expected {}, got array instead", et))?;

        if expected_dim != exprs_len {
            return Err(format!(
                "Array declared with {} elements, but assigned value has {} elements",
                expected_dim, exprs_len
            ));
        }
    }

    let mut expected_element_type = expected_type.map(|et| {
        // Same type but of individual element
        let mut element_type = et.clone();
        element_type.array_dimensions.pop_front();

        element_type
    });

    let mut vals = vec![];
    for expr in exprs {
        let (val, ty_) =
            codegen_rhs_expr(compiler, expr, expected_element_type.as_ref(), block_type).unwrap();

        if let Some(ref eet) = expected_element_type {
            if eet != &ty_ {
                return Err(format!(
                    "Found element of type {} in declaration of array with elements of type {}",
                    ty_, eet
                ));
            }
        }

        expected_element_type.get_or_insert(ty_);

        vals.push(val);
    }

    // By this point we should have a type
    let mut resulting_type = expected_element_type.unwrap();
    let element_b_type = convert_to_type_enum(compiler, &resulting_type)?;
    let array_val: ArrayValue = match element_b_type {
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

    Ok((array_val.as_basic_value_enum(), resulting_type))
}

pub fn codegen_primitive_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    primitive_val: PrimitiveVal,
    expected_type: Option<&ValueVarType>,
    block_type: BlockType,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
    let (bv, vvt) = match primitive_val {
        PrimitiveVal::Number(uop, numeric_literal) => match numeric_literal {
            NumericLiteral::Int(i) => codegen_int_val(compiler, i, uop, expected_type),
            NumericLiteral::Float(f) => codegen_float_val(compiler, f, uop, expected_type),
        },
        PrimitiveVal::Boolean(buop, bool_literal) => codegen_bool_val(compiler, bool_literal, buop),
        PrimitiveVal::Char(_) => todo!(),
        PrimitiveVal::String(string) => {
            let str_val = compiler
                .builder
                .build_global_string_ptr(&string, "globstr")
                .as_basic_value_enum();

            Ok((
                str_val,
                ValueVarType {
                    vtype: VarType::String,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0,
                },
            ))
        }
        PrimitiveVal::Array(arr_val) => {
            codegen_arr_val(compiler, arr_val, expected_type, block_type)
        }
        PrimitiveVal::Struct(_) => todo!(),
    }?;

    Ok((bv, vvt))
}
