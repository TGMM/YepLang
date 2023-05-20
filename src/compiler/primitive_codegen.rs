use super::{
    expr_codegen::codegen_rhs_expr,
    helpers::{Compiler, CompilerError, DEFAULT_TYPES},
    main_codegen::convert_to_type_enum,
};
use crate::ast::{
    ArrayVal, BoolLiteral, BoolUnaryOp, NumericLiteral, NumericUnaryOp, PrimitiveVal, ValueVarType,
    VarType,
};
use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum},
};
use std::{collections::VecDeque, mem::transmute};

pub fn codegen_int_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    int_str: &str,
    uop: Option<NumericUnaryOp>,
    mut expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
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

    Ok((basic_val, expected_type.unwrap().clone()))
}

pub fn codegen_float_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    float_str: &str,
    uop: Option<NumericUnaryOp>,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
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

    Ok((
        basic_val,
        expected_type
            .unwrap_or(&ValueVarType {
                vtype: VarType::F32,
                array_dimensions: VecDeque::new(),
                pointer_nesting_level: 0,
            })
            .clone(),
    ))
}

pub fn codegen_bool_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    bool_literal: BoolLiteral,
    buop: Option<BoolUnaryOp>,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
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

    Ok((
        basic_val,
        expected_type
            .unwrap_or(&ValueVarType {
                vtype: VarType::Boolean,
                array_dimensions: VecDeque::new(),
                pointer_nesting_level: 0,
            })
            .clone(),
    ))
}

pub fn codegen_arr_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    arr_val: ArrayVal,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
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

    Ok((array_val.as_basic_value_enum(), resulting_type))
}

pub fn codegen_primitive_val<'input, 'ctx>(
    compiler: &Compiler<'input, 'ctx>,
    primitive_val: PrimitiveVal,
    expected_type: Option<&ValueVarType>,
) -> Result<(BasicValueEnum<'ctx>, ValueVarType), CompilerError> {
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

            Ok((
                str_val,
                ValueVarType {
                    vtype: VarType::String,
                    array_dimensions: VecDeque::new(),
                    pointer_nesting_level: 0,
                },
            ))
        }
        PrimitiveVal::Array(arr_val) => codegen_arr_val(compiler, arr_val, expected_type),
        PrimitiveVal::Struct(_) => todo!(),
    }?;

    Ok((bv, vvt))
}
