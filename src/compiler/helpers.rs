use crate::ast::{ValueVarType, VarType};
use bitflags::bitflags;
use enum_as_inner::EnumAsInner;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    targets::TargetData,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};
use std::{
    cell::OnceCell,
    collections::{HashMap, VecDeque},
    sync::LazyLock,
};

pub type CompilerError = String;

pub static DEFAULT_TYPES: LazyLock<HashMap<VarType, ValueVarType>> = LazyLock::new(|| {
    let mut hm = HashMap::new();
    hm.insert(
        VarType::I32,
        ValueVarType {
            vtype: VarType::I32,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        },
    );
    hm.insert(
        VarType::U32,
        ValueVarType {
            vtype: VarType::U32,
            array_dimensions: VecDeque::new(),
            pointer_nesting_level: 0,
        },
    );

    hm
});

#[derive(Debug, Clone, PartialEq)]
pub enum ScopeMarker<'ctx> {
    ScopeBegin,
    Var(String, Option<ScopedVal<'ctx>>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopedVar<'ctx> {
    pub ptr_val: PointerValue<'ctx>,
    pub var_type: ValueVarType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopedFunc<'ctx> {
    pub ptr_val: FunctionValue<'ctx>,
    pub ret_type: ValueVarType,
}

#[derive(Debug, Clone, PartialEq, EnumAsInner)]
pub enum ScopedVal<'ctx> {
    Var(ScopedVar<'ctx>),
    Fn(ScopedFunc<'ctx>),
}

pub struct ExpectedExprType<'expr> {
    pub expected_lhs_type: Option<&'expr ValueVarType>,
    pub expected_rhs_type: Option<&'expr ValueVarType>,
    pub expected_ret_type: Option<&'expr ValueVarType>,
}

pub type BlockFlag = u8;
bitflags! {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub struct BlockType: BlockFlag {
        const GLOBAL = 0b00000001;
        const LOCAL = 0b00000010;
        const FUNC = 0b00000100;
        const IF = 0b00001000;
        const WHILE = 0b00010000;
        const FUNC_IF = Self::LOCAL.bits() | Self::FUNC.bits() | Self::IF.bits();
        const FUNC_WHILE = Self::LOCAL.bits() | Self::FUNC.bits() | Self::WHILE.bits();
        const FUNC_LOCAL = Self::FUNC.bits() | Self::LOCAL.bits();
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnRetVal<'ctx> {
    pub val: PointerValue<'ctx>,
    pub vtype: ValueVarType,
    pub ret_bb: BasicBlock<'ctx>,
}

pub struct Compiler<'input, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'input Builder<'ctx>,
    pub fpm: &'input PassManager<FunctionValue<'ctx>>,
    pub module: &'input Module<'ctx>,
    pub curr_scope_vars: HashMap<String, ScopedVal<'ctx>>,
    pub scope_stack: Vec<ScopeMarker<'ctx>>,
    pub basic_block_stack: Vec<BasicBlock<'ctx>>,
    /// Used only for return statements that need to know
    /// if we're inside a function, and its respective return type
    pub curr_func_ret_val: Option<FnRetVal<'ctx>>,
    pub func_ret_val_stack: Vec<FnRetVal<'ctx>>,
    /// This should be initialized before codegen
    pub target_data: OnceCell<TargetData>,
}

pub fn convert_type_to_metadata(ty: BasicTypeEnum) -> BasicMetadataTypeEnum {
    match ty {
        BasicTypeEnum::ArrayType(a) => BasicMetadataTypeEnum::ArrayType(a),
        BasicTypeEnum::FloatType(f) => BasicMetadataTypeEnum::FloatType(f),
        BasicTypeEnum::IntType(i) => BasicMetadataTypeEnum::IntType(i),
        BasicTypeEnum::PointerType(p) => BasicMetadataTypeEnum::PointerType(p),
        BasicTypeEnum::StructType(s) => BasicMetadataTypeEnum::StructType(s),
        BasicTypeEnum::VectorType(v) => BasicMetadataTypeEnum::VectorType(v),
    }
}

pub fn convert_value_to_metadata(val: BasicValueEnum) -> BasicMetadataValueEnum {
    match val {
        BasicValueEnum::ArrayValue(a) => BasicMetadataValueEnum::ArrayValue(a),
        BasicValueEnum::IntValue(i) => BasicMetadataValueEnum::IntValue(i),
        BasicValueEnum::FloatValue(f) => BasicMetadataValueEnum::FloatValue(f),
        BasicValueEnum::PointerValue(p) => BasicMetadataValueEnum::PointerValue(p),
        BasicValueEnum::StructValue(s) => BasicMetadataValueEnum::StructValue(s),
        BasicValueEnum::VectorValue(v) => BasicMetadataValueEnum::VectorValue(v),
    }
}

/// This only works for operand types
pub fn semantic_cube(lhs: &ValueVarType, rhs: &ValueVarType) -> Result<ValueVarType, String> {
    if lhs.array_dimensions.len() != rhs.array_dimensions.len()
        || lhs.array_dimensions != rhs.array_dimensions
    {
        return Err("Incompatible types: Array dimensions do not match".to_string());
    }

    if lhs.pointer_nesting_level != rhs.pointer_nesting_level {
        return Err("Incompatible types: Pointer nesting does not match".to_string());
    }

    if lhs.vtype.is_int() != rhs.vtype.is_int() {
        return Err("Incompatible types".to_string());
    }

    if lhs.vtype.is_float() != rhs.vtype.is_float() {
        return Err("Incompatible types".to_string());
    }

    let mut resulting_type: VarType = lhs.vtype.clone().max(rhs.vtype.clone());
    if resulting_type.is_int() {
        resulting_type = resulting_type.to_signed_int();
    }

    Ok(ValueVarType {
        vtype: resulting_type,
        array_dimensions: lhs.array_dimensions.clone(),
        pointer_nesting_level: lhs.pointer_nesting_level,
    })
}
