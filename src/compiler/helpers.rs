use crate::ast::ValueVarType;
use bitflags::bitflags;
use enum_as_inner::EnumAsInner;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};
use std::collections::HashMap;

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
        const FUNC_IF = Self::LOCAL.bits() | Self::FUNC.bits() | Self::IF.bits();
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
