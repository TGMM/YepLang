use crate::ast::{ExternType, ValueVarType, VarType};
use bitflags::bitflags;
use enum_as_inner::EnumAsInner;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    data_layout::DataLayout,
    module::Module,
    passes::PassManager,
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue},
};
use rustc_hash::FxHashMap;
use std::collections::VecDeque;

pub type CompilerError = String;

#[derive(Debug, Clone, PartialEq)]
pub struct ScopedVar<'ctx> {
    pub ptr_val: PointerValue<'ctx>,
    pub var_type: ValueVarType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopedFunc<'ctx> {
    pub ptr_val: FunctionValue<'ctx>,
    pub arg_types: Vec<ExternType>,
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

pub struct ErrorMessages<'ctx> {
    pub out_of_bounds: Option<PointerValue<'ctx>>,
}

pub type Scope<'ctx> = FxHashMap<String, ScopedVal<'ctx>>;
pub struct Compiler<'input, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'input Builder<'ctx>,
    pub fpm: &'input PassManager<FunctionValue<'ctx>>,
    pub module: &'input Module<'ctx>,
    pub var_scopes: Vec<Scope<'ctx>>,
    pub basic_block_stack: Vec<BasicBlock<'ctx>>,
    /// Used only for return statements that need to know
    /// if we're inside a function, and its respective return type
    pub curr_func_ret_val: Option<FnRetVal<'ctx>>,
    pub func_ret_val_stack: Vec<FnRetVal<'ctx>>,
    pub data_layout: Option<DataLayout>,
    pub panic_fn: Option<FunctionValue<'ctx>>,
    pub err_msgs: ErrorMessages<'ctx>,
}

impl<'input, 'ctx> Compiler<'input, 'ctx> {
    pub fn get_curr_scope_mut(&mut self) -> Result<&mut Scope<'ctx>, CompilerError> {
        self.var_scopes
            .last_mut()
            .ok_or("ICE: No variable scopes".to_string())
    }

    pub fn get_scoped_val(
        &self,
        name: &str,
        block_type: BlockType,
    ) -> Result<&ScopedVal<'ctx>, CompilerError> {
        // Local function
        if block_type.contains(BlockType::FUNC_LOCAL) {
            assert!(self.var_scopes.len() > 1);

            let global_scope = self
                .var_scopes
                .first()
                .ok_or("ICE: Could not find global scope".to_string())?;

            let local_scope = self
                .var_scopes
                .last()
                .ok_or("ICE: Could not find local scope".to_string())?;

            // In a local function, we first search the local
            // scope for the value
            if let Some(scoped_val) = local_scope.get(name) {
                return Ok(scoped_val);
            }
            // If we don't find it, then we search the global scope
            if let Some(scoped_val) = global_scope.get(name) {
                return Ok(scoped_val);
            }

            // If we still don't find it, we return an error
            let line_one = "Variable or function was not found on the local function scope or the global scope.";
            let line_two = "Local functions do not capture their environment, and can't refer to local variables outside their scope.";
            return Err(format!("{}\n{}", line_one, line_two));
        }

        for scope in self.var_scopes.iter().rev() {
            if let Some(scoped_val) = scope.get(name) {
                return Ok(scoped_val);
            }
        }

        Err(format!("Undeclared variable or function {}", name))
    }
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
        return Err(format!("Incompatible types: {} and {}", lhs, rhs));
    }

    if lhs.vtype.is_float() != rhs.vtype.is_float() {
        return Err(format!("Incompatible types: {} and {}", lhs, rhs));
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

pub fn create_default_type(vtype: VarType) -> ValueVarType {
    ValueVarType {
        vtype,
        array_dimensions: VecDeque::new(),
        pointer_nesting_level: 0,
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct YepTarget {
    pub target_triple: String,
    pub nostd: bool,
}
