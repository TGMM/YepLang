use super::helpers::{
    convert_type_to_metadata, convert_value_to_metadata, Compiler, ScopeMarker, ScopedVal,
    ScopedVar,
};
use crate::{
    ast::{
        Assignment, BExpr, BOp, Block, BoolLiteral, BoolUnaryOp, Destructure, Expr, ExternDecl,
        ExternType, FnCall, FnDecl, NumericLiteral, NumericUnaryOp, PrimitiveVal, Stmt, TopBlock,
        ValueVarType, VarDecl, VarType,
    },
    compiler::helpers::ScopedFunc,
};
use inkwell::{
    module::Linkage,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use std::{mem::transmute, path::Path};

const MAIN_FN_NAME: &str = "main";
impl<'input, 'ctx> Compiler<'input, 'ctx> {
    pub fn convert_to_type_enum(&self, vvt: ValueVarType) -> BasicTypeEnum<'ctx> {
        let ctx = self.context;
        let basic_vtype: Option<BasicTypeEnum> = match vvt.vtype {
            VarType::I8 | VarType::U8 => ctx.i8_type().as_basic_type_enum().into(),
            VarType::I16 | VarType::U16 => ctx.i16_type().as_basic_type_enum().into(),
            VarType::I32 | VarType::U32 => ctx.i32_type().as_basic_type_enum().into(),
            VarType::I64 | VarType::U64 => ctx.i64_type().as_basic_type_enum().into(),
            VarType::I128 | VarType::U128 => ctx.i128_type().as_basic_type_enum().into(),
            VarType::F32 => ctx.f32_type().as_basic_type_enum().into(),
            VarType::F64 => ctx.f64_type().as_basic_type_enum().into(),
            VarType::Boolean => ctx.bool_type().as_basic_type_enum().into(),
            _ => None,
        };

        let mut final_type = basic_vtype.unwrap();
        for _ in 0..vvt.array_nesting_level {
            // TODO: Specify size here
            final_type = final_type.array_type(1).as_basic_type_enum()
        }
        for _ in 0..vvt.pointer_nesting_level {
            final_type = final_type
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum()
        }

        final_type
    }

    pub fn declare_variable(&mut self, var_name: String, value: ScopedVar<'ctx>) {
        if let Some(redeclared_var) = self.curr_scope_vars.remove(&var_name) {
            // Var was already declared so we push it to the stack
            // to recover it later
            self.scope_stack
                .push(ScopeMarker::Var(var_name.clone(), Some(redeclared_var)))
        } else {
            // Var was not declared so we push it to the stack
            // to delete it later
            self.scope_stack
                .push(ScopeMarker::Var(var_name.clone(), None))
        }

        self.curr_scope_vars.insert(var_name, ScopedVal::Var(value));
    }

    pub fn codegen_top_block(&mut self, top_block: TopBlock) {
        let fn_type = self.context.i32_type().fn_type(&[], false);
        let fun = self
            .module
            .add_function(MAIN_FN_NAME, fn_type, Some(Linkage::External));
        let entry_basic_block = self.context.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry_basic_block);
        self.basic_block_stack.push(entry_basic_block);

        // Codegen all statements
        self.codegen_block(top_block.0, true, false);

        let int_zero = self.context.i32_type().const_zero();
        self.builder.build_return(Some(&int_zero));
    }

    fn codegen_block(&mut self, block: Block, is_global: bool, func_block: bool) {
        if !func_block {
            self.scope_stack.push(ScopeMarker::ScopeBegin);
        }

        for stmt in block.stmts {
            self.codegen_stmt(stmt, is_global);
        }

        // Now we swap
        while let Some(sm) = self.scope_stack.pop() {
            // Scope has ended, exit loop
            match sm {
                ScopeMarker::ScopeBegin => break,
                ScopeMarker::Var(id, Some(var_val)) => {
                    // Var existed before, we return it to it's previous value
                    self.curr_scope_vars.remove(&id);
                    self.curr_scope_vars.insert(id, var_val);
                }
                ScopeMarker::Var(id, None) => {
                    // Var didn't exist before, we just remove it from var map
                    self.curr_scope_vars.remove(&id);
                }
            }
        }
    }

    fn codegen_stmt(&mut self, stmt: Stmt, is_global: bool) {
        match stmt {
            Stmt::Assignment(assignment) => self.codegen_assignment(assignment),
            Stmt::Expr(expr) => _ = self.codegen_rhs_expr(expr, None),
            Stmt::ClassDecl(_) => todo!(),
            Stmt::FnDecl(fn_decl) => self.codegen_fn_decl(fn_decl, is_global),
            Stmt::For(_) => todo!(),
            Stmt::While(_) => todo!(),
            Stmt::DoWhile(_) => todo!(),
            Stmt::If(_) => todo!(),
            Stmt::Block(block) => self.codegen_block(block, false, false),
            Stmt::VarDecl(var_decl) => self.codegen_var_decl(var_decl, is_global),
            Stmt::ExternDecl(extern_decl) => self.codegen_extern_decl(extern_decl),
        }
    }

    pub fn codegen_fn_decl(&mut self, fn_decl: FnDecl, is_global: bool) {
        let fn_name = fn_decl.fn_id.0;

        let fn_type = self.context.void_type().fn_type(
            &[BasicMetadataTypeEnum::IntType(self.context.i32_type())],
            false,
        );
        let linkage = if is_global {
            Linkage::External
        } else {
            Linkage::Internal
        };
        let fun = self.module.add_function(&fn_name, fn_type, Some(linkage));

        assert_eq!(fun.count_params() as usize, fn_decl.args.len());

        let basic_block_name = format!("{}_bb", fn_name);
        let fn_basic_block = self.context.append_basic_block(fun, &basic_block_name);
        self.builder.position_at_end(fn_basic_block);
        self.basic_block_stack.push(fn_basic_block);

        let ret_type = fn_decl.ret_type.unwrap_or(ValueVarType {
            vtype: VarType::Void,
            array_nesting_level: 0,
            pointer_nesting_level: 0,
        });
        // TODO: The current function call searches for it on
        // the module instead of the var map
        self.curr_scope_vars.insert(
            fn_name,
            ScopedVal::Fn(ScopedFunc {
                ptr_val: fun,
                ret_type,
            }),
        );

        self.scope_stack.push(ScopeMarker::ScopeBegin);
        for ((arg_destructure, arg_type), arg_val) in
            fn_decl.args.into_iter().zip(fun.get_param_iter())
        {
            let ty = self.convert_to_type_enum(arg_type.clone());
            match arg_destructure {
                Destructure::Id(id) => {
                    let arg_ptr = self.builder.build_alloca(ty, &id.0);
                    self.builder.build_store(arg_ptr, arg_val);
                    self.declare_variable(
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

        self.codegen_block(fn_decl.block, false, true);
        self.builder.build_return(None);

        self.basic_block_stack.pop();
        let prev_basic_block = self
            .basic_block_stack
            .last()
            .expect("Unexpected error: Basic block stack is empty");
        self.builder.position_at_end(*prev_basic_block);
    }

    pub fn codegen_extern_decl(&mut self, extern_decl: ExternDecl) {
        let fn_name = &extern_decl.fn_id.0;
        let ret_type = self.convert_to_type_enum(extern_decl.ret_type.clone());

        let mut is_var_args = false;
        let mut param_types = vec![];
        for arg_type in extern_decl.arg_types {
            match arg_type {
                ExternType::Type(vvt) => {
                    let ty = self.convert_to_type_enum(vvt);
                    let metadata = convert_type_to_metadata(ty);
                    param_types.push(metadata);
                }
                // TODO: This should only apply if the var_args is last
                ExternType::Spread => is_var_args = true,
            }
        }

        let fn_type = ret_type.fn_type(&param_types, is_var_args);
        let fun = self
            .module
            .add_function(fn_name, fn_type, Some(Linkage::External));

        self.curr_scope_vars.insert(
            fn_name.to_string(),
            ScopedVal::Fn(ScopedFunc {
                ptr_val: fun,
                ret_type: extern_decl.ret_type,
            }),
        );
    }

    pub fn codegen_assignment(&self, assignment: Assignment) {
        let assignee_ptr = self.codegen_lhs_expr(assignment.assignee_expr, None);
        let new_val = self
            .codegen_rhs_expr(assignment.assigned_expr, None)
            .unwrap();

        self.builder.build_store(assignee_ptr, new_val.0);
    }

    /// Codegen an expression when it's used on the left-hand side of an operation.
    pub fn codegen_lhs_expr(
        &self,
        expr: Expr,
        expected_type: Option<&ValueVarType>,
    ) -> PointerValue {
        match expr {
            Expr::ParenExpr(None, expr) => self.codegen_lhs_expr(*expr, expected_type),
            Expr::Indexing(_) => todo!(),
            Expr::MemberAccess(_) => todo!(),
            Expr::Id(var_id) => {
                let var_ptr = self
                    .curr_scope_vars
                    .get(&var_id.0)
                    // TODO: This should be an user facing error
                    // and not a panic
                    .expect("Undeclared variable or function");

                match var_ptr {
                    ScopedVal::Var(v) => v.ptr_val,
                    ScopedVal::Fn(_) => todo!(),
                }
            }
            _ => panic!("Left-hand side of assignment can't be ..."),
        }
    }

    pub fn codegen_bexpr(
        &self,
        bexpr: BExpr,
        expected_type: Option<&ValueVarType>,
    ) -> Result<(BasicValueEnum, ValueVarType), String> {
        let BExpr { lhs, op, rhs } = bexpr;
        let lhs = self.codegen_rhs_expr(lhs, expected_type)?;
        let rhs = self.codegen_rhs_expr(rhs, expected_type)?;

        let (lhs_type, rhs_type) = (lhs.1, rhs.1);
        if lhs_type.array_nesting_level > 0 || rhs_type.array_nesting_level > 0 {
            if lhs_type.array_nesting_level != rhs_type.array_nesting_level {
                return Err(
                    "Operations between different array nesting levels are not supported"
                        .to_string(),
                );
            }

            match op {
                BOp::Eq => {
                    return Err(
                        "Comparing two arrays for equality is not yet supported".to_string()
                    );
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
                    return Err(
                        "Comparing two pointers for equality is not yet supported".to_string()
                    );
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

        if lhs_type.vtype != rhs_type.vtype {
            return Err("Invalid binary operation between two different types".to_string());
        }

        let operand_type = lhs_type.vtype;
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
                let lhs = lhs.0.into_int_value();
                let rhs = rhs.0.into_int_value();
                let b = self.builder;

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
                    BOp::Pow => {
                        return Err(format!("The {} operator is not yet supported", BOp::Pow))
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
                    BOp::Eq => b.build_int_compare(IntPredicate::EQ, lhs, rhs, "int_eq_cmp"),
                };

                bop_res.as_basic_value_enum()
            }
            VarType::F32 | VarType::F64 => {
                let lhs = lhs.0.into_float_value();
                let rhs = rhs.0.into_float_value();
                let b = self.builder;

                let bop_res = match op {
                    BOp::Add => b.build_float_add(lhs, rhs, "flt_add").as_basic_value_enum(),
                    BOp::Sub => b.build_float_sub(lhs, rhs, "flt_sub").as_basic_value_enum(),
                    BOp::Mul => b.build_float_mul(lhs, rhs, "flt_mul").as_basic_value_enum(),
                    BOp::Div => b.build_float_div(lhs, rhs, "flt_div").as_basic_value_enum(),
                    BOp::Mod => b.build_float_rem(lhs, rhs, "flt_mod").as_basic_value_enum(),
                    BOp::Pow => {
                        return Err(format!("The {} operator is not yet supported", BOp::Pow))
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
                let b = self.builder;

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
                array_nesting_level: 0,
                pointer_nesting_level: 0,
            }
        };
        Ok((basic_val, expr_type))
    }

    /// Codegen an expression when it's used on the right-hand side of an operation.
    /// * `expected_type` - The expected result type of the expression.
    /// Only useful if we're coming from an assignment or the var decl has an explicit type,
    /// otherwise inferred to defaults
    pub fn codegen_rhs_expr(
        &self,
        expr: Expr,
        expected_type: Option<&ValueVarType>,
    ) -> Result<(BasicValueEnum, ValueVarType), String> {
        match expr {
            // TODO: Handle expr unary operator
            Expr::ParenExpr(_, expr) => self.codegen_rhs_expr(*expr, expected_type),
            Expr::BinaryExpr(bexpr) => self.codegen_bexpr(*bexpr, expected_type),
            Expr::PrimitiveVal(primitive_val) => {
                Ok(self.codegen_primitive_val(primitive_val, expected_type))
            }
            Expr::FnCall(fn_call) => self.codegen_fn_call(*fn_call),
            Expr::Indexing(_) => todo!(),
            Expr::MemberAccess(_) => todo!(),
            Expr::Id(var_id) => {
                let scoped_val = self
                    .curr_scope_vars
                    .get(&var_id.0)
                    .ok_or("Undeclared variable")?;
                let scoped_var = scoped_val.clone().into_var().map_err(|_| {
                    "Using function pointer as a value is not supported".to_string()
                })?;

                let instruction_name = format!("load_{}", var_id.0);
                let var_val = self
                    .builder
                    .build_load(scoped_var.ptr_val, &instruction_name);

                Ok((var_val, scoped_var.var_type.clone()))
            }
        }
    }

    pub fn codegen_fn_call(
        &self,
        fn_call: FnCall,
    ) -> Result<(BasicValueEnum, ValueVarType), String> {
        let fn_name = match fn_call.fn_expr {
            Expr::Id(id) => id.0,
            _ => return Err("Functions as values are not yet supported".to_string()),
        };
        // TODO: Handle this error user-side
        let function = self
            .curr_scope_vars
            .get(&fn_name)
            .ok_or("Function does not exist")?
            .clone()
            .into_fn()
            .unwrap()
            .ptr_val;

        let mut args = vec![];
        for arg_expr in fn_call.args {
            let arg_val = self.codegen_rhs_expr(arg_expr, None)?;
            let arg_metadata: BasicMetadataValueEnum = convert_value_to_metadata(arg_val.0);
            args.push(arg_metadata);
        }

        let instruction_name = format!("call_{}", fn_name);
        let call = self.builder.build_call(function, &args, &instruction_name);
        let ret_val = call.try_as_basic_value();

        let vvt = if ret_val.left().is_none() {
            ValueVarType {
                array_nesting_level: 0,
                pointer_nesting_level: 0,
                vtype: VarType::Void,
            }
        } else {
            ValueVarType {
                array_nesting_level: 0,
                pointer_nesting_level: 0,
                vtype: VarType::Void,
            }
        };

        ret_val
            .left()
            .map(|rv| (rv, vvt))
            .ok_or("Fn call returned void".to_string())
    }

    pub fn codegen_int_val(
        &self,
        int_str: &str,
        uop: Option<NumericUnaryOp>,
        expected_type: Option<&ValueVarType>,
    ) -> (BasicValueEnum, ValueVarType) {
        let int_type;
        let mut u64_val_res: u64;
        let mut unsigned = false;

        if let Some(expected_type) = expected_type {
            if expected_type.array_nesting_level > 0 {
                panic!("Unexpected assignment of number to array type");
            }
            if expected_type.pointer_nesting_level > 0 {
                panic!("Unexpected assignment of number to pointer type. Explicit address assignment is not supported.");
            }

            match &expected_type.vtype {
                VarType::I8 => {
                    int_type = self.context.i8_type();
                    let int_val = int_str.parse::<i8>().unwrap();
                    let i64_val: i64 = int_val.into();
                    u64_val_res = unsafe { transmute(i64_val) }
                }
                VarType::U8 => {
                    int_type = self.context.i8_type();
                    let int_val = int_str.parse::<u8>().unwrap();
                    let u64_val: u64 = int_val.into();
                    unsigned = true;
                    u64_val_res = unsafe { transmute(u64_val) }
                }
                VarType::I16 => {
                    int_type = self.context.i16_type();
                    let int_val = int_str.parse::<i16>().unwrap();
                    let i64_val: i64 = int_val.into();
                    u64_val_res = unsafe { transmute(i64_val) }
                }
                VarType::U16 => {
                    int_type = self.context.i16_type();
                    let int_val = int_str.parse::<u16>().unwrap();
                    let u64_val: u64 = int_val.into();
                    unsigned = true;
                    u64_val_res = unsafe { transmute(u64_val) }
                }
                VarType::I32 => {
                    int_type = self.context.i32_type();
                    let int_val = int_str.parse::<i32>().unwrap();
                    let i64_val: i64 = int_val.into();
                    u64_val_res = unsafe { transmute(i64_val) }
                }
                VarType::U32 => {
                    int_type = self.context.i32_type();
                    let int_val = int_str.parse::<u32>().unwrap();
                    let u64_val: u64 = int_val.into();
                    unsigned = true;
                    u64_val_res = unsafe { transmute(u64_val) }
                }
                VarType::I64 => {
                    int_type = self.context.i64_type();
                    let int_val = int_str.parse::<i64>().unwrap();
                    u64_val_res = unsafe { transmute(int_val) }
                }
                VarType::U64 => {
                    int_type = self.context.i64_type();
                    let int_val = int_str.parse::<u64>().unwrap();
                    unsigned = true;
                    u64_val_res = int_val;
                }
                VarType::I128 | VarType::U128 => {
                    panic!("128 bit integers are not supported.")
                }
                ty => panic!("Invalid int assignment to {}", ty),
            }
        } else {
            // Default value is i32
            int_type = self.context.i32_type();
            let int_val = int_str.parse::<i32>().unwrap();
            let i64_val: i64 = int_val.into();
            u64_val_res = unsafe { transmute(i64_val) }
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

        (
            basic_val,
            expected_type
                .unwrap_or(&ValueVarType {
                    vtype: VarType::I32,
                    array_nesting_level: 0,
                    pointer_nesting_level: 0,
                })
                .clone(),
        )
    }

    pub fn codegen_float_val(
        &self,
        float_str: &str,
        uop: Option<NumericUnaryOp>,
        expected_type: Option<&ValueVarType>,
    ) -> (BasicValueEnum, ValueVarType) {
        let float_type;
        let mut f64_val_res: f64;

        if let Some(expected_type) = expected_type {
            if expected_type.array_nesting_level > 0 {
                panic!("Unexpected assignment of number to array type");
            }
            if expected_type.pointer_nesting_level > 0 {
                panic!("Unexpected assignment of number to pointer type. Explicit address assignment is not supported.");
            }

            match &expected_type.vtype {
                VarType::F32 => {
                    float_type = self.context.f32_type();
                    let float_val = float_str.parse::<f32>().unwrap();
                    f64_val_res = float_val.into();
                }
                VarType::F64 => {
                    float_type = self.context.f64_type();
                    let float_val = float_str.parse::<f64>().unwrap();
                    f64_val_res = float_val.into();
                }
                ty => panic!("Invalid int assignment to {}", ty),
            }
        } else {
            // Default value is f32
            float_type = self.context.f32_type();
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

        (
            basic_val,
            expected_type
                .unwrap_or(&ValueVarType {
                    vtype: VarType::F32,
                    array_nesting_level: 0,
                    pointer_nesting_level: 0,
                })
                .clone(),
        )
    }

    pub fn codegen_bool_val(
        &self,
        bool_literal: BoolLiteral,
        buop: Option<BoolUnaryOp>,
        expected_type: Option<&ValueVarType>,
    ) -> (BasicValueEnum, ValueVarType) {
        let bool_type = self.context.bool_type();
        let mut bool_val_res: bool;

        if let Some(expected_type) = expected_type {
            if expected_type.array_nesting_level > 0 {
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

        (
            basic_val,
            expected_type
                .unwrap_or(&ValueVarType {
                    vtype: VarType::Boolean,
                    array_nesting_level: 0,
                    pointer_nesting_level: 0,
                })
                .clone(),
        )
    }

    pub fn codegen_primitive_val(
        &self,
        primitive_val: PrimitiveVal,
        expected_type: Option<&ValueVarType>,
    ) -> (BasicValueEnum, ValueVarType) {
        match primitive_val {
            PrimitiveVal::Number(uop, numeric_literal) => match numeric_literal {
                NumericLiteral::Int(i) => self.codegen_int_val(i, uop, expected_type),
                NumericLiteral::Float(f) => self.codegen_float_val(f, uop, expected_type),
            },
            PrimitiveVal::Boolean(buop, bool_literal) => {
                self.codegen_bool_val(bool_literal, buop, expected_type)
            }
            PrimitiveVal::Char(_) => todo!(),
            PrimitiveVal::String(string) => unsafe {
                let str_val = self
                    .builder
                    .build_global_string(&string, "globstr")
                    .as_basic_value_enum();

                (
                    str_val,
                    ValueVarType {
                        vtype: VarType::String,
                        array_nesting_level: 0,
                        pointer_nesting_level: 0,
                    },
                )
            },
            PrimitiveVal::Array(_) => todo!(),
            PrimitiveVal::Struct(_) => todo!(),
        }
    }

    fn codegen_var_decl(&mut self, var_decl: VarDecl, is_global: bool) {
        // TODO: Handle scope specifier
        for decl_as in var_decl.decl_assignments {
            // TODO: Handle destructures instead of only ids
            let id = match decl_as.destructure {
                Destructure::Id(id) => id,
                _ => panic!("Destructures are not supported yet"),
            };
            let (initial_val, var_type) = self
                .codegen_rhs_expr(decl_as.expr, decl_as.var_type.as_ref())
                .unwrap();
            let type_ = self.convert_to_type_enum(var_type.clone());

            if is_global {
                let new_global = self.module.add_global(type_, None, &id.0);
                new_global.set_initializer(&initial_val);
                let global_ptr = new_global.as_pointer_value();
                self.curr_scope_vars.insert(
                    id.0,
                    ScopedVal::Var(ScopedVar {
                        ptr_val: global_ptr,
                        var_type,
                    }),
                );

                return;
            }

            let new_local = self.builder.build_alloca(type_, &id.0);
            self.builder.build_store(new_local, initial_val);
            self.declare_variable(
                id.0,
                ScopedVar {
                    ptr_val: new_local,
                    var_type,
                },
            );
        }
    }

    pub fn compile_to_x86(
        compiler: &Compiler<'input, 'ctx>,
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
}
