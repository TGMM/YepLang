use crate::ast::{Block, Stmt, TopBlock, ValueVarType, VarDecl, VarType};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicType, BasicTypeEnum},
    values::FunctionValue,
    OptimizationLevel,
};
use std::path::Path;

pub struct Compiler<'input, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'input Builder<'ctx>,
    pub fpm: &'input PassManager<FunctionValue<'ctx>>,
    pub module: &'input Module<'ctx>,
}

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
            // TODO: Specify size here
            final_type = final_type.into_pointer_type().as_basic_type_enum()
        }

        final_type
    }

    pub fn codegen_top_block(&self, top_block: TopBlock) {
        let fn_type = self.context.i32_type().fn_type(&[], false);
        let fun = self
            .module
            .add_function(MAIN_FN_NAME, fn_type, Some(Linkage::External));
        let entry_basic_block = self.context.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry_basic_block);

        // Codegen all statements
        self.codegen_block(top_block.0);

        let int_zero = self.context.i32_type().const_zero();
        self.builder.build_return(Some(&int_zero));
    }

    fn codegen_block(&self, block: Block) {
        for stmt in block.stmts {
            self.codegen_stmt(stmt);
        }
    }

    fn codegen_stmt(&self, stmt: Stmt) {
        match stmt {
            Stmt::Assignment(_) => todo!(),
            Stmt::Expr(_) => todo!(),
            Stmt::ClassDecl(_) => todo!(),
            Stmt::FnDecl(_) => todo!(),
            Stmt::For(_) => todo!(),
            Stmt::While(_) => todo!(),
            Stmt::DoWhile(_) => todo!(),
            Stmt::If(_) => todo!(),
            Stmt::Block(_) => todo!(),
            Stmt::VarDecl(vd) => self.var_decl_codegen(vd),
            Stmt::ExternDecl(_) => todo!(),
        }
    }

    fn var_decl_codegen(&self, var_decl: VarDecl) {
        // TODO: Handle var scope
        for decl_as in var_decl.decl_assignments {
            let id = match decl_as.destructure {
                crate::ast::Destructure::Id(id) => id,
                _ => todo!(),
            };

            let type_ = decl_as
                .var_type
                .map(|vvt| self.convert_to_type_enum(vvt))
                .expect("TODO: Handle type inference");
            self.module.add_global(type_, None, &id.0);
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
