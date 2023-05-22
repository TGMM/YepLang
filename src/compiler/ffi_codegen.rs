use super::{
    helpers::{convert_type_to_metadata, Compiler, ScopedVal},
    main_codegen::convert_to_type_enum,
};
use crate::{
    ast::{ExternDecl, ExternType},
    compiler::helpers::ScopedFunc,
};
use inkwell::{module::Linkage, types::BasicType};

pub fn codegen_extern_decl(compiler: &mut Compiler, extern_decl: ExternDecl) -> Result<(), String> {
    let fn_name = &extern_decl.fn_id.0;
    let ret_type = convert_to_type_enum(compiler, &extern_decl.ret_type)?;

    let mut is_var_args = false;
    let mut param_types = vec![];
    for arg_type in extern_decl.arg_types.iter() {
        match arg_type {
            ExternType::Type(vvt) => {
                let ty = convert_to_type_enum(compiler, vvt)?;
                let metadata = convert_type_to_metadata(ty);
                param_types.push(metadata);
            }
            // TODO: This should only apply if the var_args is last
            ExternType::Spread => is_var_args = true,
        }
    }

    let fn_type = ret_type.fn_type(&param_types, is_var_args);
    let fun = compiler
        .module
        .add_function(fn_name, fn_type, Some(Linkage::External));

    compiler.curr_scope_vars.insert(
        fn_name.to_string(),
        ScopedVal::Fn(ScopedFunc {
            ptr_val: fun,
            arg_types: extern_decl.arg_types,
            ret_type: extern_decl.ret_type,
        }),
    );

    Ok(())
}
