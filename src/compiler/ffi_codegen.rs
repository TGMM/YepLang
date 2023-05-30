use super::{
    helpers::{convert_type_to_metadata, Compiler, ScopedVal},
    main_codegen::{convert_to_type_enum, declare_scoped_val},
};
use crate::{
    ast::{ExternDecl, ExternType, VarType},
    compiler::helpers::ScopedFunc,
};
use inkwell::{module::Linkage, types::BasicType};

pub fn codegen_extern_decl(compiler: &mut Compiler, extern_decl: ExternDecl) -> Result<(), String> {
    let fn_name = &extern_decl.fn_id.0;
    let vvt_ret_type = &extern_decl.ret_type;

    let mut is_var_args = false;
    let mut param_types = vec![];

    let mut arg_type_it = extern_decl.arg_types.iter().peekable();
    while let Some(arg_type) = arg_type_it.next() {
        match arg_type {
            ExternType::Type(vvt) => {
                let ty = convert_to_type_enum(compiler, vvt)?;
                let metadata = convert_type_to_metadata(ty);
                param_types.push(metadata);
            }
            // TODO: This should only apply if the var_args is last
            ExternType::Spread => {
                // This is not the last element
                // throw an error
                if !arg_type_it.peek().is_none() {
                    return Err(
                        "The var args specifier must be at the end of the argument list"
                            .to_string(),
                    );
                }

                is_var_args = true;
            }
        }
    }

    // If this is true, function is void
    let fn_type = if vvt_ret_type.array_dimensions.is_empty()
        && vvt_ret_type.pointer_nesting_level == 0
        && vvt_ret_type.vtype == VarType::Void
    {
        compiler
            .context
            .void_type()
            .fn_type(&param_types, is_var_args)
    } else {
        let ret_type = convert_to_type_enum(compiler, vvt_ret_type)?;
        ret_type.fn_type(&param_types, is_var_args)
    };

    // Extern functions might be declared in run-time errors
    // we need to check that first
    let fun = if let Some(fun) = compiler.module.get_function(&fn_name) {
        let line_one = format!(
            "The extern declaration of '{}' does not correspond with a previous existing one.",
            fn_name
        );
        let line_two = "This might be because the extern'd function is a libc defined function.";
        let line_three =
            "The compiler internally links to some libc functions for run-time error handling.";

        if fn_type != fun.get_type() {
            return Err(format!("{}\n{}\n{}", line_one, line_two, line_three));
        }

        fun
    } else {
        let fun = compiler
            .module
            .add_function(fn_name, fn_type, Some(Linkage::External));

        fun
    };

    declare_scoped_val(
        compiler,
        fn_name.to_string(),
        ScopedVal::Fn(ScopedFunc {
            ptr_val: fun,
            arg_types: extern_decl.arg_types,
            ret_type: extern_decl.ret_type,
        }),
    )?;

    Ok(())
}
