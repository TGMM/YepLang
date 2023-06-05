use super::helpers::CompilerError;
use lld_rs::{link, LldFlavor};

pub fn link_exe(exe_path: String, obj_path: String) -> Result<(), CompilerError> {
    let cmt_arg = "-defaultlib:libcmt".to_string();
    let out_arg = format!("-out:{}", exe_path);
    link(LldFlavor::Coff, &[obj_path, out_arg, cmt_arg]).ok()?;

    Ok(())
}
