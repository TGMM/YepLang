use std::path::Path;

use super::helpers::CompilerError;
use lld_rs::{link, LldFlavor};

pub fn link_exe(
    exe_path: String,
    obj_path: String,
    obj_files: Option<Vec<String>>,
) -> Result<(), CompilerError> {
    let obj_files = obj_files.unwrap_or(vec![]);
    let mut obj_files = obj_files
        .into_iter()
        .filter(|obj| {
            let is_file = Path::new(obj.as_str()).is_file();
            if !is_file {
                println!("Warning: \"{}\" was not a valid file, skipping.", obj);
            }

            is_file
        })
        .collect::<Vec<_>>();

    let cmt_arg = "-defaultlib:libcmt".to_string();
    let out_arg = format!("-out:{}", exe_path);

    obj_files.extend([obj_path, out_arg, cmt_arg]);
    link(LldFlavor::Coff, &obj_files).ok()?;

    Ok(())
}
