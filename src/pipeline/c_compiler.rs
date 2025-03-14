use std::{fs, path::Path, process::Command};
use crate::{tools::err, Fallible, P};

/// puts c_code into c_file and compiles it. Executable lands at program_path.
pub fn c_compile(
    c_code: String, c_file: &str, exe_file: &str, library_folder: &str, library_names: Vec<String>
    ) -> Result<(), err::CmdError> {
    // ready the file system
    let _ = fs::remove_file(exe_file); // we dont care about the error case
    fs::create_dir_all(Path::new(c_file).parent().expect(
        "Expected intermediate c file to be inside a directory."
    ))?; fs::create_dir_all(Path::new(exe_file).parent().expect(
        "Expected program executable to be inside a directory."
    ))?; fs::create_dir_all(Path::new(library_folder).parent().expect(
        "Expected library folder to be inside a directory."
    ))?;
    
    // do work
    let library_folder_flag = "-L".to_string() + library_folder;
    let mut args = vec![c_file.to_string(), "-o".to_string(), exe_file.to_string(), library_folder_flag];
    for lib in library_names {
        args.push("-l:".to_string() + lib.as_str());
    }
    fs::write(c_file, c_code)?; //? create if not exists, overwrite content
    let status = Command::new(
        "gcc").args(args).spawn()?.wait()?;
    return err::cmd_result(status);
}


pub fn run(program: &str) -> Fallible<()> { // args, kwargs should be granted if the language supports it
    Command::new(program).spawn()?.wait()?; // we ignore non-zero exit status, since it might not be a program error
    return Ok(());
}




