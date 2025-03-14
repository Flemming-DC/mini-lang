
use std::path::{Path, PathBuf};
use std::{fs, env};
use crate::{input_error, P};
use crate::tools::err::{CompilationError, InputError};


// ------- read Cmd Input ------- //

#[derive(Debug)]
pub struct Config {
    pub source_folder: String,
    pub exe_file: String,
    pub c_file: String,
    pub lib_name_exts: Vec<String>,
    pub lib_folder: String,
    pub test: bool,
    pub run: bool,
}

pub fn read_cmd_line() -> Result<Config, InputError> {
    let prog_and_args: Vec<String> = env::args().collect(); // Collect arguments into a vector

    let min_arg_count = 1;
    if prog_and_args.len() < 1 + min_arg_count {
        return input_error!("Expected at lest {min_arg_count} argument received {}. arguments", prog_and_args.len() - 1);
    }
    let project_folder = prog_and_args[1].clone();
    if &project_folder == "--test_compiler" || &project_folder == "--run" { return input_error!(
        r#"The first cmd input should be the project_folder. Received "{project_folder}"."#
    )};
    let lib_folder = project_folder.clone() + "/libraries";

    
    return Ok(Config {
        source_folder: project_folder.clone() + "/src",
        exe_file: project_folder.clone() + "/out/program.exe", 
        c_file: project_folder.clone() + "/out/intermediary.c", 
        lib_name_exts: get_lib_name_exts(&lib_folder)?,
        lib_folder: lib_folder, 
        test: prog_and_args[1..].contains(&"--test_compiler".to_string()),
        run: prog_and_args[1..].contains(&"--run".to_string()),
    });
}

fn get_lib_name_exts(lib_folder: &str) -> Result<Vec<String>, InputError> {
    fs::create_dir_all(lib_folder)?;
    let mut lib_name_exts: Vec<String> = Vec::new();

    for entry in fs::read_dir(lib_folder)? {
        let path = entry?.path();
        if path.is_dir() { continue; }
        let path_str = path.to_str()
            .ok_or(InputError { msg: format!("unreadable file name {:?}. Expected utf-8 compatible name.", path)})?
            .replace("\\", "/");
        
        let lib_name_ext = path_str.replacen(lib_folder, "", 1).replacen("/", "", 1);
        if lib_name_ext.contains('/') {return input_error!(
            "Library name and extension should not contain '/'. Found {lib_name_ext}" 
        )};
        if !lib_name_ext.contains('.') {return input_error!(
            "Expected `library_name.extension`, but there is no dot in {lib_name_ext}" 
        )};
        let ext = lib_name_ext.split(".").last().unwrap();
        let possible_exts = ["a", "lib", "so", "dylib", "dll"]; // repeated in parser
        if !possible_exts.contains(&ext) { 
            println!(r#"WARNING: {lib_name_ext} is in your libraries folder, but it is not recognized as a library. "
                     "Expected one of {:?} as the extension."#, possible_exts);
            continue;
        }

        lib_name_exts.push(lib_name_ext.to_string());
    }
    return Ok(lib_name_exts);
}


// ------- read source code ------- //

pub fn read_source_code(source_folder: &str) -> Result<String, InputError> {

    fs::create_dir_all(source_folder.to_string())?;
    let is_empty = fs::read_dir(source_folder)?.into_iter().count() == 0;
    if is_empty { return input_error!(
        r#"Expected some source code to be in "{source_folder}", but the folder is empty."#
    )}; 

    return read_source_code_rec(Path::new(source_folder));
}

fn read_source_code_rec(dir: &Path) -> Result<String, InputError> {
    // find sorted content of folder
    let mut entries: Vec<(String, PathBuf)> = Vec::new();
    for entry in fs::read_dir(dir)? {
        let path = entry?.path();
        let path_str = path.to_str()
            .ok_or(InputError { msg: format!("unreadable file name {:?}. Expected utf-8 compatible name.", path)})?;
        entries.push((path_str.to_string(), path));
    }
    entries.sort_by_key(|e| e.0.clone());
    entries.sort_by_key(|e| !e.1.is_dir());

    // read content in order
    let mut source = String::with_capacity(1024); // this number is a guess
    for (path_str, path) in entries {
        if path.is_dir() {
            // no block, since c doesn't allow it
            source += read_source_code_rec(&path)?.as_str();
        } else if path_str.ends_with(".mini") {
            source += fs::read_to_string(&path)?.as_str();
        }
    }

    return Ok(source);
}

