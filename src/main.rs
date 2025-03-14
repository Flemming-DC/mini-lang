// #![allow(dead_code, unused_imports, unused_variables, unused_parens, non_upper_case_globals)]
#![allow(unused_variables, unused_imports, unused_parens, non_upper_case_globals)]
mod pipeline;
mod tools;
mod tests;
use std::path::Path;
use tools::err::*;
use pipeline::*;



fn main() { if let Err(e) = main_() { eprintln!("{}", e); } }

fn main_() -> Result<(), CompilationError> {
    let config = input_reader::read_cmd_line()?;

    if config.test { test(); return Ok(()); }
    let source = input_reader::read_source_code(&config.source_folder)?; 
    compile_script(source, config)?; 

    return Ok(());
}


fn compile_script(source: String, config: Config) -> Result<(), CompilationError> {
    let mut parser = Parser::from(Tokenizer::from(source.into()), config.lib_name_exts.clone())?;
    let (ast, libs) = parser.parse()?;
    let types = type_checker::check_ast(&ast, true)?;
    let c_code = c_code_gen::generate_c_code(ast, types);
    
    c_compiler::c_compile(c_code, &config.c_file, &config.exe_file, &config.lib_folder, libs)?;
    if config.run { c_compiler::run(&config.exe_file)?; }
    Ok(())
}

fn test() {
    println!("\n------------------ BEGIN TEST ------------------ ");
    use tests::*;
    tok_test::tests::run();
    parse_test::tests::run();
    type_checker_test::tests::run();
    c_code_gen_test::tests::run();
    c_compiler_test::tests::run();
    println!("\n------------------ END TEST ------------------ ");
}
