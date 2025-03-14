use crate::tools::err::*;
use crate::pipeline::*;
use super::test_tools;


pub mod tests {
    use crate::{func_log, func_name};
    use super::*;

    pub fn run() {
        println!("---- C_Compiler ----");
        single_number();
        binary();
    }

    fn single_number() {
        let source = r#"
        main fn foo() int { 1; };
        "#;
        get_expected(func_log!(source), source, false);
    }

    fn binary() {
        let source = r#"
        main fn foo() int { 1 + 2 > 2 and !true; };
        "#;
        get_expected(func_log!(source), source, false);
    }
    
    

    // --------- helpers --------- //
    fn get_expected(test_name: &str, source: &str, use_log: bool) {
        let mut parser = Parser::from(Tokenizer::from(source.into()), Vec::new()).unwrap();
        parser.use_log = use_log;
        let (ast, libs) = parser.parse().unwrap();

        let project_folder = "C:/Mine/Rust/mini-lang/demo-project".to_string();
        let c_file = project_folder.clone() + "/out/intermediary.c";
        let exe_file = project_folder.clone() + "/out/program.exe";
        let lib_folder = project_folder.clone() + "libraries";

        set_valid_program_assertion(false);
        let types = match type_checker::check_ast(&ast, false) {
            Ok(pt) => pt,
            Err(_) => Vec::new(), // ad hoc hack
        }; 
        set_valid_program_assertion(true);
        let c_code = c_code_gen::generate_c_code(ast, types);
        c_compiler::c_compile(c_code, &c_file, &exe_file, &lib_folder, libs).unwrap();
        c_compiler::run(&exe_file).unwrap();

        println!("SUCCESS: {}", test_tools::into_oneliner(test_name));
    }
    
    // fn get_error(test_name: &str, source: &str, use_log: bool) {
    //     let mut parser = Parser::from(Tokenizer::from(source.into())).unwrap();
    //     parser.use_log = use_log;

    //     let is_err = parser.parse().is_err();

    //     assert!(is_err, "{}", test_name);
    //     println!("SUCCESS: {}", test_name);
    // }


}


