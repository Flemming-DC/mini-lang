use crate::tools::err::*;
use crate::pipeline::*;
use super::test_tools;
use crate::{brk_if, func_log, func_name, P};

pub mod tests {
    use super::*;

    pub fn run() {
        println!("---- Type Check ----");
        single_number();
        unary();
        binary();
        var();
        print_();
        array();
        if_else();
        while_();
        for_();
        index();
        skip();
        func();
        // import();
        // extern_();
        struct_();
    }

    fn single_number() {
        let source = "1;";
        is_valid(func_log!(source), &source, true);

        let source = "a;";
        is_invalid(func_log!(source), &source, false);
    }

    fn unary() {
        let source = "-1;";
        is_valid(func_log!(source), &source, false);
        let source = "!false;";
        is_valid(func_log!(source), &source, false);
        let source = ">>1.0;";
        is_valid(func_log!(source), &source, false);
        let source = ">>nil;";
        is_valid(func_log!(source), &source, false);

        let source = "-true;";
        is_invalid(func_log!(source), &source, false);
        let source = "!1.0;";
        is_invalid(func_log!(source), &source, false);
    }

    fn binary() {
        let source = "4 - 1;";
        is_valid(func_log!(source), &source, false);
        let source = "3.1 * 1;";
        is_valid(func_log!(source), &source, false);
        let source = "1 > 3 and !false;";
        is_valid(func_log!(source), &source, false);

        let source = "1.4 > 1.3 + !false;";
        is_invalid(func_log!(source), &source, false);
        let source = "(1+3) and (true == !false);";
        is_invalid(func_log!(source), &source, false);
    }

    fn var() {
        let source = "x: int := -1;";
        is_valid(func_log!(source), &source, false);
        let source = "x: bool := !false;";
        is_valid(func_log!(source), &source, false);
        let source = "x: >>float := >>1.0;";
        is_valid(func_log!(source), &source, false);
        // let source = "x: >>nil := >>nil;";
        // is_valid(func_log!(source), &source, false);

        let source = "x: int := -1.5;";
        is_invalid(func_log!(source), &source, false);
    }

    fn print_() {
        let source = "P 4 - P 1 + P(1 + 2);";
        is_valid(func_log!(source), &source, false);
    }

    fn array() {
        let source = "[1, 3, 2];";
        is_valid(func_log!(source), &source, false);

        let source = r#"["1", "a"];"#;
        is_valid(func_log!(source), &source, false);

        let source = "[true, 3, 2.0];";
        is_invalid(func_log!(source), &source, false);
    }

    fn if_else() {
        let decl = "a: int := 1   ".to_string();

        let source = decl.clone() + "a = if true do 5 else 3;";
        is_valid(func_log!(source), &source, false);
        
        // let source = decl.clone() + "a = if true do 5; else 3.0;";
        // is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "if a != 1 do a = a + 1 else a = a - 1";
        is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "a = if true do 5 else true;";
        is_invalid(func_log!(source), &source, false);

        let source = decl.clone() + "if a != 1 do 3 else a = a - 1";
        is_invalid(func_log!(source), &source, false);

        let source = decl.clone() + "if a != 1 do a = a + 1 else 4;";
        is_invalid(func_log!(source), &source, false);
    }

    fn while_() {
        let decl = "a: int := 1   ".to_string();

        let source = decl.clone() + "
        while true do a = 3;";
        is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "
        while 1 do a = 3;";
        is_invalid(func_log!(source), &source, false);
    }

    fn for_() {
        let decl = "arr: []int := [1, 2]   ".to_string();

        let source = decl.clone() + "for a in arr do a;";
        is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "for a in arr do a; P a";
        is_invalid(func_log!(source), &source, false);
    }

    fn index() {
        let decl = "arr: []int := [1, 2]   b: int := 1   ".to_string();

        let source = decl.clone() + "c: int := arr[2];";
        is_valid(func_log!(source), &source, false);
        let source = decl.clone() + "c: int := arr[b];";
        is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "arr[2] = 1;";
        is_valid(func_log!(source), &source, false);
        let source = decl.clone() + "arr[b] = 2;";
        is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "c: int := arr[true]";
        is_invalid(func_log!(source), &source, false);
        let source = decl.clone() + "c: int := b[2]";
        is_invalid(func_log!(source), &source, false);

        let source = decl.clone() + "arr[true] = 3";
        is_invalid(func_log!(source), &source, false);
        let source = decl.clone() + "b[2] = 4";
        is_invalid(func_log!(source), &source, false);
    }

    fn skip() {
        let decl = "a: int := 1   ".to_string();

        let source = decl.clone() + "
        while true do {
            if a > 1 do break 
            a = 3
        };";
        is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "
        while true do {
            if a > 1 do continue
            a = 3
        };";
        is_valid(func_log!(source), &source, false);

        let source = decl.clone() + "
        if a > 1 do continue;";
        is_invalid(func_log!(source), &source, false);
    }

    fn func() {
        let source = "
        fn gt(x: int, y: int) bool {
            return x > y
        }
        b: bool := gt(2, 1)
        ";
        is_valid(func_log!(source), &source, false);

        let source = "
        fn print(x: int, y: int) {
            P (x + y);
        }
        print(2, 1);
        ";
        is_valid(func_log!(source), &source, false);

        let source = "
        fn sure() bool {
            return true
        }
        sure();
        ";
        is_valid(func_log!(source), &source, false);

        let source = "
        a: int := 3
        return true
        ";
        is_invalid(func_log!(source), &source, false);
    }


    fn struct_() {
        let source = "
        struct Vector2 {x: float, y: float}
        v: Vector2 := Vector2 {x: 1.0, y: 1.0}
        ";
        is_valid(func_log!(source), &source, false);

        let source = "
        struct LinkList {next: >>LinkList, data: float}
        ll: LinkList := LinkList {next: as(>>LinkList) nil, data: 1.0} # suffix nil pointer by cast
        
        ll0: LinkList := LinkList {next: as(>>LinkList) nil, data: 0.0}
        ll1: LinkList := LinkList {next: >>ll0, data: 1.0}
        ll2: LinkList := LinkList {next: >>ll1, data: 2.0}
        ";
        is_valid(func_log!(source), &source, false);

        // let source = "
        // struct Vector2 {x: float, y: float}
        // v: Vector2 := {x: 1.0, y: 1.0}
        // ";
        // is_invalid(func_log!(source), &source, false);
    }

    
    

    // --------- helpers --------- //
    fn is_valid(test_name: &str, source: &str, use_log: bool) {
        set_valid_program_assertion(true);
        // let source = "main fn foo() {}".to_string() + source;
        let mut parser = Parser::from(Tokenizer::from(source.to_string()), Vec::new()).unwrap();
        parser.use_log = use_log;

        let (ast, _) = parser.parse().unwrap();
        type_checker::check_ast(&ast, false).unwrap(); 

        println!("SUCCESS: {}", test_tools::into_oneliner(test_name));
    }

    fn is_invalid(test_name: &str, source: &str, use_log: bool) {
        set_valid_program_assertion(false);
        let mut parser = Parser::from(Tokenizer::from(source.into()), Vec::new()).unwrap();
        parser.use_log = use_log;

        let is_err = match parser.parse() {
            Ok((ast, _)) => type_checker::check_ast(&ast, false).is_err(),
            Err(_) => true,
        };
        assert!(is_err, "{}", test_name);

        println!("SUCCESS: {}", test_tools::into_oneliner(test_name));
    }


}