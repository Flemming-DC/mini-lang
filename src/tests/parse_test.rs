use crate::tools::err::*;
use crate::pipeline::*;
use crate::pipeline::Token::*;
use crate::pipeline::ast::Ast::*;
use super::test_tools;

pub mod tests {
    use crate::{func_log, func_name};

    use super::*;

    pub fn run() {
        println!("---- Parse ----");
        // ---- expr ---- //
        single_number();
        unary();
        binary();
        call();
        parse_paren();
        // ---- stmt ---- //
        parse_fn_decl();
        parse_init();
        parse_assign();
        parse_main();
        stmt_sep();

    }

    // empty_string


    fn single_number() {
        let source = "1";
        let expected = Terminal(IntLit(1));
        get_expected(func_log!(source), source, expected, false);
    }

    fn unary() {
        let source = "!true";
        let expected = UnaryExpr { op: Not, arg: bool_lit(true) };
        get_expected(func_log!(source), source, expected, false);

        let source = "-5";
        let expected = UnaryExpr { op: Minus, arg: int_lit(5) };
        get_expected(func_log!(source), source, expected, false);

        let source = "<<-a>>";
        let expected = UnaryExpr { 
            op: PtrOut, arg: Box::new(UnaryExpr { 
                op: Minus, arg: Box::new(UnaryExpr { 
                    op: PtrOut, arg: ident("a") 
        })})};
        get_expected(func_log!(source), source, expected, false);

        let source = "!>>5";
        let expected = UnaryExpr { 
            op: Not, arg: Box::new(UnaryExpr { 
                op: PtrIn, arg: int_lit(5) 
        })};
        get_expected(func_log!(source), source, expected, false);

        // let source = "!1"; // type checking is pushed to semantic analysis. so it won't be caught yet.
        // get_error(func_log!(source), source, false);
    }

    fn binary() {
        // let source = "1+2";
        // let expected = BinaryExpr { op: Add, left: int_lit(1), right: int_lit(2) };
        // get_expected(func_log!(source), source, expected, false);

        let source = "-1 + 2 * 3.0 + 4";
        let expected = BinaryExpr { // (-1 + (2 * 3.0)) + 4
            op: Add, 
            left: Box::new(BinaryExpr { // -1 + (2 * 3.0)
                op: Add, 
                left: Box::new(UnaryExpr { op: Minus, arg: int_lit(1) }),
                right: Box::new(BinaryExpr { // 2 * 3.0
                    op: Mul, 
                    left: int_lit(2), 
                    right: float_lit(3.0) }), 
            }), 
            right: int_lit(4), 
        };
        get_expected(func_log!(source), source, expected, false);

        let source = "3.0**2 + 2 > 3.0 + 4";
        let expected = BinaryExpr { // ((3.0^2) + 2) > (3.0 + 4)
            op: Gt, 
            left: Box::new(BinaryExpr { // (3.0^2) + 2
                op: Add, 
                left: Box::new(BinaryExpr { // 3.0^2
                    op: Pow, 
                    left: float_lit(3.0), 
                    right: int_lit(2) }),
                right: int_lit(2), 
            }), 
            right: Box::new(BinaryExpr { // 3.0 + 4
                op: Add, 
                left: float_lit(3.0), 
                right: int_lit(4) }),
        };
        get_expected(func_log!(source), source, expected, false);

        let source = "2 > 1.5 and true or false";
        let expected = BinaryExpr { // ((2 > 1.5) and true) or false
            op: Or, 
            left: Box::new(BinaryExpr { // (2 > 1.5) and true
                op: And, 
                left: Box::new(BinaryExpr { // 2 > 1.5
                    op: Gt, 
                    left: int_lit(2), 
                    right: float_lit(1.5) }),
                right: bool_lit(true), 
            }), 
            right: bool_lit(false),
        };
        get_expected(func_log!(source), source, expected, false);

    }

    fn call() {
        let source = "foo(hejsa)";
        let expected = Call { func_name: "foo".into(), args: vec![ident("hejsa")] };
        get_expected(func_log!(source), source, expected, false);

        let source = "bar(hejsa, 3, 1+1)";
        let expected = Call { func_name: "bar".into(), args: vec![
            ident("hejsa"),
            int_lit(3),
            Box::new(BinaryExpr { op: Add, left: int_lit(1), right: int_lit(1) }),
        ] };
        get_expected(func_log!(source), source, expected, false);

        let source = r"baz(
            bib + 3, 
            !true and false, 
            1+1**3,
        )";
        let expected = Call { func_name: "baz".into(), args: vec![
            Box::new(BinaryExpr { op: Add, left: ident("bib"), right: int_lit(3) }),
            Box::new(BinaryExpr { 
                op: And, 
                left: Box::new(UnaryExpr { op: Not, arg: bool_lit(true) }), 
                right: bool_lit(false), 
            }),
            Box::new(BinaryExpr { 
                op: Add, 
                left: int_lit(1), 
                right: Box::new(BinaryExpr { op: Pow, left: int_lit(1), right: int_lit(3) }), 
            }),
        ]};
        get_expected(func_log!(source), source, expected, false);

    }
    
    fn parse_paren() {
        let source = "(!hejsa)";
        let expected = UnaryExpr { op: Not, arg: ident("hejsa") };
        get_expected(func_log!(source), source, expected, false);
        
    }

        
    fn parse_fn_decl() {
        let source = "fn foo(hejsa: str) bool { true and hejsa; }";
        let expected = FnDecl { 
            name: "foo".into(), 
            args: vec![
                Box::new(Decl { name: "hejsa".into(), type_: StrTy.into() }),
            ], 
            ret_type: BoolTy.into(), 
            body: Box::new(Block { stmts: vec![
                Box::new(BinaryExpr { op: And, left: bool_lit(true), right: ident("hejsa") })
            ]}), 
        };
        get_expected(func_log!(source), source, expected, false);
    }
    
    fn parse_init() {
        let source = "vip: int := 2";
        let expected = Init { 
            decl: Box::new(Decl { name: "vip".into(), type_: IntTy.into() }), 
            value: int_lit(2),
        };
        get_expected(func_log!(source), source, expected, false);
    }
    
    fn parse_assign() {
        let source = "bob = true";
        let expected = Assign { var_name: "bob".into(), value: bool_lit(true) };
        get_expected(func_log!(source), source, expected, false);
    }

        
    fn parse_main() {
        let source = r#"
        fn bar(hejsa: str) bool { true; }
        main fn foo() int { bar("hello"); }
        "#;
        let expected = Script {stmts: vec![
            Box::new(FnDecl { 
                name: "bar".into(), 
                args: vec![ Box::new(Decl { name: "hejsa".into(), type_: StrTy.into() }) ], 
                ret_type: BoolTy.into(), 
                body: Box::new(Block { stmts: vec![bool_lit(true)]}),
            }),
            Box::new(FnDecl { 
                name: "foo".into(), 
                args: vec![], 
                ret_type: IntTy.into(), 
                body: Box::new(Block { stmts: vec![
                    Box::new(Call { func_name: "bar".into(), args: vec![str_lit("hello")] })
                ]})
            }),
        ], 
        main: Some("foo".into()) 
        };
        get_expected_script(func_log!(source), source, expected, false);
    }


    fn stmt_sep() {
        let source = ";";
        let expected = Script { stmts: vec![], main: None };
        get_expected_script(func_log!(source), source, expected, false);
    }



    // --------- helpers --------- //
    fn get_expected(test_name: &str, source: &str, expected: Ast, use_log: bool) {
        set_valid_program_assertion(true);
        let source = source.to_string() + (if expected.is_expr() {";"} else {""});

        let mut parser = Parser::from(Tokenizer::from(source), Vec::new()).unwrap();
        parser.use_log = use_log;

        let (actual_, _) = parser.parse().unwrap();
        let Script { stmts, main } = actual_ else {unreachable!()};
        let actual = *stmts[0].clone();

        assert_eq!(actual, expected, "{}", test_name);
        println!("SUCCESS: {}", test_tools::into_oneliner(test_name));
    }

    fn get_expected_script(test_name: &str, source: &str, expected: Ast, use_log: bool) {
        set_valid_program_assertion(true);
        let mut parser = Parser::from(Tokenizer::from(source.into()), Vec::new()).unwrap();
        parser.use_log = use_log;

        let (actual, _) = parser.parse().unwrap();

        assert_eq!(actual, expected, "{}", test_name);
        println!("SUCCESS: {}", test_tools::into_oneliner(test_name));
    }
    
    // fn get_error(test_name: &str, source: &str, use_log: bool) {
    //     set_valid_program_assertion(false);
    //     let mut parser = Parser::from(Tokenizer::from(source.into())).unwrap();
    //     parser.use_log = use_log;

    //     let is_err = parser.parse().is_err();

    //     assert!(is_err, "{}", test_name);
    //     println!("SUCCESS: {}", test_name);
    // }


    // --------- wrappers --------- //
    fn bool_lit(x: bool) -> Box<Ast> { Box::new(Terminal(BoolLit(x))) }
    fn int_lit(x: i128)  -> Box<Ast> { Box::new(Terminal(IntLit(x))) }
    fn float_lit(x: f64) -> Box<Ast> { Box::new(Terminal(FloatLit(x))) }
    fn str_lit(x: &str)  -> Box<Ast> { Box::new(Terminal(StrLit(x.to_string()))) }
    fn ident(x: &str)    -> Box<Ast> { Box::new(Terminal(Ident(x.to_string()))) }
     
}