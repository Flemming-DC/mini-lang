use crate::tools::err::*;
use crate::pipeline::Tokenizer;
use crate::pipeline::Token::*;


pub mod tests {
    use super::*;

    pub fn run() {
        println!("---- Tokenize ----");        
        set_valid_program_assertion(true);
        empty_string();
        literals();
        symbols();
        words();
        function();
        aritmetik();
        case_sensitive();
        decl_init();
        blocks();
        set_valid_program_assertion(false);
        error_on_unterminated_string_literal();
        error_on_unrecognized_token();
    }

    fn empty_string() {
        let source: String = "".into();
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![End];
        assert_eq!(actual, expected);
        println!("SUCCESS: empty_string");
    }

    fn literals() {
        let source: String = r#"132 12.1 12. true false "hello""#.into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            IntLit(132),
            FloatLit(12.1),
            FloatLit(12.0),
            BoolLit(true),
            BoolLit(false),
            StrLit("hello".into()),
            End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: literals");
    }

    fn symbols() {
        let source: String = "+-*/**()>><<=:!>< >= <= != == := ".into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            Add, Minus, Mul, Div, Pow, ParenL, ParenR, PtrIn, PtrOut, AssignOp, Colon, Not, Gt, Lt,
            Gte, Lte, Neq, Eq, InitOp, End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: symbols");
    }
    
    fn words() {
        let source: String = "bool int float str fn hejsa Bool goddag and or".into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            BoolTy, IntTy, FloatTy, StrTy, Fn,
            Ident("hejsa".into()), Ident("Bool".into()), Ident("goddag".into()),
            And, Or,
            End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: words");
    }

    fn function() {
        let source: String = "fn foo(x int) bool: x > 0".into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            Fn, Ident("foo".into()), ParenL, Ident("x".into()), IntTy, ParenR, BoolTy, Colon, 
            Ident("x".into()), Gt, IntLit(0),
            End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: function");
    }

    fn decl_init() {
        let source: String = "Y float := 3.".into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            Ident("Y".into()), FloatTy, InitOp, FloatLit(3.0), 
            End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: decl_init");
    }
    
    fn aritmetik() {
        let source: String = "-2+3 + 5.*4**2.0".into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            Minus, IntLit(2), Add, IntLit(3), Add, FloatLit(5.0), Mul, IntLit(4), Pow, FloatLit(2.0),
            End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: aritmetik");
    }

    fn case_sensitive() {
        let source: String = "y Float := 3.".into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            Ident("y".into()), Ident("Float".into()), InitOp, FloatLit(3.0), 
            End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: case_sensitive");
    }

    fn blocks() {
        let source: String = "bib; { 2 3 }".into(); 
        let actual = Tokenizer::tokenize(source).unwrap();
        let expected= vec![
            Ident("bib".into()),
            StmtSep, 
            BlockStart, 
            IntLit(2), 
            IntLit(3), 
            BlockEnd,
            End,
        ];
        assert_eq!(actual, expected);
        println!("SUCCESS: blocks");
    }

    fn error_on_unterminated_string_literal() {
        let source: String = r#"fn foo(x int) str: "hejsa"#.into(); 
        let is_err = Tokenizer::tokenize(source).is_err();
        assert!(is_err);
        println!("SUCCESS: error_on_unterminated_string_literal");
    }

    fn error_on_unrecognized_token() {
        let source: String = r#"int float hejsa ¤§½"#.into(); 
        let is_err = Tokenizer::tokenize(source).is_err();
        assert!(is_err);
        println!("SUCCESS: error_on_unrecognized_token");
    }


}

