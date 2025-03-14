use crate::tools::err::*;
use crate::pipeline::*;
use super::test_tools;
use crate::{brk_if, func_log, func_name, P};

pub mod tests {
    use super::*;

    pub fn run() {
        println!("---- C Code Gen ----");
        empty_string();
        single_number();
        unary();
        binary();
        main_();
        print_(); 
        array();
        if_else();
        while_();
        for_();
        index();
        skip();
        nil();
        // import();
        extern_();
        struct_();
        comment();
    }

    fn empty_string() {
        let source = "";
        let expected = "";
        get_expected(func_log!(source), source, expected, false);
    }

    fn single_number() {
        let source = "1;";
        let expected = "1;";
        get_expected(func_log!(source), &source, expected, false);
    }

    fn unary() {
        let source = "!true;";
        let expected = "!1;";
        get_expected(func_log!(source), source, expected, false);
    }

    fn binary() {
        let source = "1 + 2 > 2 and !true;";
        let expected = "1 + 2 > 2 && !1;";
        get_expected(func_log!(source), source, expected, false);

        let source = "2 ** 3;";
        let expected = "pow(2, 3);";
        get_expected(func_log!(source), source, expected, false);
    }
    
    fn main_() {
        // this should fail the type checker
        let source = r#"
        fn bar(hejsa: float) bool { return true }
        main fn foo() int { return bar(2.0) }
        "#;
        let expected = r#"
        int8_t bar (float hejsa) { return 1; }
        int32_t foo () { return bar(2.0f); }

        int main(int argc, char *argv[]) {
            foo();
            return 0;
        }"#;
        get_expected(func_log!(source), source, expected, false);
    }

    fn print_() {
        let source = "1 + P 2;";
        let expected = r#"1 + printf("%d\n", 2);"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = "P(2 + 1);";
        let expected = r#"printf("%d\n", 2 + 1);"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = "1.0 + P 1.0;";
        let expected = r#"1.0f + printf("%f\n", 1.0f);"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = r#"P true;"#;
        let expected = r#"printf("%d\n", 1);"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = r#"P "1.0";"#;
        let expected = r#"printf("%s\n", c_str(make_char_array(3, "1.0")));"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = r#"a: >>int := >>1; P a;"#;
        let expected = r#"int32_t* a = &1; printf(">>%d\n", *a);"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = r#"a: >>int := >>1; P <<a;"#;
        let expected = r#"int32_t* a = &1; printf("%d\n", *a);"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = r#"fn foo(x: int) int {return x}; P foo;"#;
        let expected = r#"int32_t foo(int32_t x) {return x;} printf("fn (int) int\n");"#; 
        type_ok_get_expected(func_log!(source), source, expected, false);

        // array omitted
    }

    fn array() {

        fn array_type(type_str: &str) -> String {format!(r#"
typedef struct {{ 
    {type_str}* ptr; 
    size_t len; 
}} {type_str}_array;

{type_str}_array make_{type_str}_array(size_t len, {type_str}* initializer) {{
    {type_str}_array arr;
    arr.ptr = ({type_str}*) malloc(len * sizeof({type_str}));
    if (!arr.ptr) 
        Crash("Failed to allocate array.");
    arr.len = len;

    for (size_t i = 0; i < len; ++i) {{
        arr.ptr[i] = initializer[i];
    }}

    return arr;
}}
        "#)}

        let source = "arr: []int := [1, 2, 3];";
        let expected = "int32_t_array arr = make_int32_t_array(3, (int32_t[]){ 1, 2, 3 });"; 
        let expected = array_type("int32_t") + expected;
        get_expected(func_log!(source), source, expected.as_str(), false);

        let source = "arr: []float := [1.0, 2.0, 3.0];";
        let expected = "float_array arr = make_float_array(3, (float[]){ 1.0f, 2.0f, 3.0f });"; 
        let expected = array_type("float") + expected;
        get_expected(func_log!(source), source, expected.as_str(), false);

        let source = "arr: [][]float := [[1.0, 2.0], [3.0, 4.0]];";
        let expected = r#"
        float_array_array arr = make_float_array_array(2, (float_array[]){ 
            make_float_array(2, (float[]){ 1.0f, 2.0f }),
            make_float_array(2, (float[]){ 3.0f, 4.0f })
        });"#; 
        let expected = array_type("float") + array_type("float_array").as_str() + expected;
        get_expected(func_log!(source), source, expected.as_str(), false);

        let source = r#"s: str := "hejsa";"#;
        let expected = r#"char_array s = make_char_array(5, "hejsa");"#; 
        let expected = array_type("char") + expected;
        get_expected(func_log!(source), source, expected.as_str(), false);

    }

    fn if_else() {
        // stmt
        let source = "if x > 1 do y = y + 1 else y = y - 1";
        let expected = "if (x > 1) {y = y + 1;} else {y = y - 1;}"; 
        get_expected(func_log!(source), source, expected, false);

        let source = "if x > 1 do y = y + 1";
        let expected = "if (x > 1) {y = y + 1;}"; 
        get_expected(func_log!(source), source, expected, false);

        let source = "if x > 1 do y = y + 1 else if x < -1 do y = 2 else y = y - 1";
        let expected = "if (x > 1) {y = y + 1;} else {if (x < -1) {y = 2;} else {y = y - 1;}}"; 
        get_expected(func_log!(source), source, expected, false);

        // expr
        let source = "a = if x > 1 do 5 else 3;";
        let expected = "a = x > 1 ? 5 : 3;"; 
        get_expected(func_log!(source), source, expected, false);

        // if_else expr can't have a missing else part

        let source = "a = if x > 1 do 5 else if x < -1 do 4 else 3;";
        let expected = "a = x > 1 ? 5 : x < -1 ? 4 : 3;"; 
        get_expected(func_log!(source), source, expected, false);

    }

    fn while_() {
        let source = "while x > -1 do y = y + 1;";
        let expected = "while (x > -1) {y = y + 1;};"; 
        get_expected(func_log!(source), source, expected, false);
    }

    fn for_() {
        let source = "for a in arr do a = 1.0;";
        let expected = r#"for (size_t i = 0; i < arr.len; ++i) {typeof(mem.ptr[i]) a = arr.ptr[i]; a = 1.0f;}"#; 
        get_expected(func_log!(source), source, expected, false);

        let source = "for a, i_ in arr do a = 1.0;";
        let expected = r#"for (size_t i_ = 0; i_ < arr.len; ++i_) {typeof(mem.ptr[i]) a = arr.ptr[i_]; a = 1.0f;}"#; 
        get_expected(func_log!(source), source, expected, false);
    }

    fn index() {
        // get
        let source = "a = arr[2]";
        let expected = "a = arr.ptr[2];";
        // let expected = r#"
        //     a = (2 < 0 || 2 >= arr.len) 
        //         ? Crash("Index out of bounds") 
        //         : arr.ptr[2];
        // "#;
        get_expected(func_log!(source), source, expected, false);

        // set
        let source = "arr[j] = 3;";
        let expected = "arr.ptr[j] = 3;";
        // let expected = r#"
        //     if (j < 0 || j >= arr.len) Crash("Index out of bounds"); 
        //     arr.ptr[j] = 3;
        // "#; 
        get_expected(func_log!(source), source, expected, false);
    }

    fn skip() {
        let source = r#"
            fn foo() int {
                if true do return 3 
                else return 2
            }"#;
        let expected = r#"
            int32_t foo () {
                if (1) {return 3;} 
                else {return 2;}
            }"#;
        get_expected(func_log!(source), source, expected, false);

        let source = "while true do if x > 2 do break;"; 
        let expected = "while (1) {if (x > 2) {break}};"; 
        get_expected(func_log!(source), source, expected, false);

        let source = "while true do if x > 2 do continue;";
        let expected = "while (1) {if (x > 2) {continue}};"; 
        get_expected(func_log!(source), source, expected, false);
    }

    fn nil() {
        let source = r#"a: >>A := as(>>A) nil;"#;
        let expected = r#"struct A* a = (struct A*) NULL;"#;
        get_expected(func_log!(source), source, expected, false);

        let source = r#"a = if ptr != nil do ptr>> else 0;"#;
        let expected = r#"a = ptr != NULL ? *ptr : 0;"#;
        get_expected(func_log!(source), source, expected, false);
    }

    // import
    
    fn extern_() {
        let source = r#"extern x: float;"#;
        let expected = r#"extern float x;"#;
        get_expected(func_log!(source), source, expected, false);

        let source = r#"extern fn foo(x: int) int;"#;
        let expected = r#"extern int32_t foo(int32_t x);"#;
        get_expected(func_log!(source), source, expected, false);
    }

    fn struct_() {
        let source = "struct A {x: int, y: float}";
        let expected = "struct A { int32_t x; float y; };";
        type_ok_get_expected(func_log!(source), source, expected, false);

        let source = "a: A := A {x: 1, y: 2.0}";
        let expected = "struct A a = (struct A){.x = 1, .y = 2.0f};";
        get_expected(func_log!(source), source, expected, false);

        let source = "a.x = 1";
        let expected = "a.x = 1;";
        get_expected(func_log!(source), source, expected, false);

        let source = "b: int := a.x";
        let expected = "int32_t b = a.x;";
        get_expected(func_log!(source), source, expected, false);

        let source = "
        struct LinkList_2 {next: >>LinkList_2, data: float}
        struct LinkList {next: >>LinkList, data: float}
        
        ll: LinkList := LinkList {next: as(>>LinkList) nil, data: 0.0}
        P ll;
        ";
        let expected = r#"
        struct LinkList_2 { struct LinkList_2* next; float data; };
        struct LinkList { struct LinkList* next; float data; };
        
        struct LinkList ll = (struct LinkList){.next = (struct LinkList*) NULL, .data = 0.0f};
        printf("LinkList {");printf("}\n");
        "#;
        get_expected(func_log!(source), source, expected, false);
    }

    fn comment() {
        let source = "
        a: int := 1#00
        # hejsa
        a = 2
        ";
        let expected = "
        int32_t a = 1;

        a = 2;
        ";
        type_ok_get_expected(func_log!(source), source, expected, false);
    }

    // --------- helpers --------- //
    fn type_ok_get_expected(test_name: &str, source: &str, expected: &str, use_log: bool) {
        set_valid_program_assertion(true);
        // let source = "main fn fake_main() {}".to_string() + source;
        // let expected = format!(
        //     r#"void fake_main () {{ }} {expected} int main (int argc, char *argv[]) {{ fake_main () ; return 0 ; }}"#
        // );

        let mut parser = Parser::from(Tokenizer::from(source.to_string()), Vec::new()).unwrap();
        parser.use_log = use_log;

        let (ast, _) = parser.parse().unwrap();
        // we dont report the check violations, since the code fragments are just fragments 
        // and may therefore have undeclared stuff.
        let types = type_checker::check_ast(&ast, false).unwrap();
        let actual_raw = c_code_gen::generate_c_code(ast, types);

        let prelude = "// ---------- PRELUDE ---------- //".to_string() + c_code_gen::prelude;
        let actual = match actual_raw.trim_start().strip_prefix(prelude.trim_start()) {
            None => {println!("No match for prelude!"); actual_raw},
            Some(after_prefix) => after_prefix.to_owned(),
        };
        assert_eq!(normalize_whitespace(&actual), normalize_whitespace(&expected), "{}", test_name);
        println!("SUCCESS: {}", test_tools::into_oneliner(test_name));
    }

    fn get_expected(test_name: &str, source: &str, expected: &str, use_log: bool) {
        set_valid_program_assertion(true);
        let mut parser = Parser::from(Tokenizer::from(source.into()), Vec::new()).unwrap();
        parser.use_log = use_log;

        let (ast, _) = parser.parse().unwrap();
        // we dont report the check violations, since the code fragments are just fragments 
        // and may therefore have undeclared stuff. 
        set_valid_program_assertion(false);
        let types = match type_checker::check_ast(&ast, false) {
            Ok(pt) => pt,
            Err(_) => Vec::new(), // ad hoc hack
        }; 
        set_valid_program_assertion(true);
        let actual_raw = c_code_gen::generate_c_code(ast, types);

        let prelude = "// ---------- PRELUDE ---------- //".to_string() + c_code_gen::prelude;
        let actual = match actual_raw.trim_start().strip_prefix(prelude.trim_start()) {
            None => {println!("No match for prelude!"); actual_raw},
            Some(after_prefix) => after_prefix.to_owned(),
        };
        assert_eq!(normalize_whitespace(&actual), normalize_whitespace(expected), "{}", test_name);
        println!("SUCCESS: {}", test_tools::into_oneliner(test_name));
    }

    fn normalize_whitespace(code: &str) -> String {
        if code.is_empty() { return code.to_string(); }

        let code = code
            .replace("// ---------- PRELUDE ---------- //", "")
            .replace("// -------- ARRAY_TYPES -------- //", "")
            .replace("// ----------- SCRIPT ----------- //", "")
        ;

        // add single blanks, when desired, and reduce ;; to ;
        let mut out_code = String::new();
        for i in 0..code.len() {
            let c1 = code.chars().nth(i).unwrap();
            let c2_opt = code.chars().nth(i + 1); 
            if c2_opt == None {
                // last element
                out_code += c1.to_string().as_str();
                break;
            }
            let c2 = c2_opt.unwrap();

            if c1 == ';' && c2 == ';' {
                continue;
            }
            out_code += c1.to_string().as_str();

            // let w1 = c1.is_alphanumeric() || c1 == '_';
            // let w2 = c2.is_alphanumeric() || c1 == '_';
            if c1 == '{' || c2 == '}' || c2 == ';' ||  c2 == '(' {
                out_code += " "; 
            }
        }

        // remove excess whitespace e.g. "abc  d" becomes "abc d"
        let out_code = out_code.split_whitespace().collect::<Vec<_>>().join(" ");

        return out_code;
    }
    
    // fn get_error(test_name: &str, source: &str, use_log: bool) { }

}





