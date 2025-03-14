use indexmap::IndexMap;
use crate::{dbg_assert, P};
use super::{Ast, Token, Type};
use super::Ast::*;


pub fn generate_c_code(ast: Ast, types: Vec<(Ast, Type)>) -> String {
    return CodeGenerator {
        array_types: IndexMap::new(), types
    }.generate_c_code(ast);
}

pub const prelude: &'static str = {r#"
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define Crash(message) crash(message, __FILE__, __FUNCTION__, __LINE__)

void crash(const char* message, const char* file, const char* function, int line) {{
    printf("%s in %s at %i: \n", file, function, line );
    printf("ERROR: %s\n", message);
    exit(1);
}}

typedef struct {
    char* ptr; 
    size_t len; 
} char_array;

char* c_str(char_array str) {
    char* c_str = (char*) malloc((str.len + 1) * sizeof(char));
    if (!c_str) 
        Crash("Failed to allocate array.");

    for (size_t i = 0; i < str.len; ++i) {
        c_str[i] = str.ptr[i];
    }
    c_str[str.len] = '\0';
    return c_str;
}
"#};

/// CodeGenerator.includes also contains tools, that would have been handled by #include in a normal C project.
/// such as the crash handler and the array types.
struct CodeGenerator { array_types: IndexMap<String, String>, types: Vec<(Ast, Type)> } 

impl CodeGenerator {

    fn generate_c_code(&mut self, ast: Ast) -> String {
        let script = self.generate_ast(ast.assert_script());
    
        // let include_str = self.arrays.iter().cloned().collect::<Vec<&'static str>>().join("\n");
        let array_types_str = self.array_types.values().cloned().collect::<Vec<String>>().join("\n");
        
        return format!(r#"
// ---------- PRELUDE ---------- //{prelude}
// -------- ARRAY_TYPES -------- //{array_types_str}
// ----------- SCRIPT ----------- //
{script}
        "#);
    }
    
    fn generate_ast(&mut self, ast: Ast) -> String {
        return match ast {
            // stmt
            Script { stmts, main } => self.generate_script(stmts, main),
            Block {stmts, .. } => self.generate_block(stmts),
            Assign {var_name, value} => self.generate_assign(var_name, value),
            IndexSet { array, idx , value} => self.generate_index_set(array, idx, value),
            StructSet {struct_, var, value} => self.generate_struct_set(struct_, var, value),
            Init {decl, value } => self.generate_init(decl, value),
            StructDecl { name, content } => self.generate_struct_decl(name, content),
            FnDecl {name, args, ret_type, body} => self.generate_fn_decl(name, args, ret_type, body),
            Decl {name, type_} => self.generate_decl(name, type_),
            ExternDecl { name, type_ } => self.generate_extern_decl(name, type_),
            ExternFnDecl { name, args, ret_type } => self.generate_extern_fn_decl(name, args, ret_type),
            ImportStmt(_) => {"".to_string()},
            ReturnStmt(expr) => self.generate_return(expr),
            ForLoop { item, idx, array, step } => self.generate_for(item, idx, array, step),
            WhileLoop { condition, step } => self.generate_while(condition, step), 
            IfElse { condition, then, else_ } => self.generate_if_else(condition, then, else_),
        
            // expr
            BinaryExpr {op, left, right } => self.generate_binary(op, left, right),
            UnaryExpr {op, arg } => self.generate_unary(op, arg),
            Call {func_name, args } => self.generate_call(func_name, args),
            IndexGet {array, idx } => self.generate_index_get(array, idx),
            StructGet {struct_, var} => self.generate_struct_get(struct_, var),
            CastExpr { from, to } => self.generate_cast(from, to),
            PrintExpr {expr } => self.generate_print(expr),
            ArrayLit (content) => self.generate_array_literal(content),
            StructLit {name, content} => self.generate_struct_literal(name, content),
            Terminal (token) => self.generate_terminal(token),
    
            // _ => unimplemented!("Not yet implemented"),
        };
    }
    
    // ---------- stmt ---------- //
    
    
    fn generate_script(&mut self, stmts: Vec<Box<Ast>>, main: Option<String>) -> String {
        let mut stmts_str = String::new();
        for stmt in stmts {
            let is_expr = stmt.is_expr();
            stmts_str += &self.generate_ast(*stmt);

            stmts_str += if is_expr {";\n"} else {"\n"};
        }
    
        let main_str = match main {
            None => "".to_string(),
            Some(func) => format!(r"
int main(int argc, char *argv[]) {{
    {func}();
    return 0;
}}"),
        };
        // stmts_str += main_str.as_str();
    
        return stmts_str + main_str.as_str();
    }
    
    fn generate_block(&mut self, stmts: Vec<Box<Ast>>) -> String {
        let mut stmts_str = String::new();
        for stmt in stmts {
            let is_expr = stmt.is_expr();
            stmts_str += &self.generate_ast(*stmt);
            stmts_str += if is_expr {";\n"} else {"\n"};
        }
        return format!("{{\n{stmts_str}}}");
    }
    
    fn generate_assign(&mut self, var_name: String, value: Box<Ast>) -> String {
        let value_str = self.generate_ast(value.assert_expr());
        return format!("{var_name} = {value_str};");
    }
    
    fn generate_index_set(&mut self, array: String, idx: Box<Ast>, value: Box<Ast>) -> String {
        let idx_str = self.generate_ast(idx.assert_expr());
        let value_str = self.generate_ast(value.assert_expr());
        // if ({idx_str} < 0 || {idx_str} >= {array}.len) Crash("Index out of bounds");
        return format!(r#"{array}.ptr[{idx_str}] = {value_str};"#);
    }
    
    fn generate_struct_set(&mut self, struct_: String, var: String, value: Box<Ast>) -> String {
        let value_str = self.generate_ast(value.assert_expr());
        return format!(r#"{struct_}.{var} = {value_str};"#);
    }
    
    fn generate_init(&mut self, decl: Box<Ast>, value: Box<Ast>) -> String {
        let decl = self.generate_ast(decl.assert_decl());
        let value = self.generate_ast(value.assert_expr());
        return format!("{decl} = {value};");
    }
    
    fn generate_fn_decl(&mut self, name: String, args: Vec<Box<Ast>>, ret_type: Box<Type>, body: Box<Ast>) -> String {
        let mut args_str = String::new();
        let len = args.len();
        for (i, arg) in args.into_iter().enumerate() {
            args_str += &self.generate_ast(arg.assert_decl());
            if i != len - 1 { args_str += ", "; } 
        }
        let ret_type_str = self.generate_type(ret_type);
        
        let body_str = self.generate_ast(body.assert_block()); // what about void functions?
        return format!("{ret_type_str} {name} ({args_str}) \n{body_str}");
    }

    fn generate_extern_fn_decl(&mut self, name: String, args: Vec<Box<Ast>>, ret_type: Box<Type>) -> String {
        let mut args_str = String::new();
        let len = args.len();
        for (i, arg) in args.into_iter().enumerate() {
            args_str += &self.generate_ast(arg.assert_decl());
            if i != len - 1 { args_str += ", "; } 
        }
        let ret_type_str = self.generate_type(ret_type);
        
        return format!("extern {ret_type_str} {name} ({args_str});");
    }
    
    fn generate_decl(&mut self, name: String, type_: Box<Type>) -> String {
        let type_str = self.generate_type(type_);
        return format!("{type_str} {name}");
    }

    fn generate_extern_decl(&mut self, name: String, type_: Box<Type>) -> String {
        let type_str = self.generate_type(type_);
        return format!("extern {type_str} {name};");
    }

    fn generate_struct_decl(&mut self, name: String, content: Vec<Box<Ast>>) -> String {
        let mut content_str = String::new();
        for var in content {
            let var_str = self.generate_ast(var.assert_decl());
            content_str += format!("    {var_str};\n").as_str();
        }
        // return format!("typedef struct {{\n{content_str}}} {name};\n");
        return format!("struct {name} {{\n{content_str}}};\n");
    }

    fn generate_return(&mut self, expr: Option<Box<Ast>>) -> String {
        let expr_str = match expr {
            Some(expr) => self.generate_ast(expr.assert_expr()),
            None => "".to_string(),
        };
        return format!("return {expr_str};");
    }

    

    // ---------- expr ---------- //

    fn generate_for(&mut self, item: String, idx: Option<String>, array: Box<Ast>, step: Box<Ast>) -> String {
        let array_str = self.generate_ast(array.assert_expr());
        let step_str = self.generate_ast(*step);
        let idx_str =  match idx {
            Some(idx) => idx, 
            None => "i".to_string()
        };
        // typeof(mem.ptr[i]) serves as 'auto' in gcc and clang, but not MSVC
        return format!("for (size_t {idx_str} = 0; {idx_str} < {array_str}.len; ++{idx_str}) {{ 
    typeof(mem.ptr[i]) {item} = {array_str}.ptr[{idx_str}]; 
    {step_str} 
}}"
        ); // for (expr; expr; expr) {stmt}
    }

    fn generate_while(&mut self, condition: Box<Ast>, step: Box<Ast>) -> String {
        let condition_str = self.generate_ast(condition.assert_expr());
        let step_str = self.generate_ast(*step);
        return format!("while ({condition_str}) {{ {step_str} }}"); // while (expr) {stmt}
    }

    fn generate_if_else(&mut self, condition: Box<Ast>, then: Box<Ast>, else_: Option<Box<Ast>>) -> String {
        let is_expr = then.is_expr();
        let condition_str = self.generate_ast(condition.assert_expr());
        let then_str = self.generate_ast(*then);

        if is_expr {
            dbg_assert!(else_ != None && else_.clone().unwrap().is_expr(), 
                "if_else expr should have an expr else clause.");
            let else_str = self.generate_ast(*else_.unwrap());
            return format!("{condition_str} ? {then_str} : {else_str}"); // expr ? expr : expr

        } else if let Some(else_) = else_ {
            let else_str = self.generate_ast(*else_);
            return format!("if ({condition_str}) {{ {then_str} }} else {{ {else_str} }}"); // if (expr) {stmt} else {stmt}

        } else {
            return format!("if ({condition_str}) {{ {then_str} }}"); // if (expr) {stmt}
        }
    }

    fn generate_binary(&mut self, op: Token, left: Box<Ast>, right: Box<Ast>) -> String {
        dbg_assert!(op.is_binary_op());
        
        let left_code = self.generate_ast(left.assert_expr());
        let right_code = self.generate_ast(right.assert_expr());
        return match op {
            Token::Add   => format!("{} + {}", left_code, right_code),
            Token::Minus => format!("{} - {}", left_code, right_code),
            Token::Mul   => format!("{} * {}", left_code, right_code),
            Token::Div   => format!("{} / {}", left_code, right_code),
            Token::Pow   => format!("pow({}, {})", left_code, right_code),
            Token::Gt    => format!("{} > {}", left_code, right_code),
            Token::Gte   => format!("{} >= {}", left_code, right_code),
            Token::Eq    => format!("{} == {}", left_code, right_code),
            Token::Neq   => format!("{} != {}", left_code, right_code),
            Token::Lte   => format!("{} <= {}", left_code, right_code),
            Token::Lt    => format!("{} < {}", left_code, right_code),
            Token::And   => format!("{} && {}", left_code, right_code),
            Token::Or    => format!("{} || {}", left_code, right_code),
            _ => unreachable!("Expected a binary, found {}", op),
        };
    }
    
    fn generate_unary(&mut self, op: Token, arg: Box<Ast>) -> String {
        dbg_assert!(op.is_prefix_op() || op.is_postfix_op());
        let arg_code = self.generate_ast(arg.assert_expr());
        return match op {
            Token::Not => format!("!{}", arg_code), 
            Token::Minus => format!("-{}", arg_code), 
            Token::PtrIn => format!("&{}", arg_code), 
            Token::PtrOut => format!("*{}", arg_code), 
            _ => unreachable!("Expected a unary, found {}", op),
        };
    }
    
    fn generate_call(&mut self, func_name: String, args: Vec<Box<Ast>>) -> String {
        let mut args_str = String::new();
        let len = args.len();
        for (i, arg) in args.into_iter().enumerate() {
            args_str += &self.generate_ast(arg.assert_expr());
            if i != len - 1 { args_str += ", "; } 
        }
    
        return format!("{func_name}({args_str})"); // func_name(arg, ..., arg)
    }

    fn generate_index_get(&mut self, array: String, idx: Box<Ast>) -> String {
        let idx_str = self.generate_ast(idx.assert_expr());
        // ({idx_str} < 0 || {idx_str} >= {array}.len) ? Crash("Index out of bounds") :
        return format!(r#"{array}.ptr[{idx_str}]"#);
    }
    
    fn generate_struct_get(&mut self, struct_: String, var: String) -> String {
        return format!(r#"{struct_}.{var}"#);
    }

    fn generate_cast(&mut self, from: Box<Ast>, to: Type) -> String {
        let from_str = self.generate_ast(from.assert_expr());
        let to_str = self.generate_type(Box::new(to));
        return format!("({}) {}", to_str, from_str);
    }

    fn generate_print(&mut self, expr: Box<Ast>) -> String {
        dbg_assert!(!self.types.is_empty());

        fn get_type(expr: &Ast, types: Vec<(Ast, Type)>) -> Type {
            for (ast, pt) in types {
                if ast == *expr { return pt; }
            }
            unreachable!();
        }
        fn type_format(ty: &Type) -> String {
            return match ty {
                Type::Func(args, ret) => "".to_string(),
                Type::Array(item) => "[]".to_string() + type_format(item).as_str(),
                Type::PtrIn(to) => ">>".to_string() + type_format(to).as_str(),
                Type::PtrOut(from_) => "<<".to_string() + type_format(from_).as_str(),
                Type::Basic(tok) => match tok {
                    Token::BoolTy  => "%d".to_string(),
                    Token::IntTy   => "%d".to_string(),
                    Token::FloatTy => "%f".to_string(),
                    Token::StrTy   => "%s".to_string(),
                    Token::Nil     => "".to_string(),
                    _ => unreachable!(),
                },
                Type::Stmt | Type::Struct(..) => unreachable!(),
            }
        }

        let ty = get_type(&expr, self.types.clone());
        let expr_str = self.generate_ast(expr.assert_expr());

        use Token::*;
        let out = match ty {
            Type::Func(args, ret) => {
                let fn_str = Type::Func(args, ret).to_string();
                format!(r#"printf("{fn_str}\n");"#)
            },
            Type::Struct(name, content) => {
                let mut print_struct_str = format!(r#"printf("{name} {{");"#);
                let len = content.len();
                for (i, (var, type_)) in content.into_iter().enumerate() {
                    let type_format = type_format(&type_);
                    if i != len - 1 { 
                        print_struct_str += format!(r#"printf("{var}: {type_format}, ", {expr_str}.{var});"#).as_str(); 
                    } else {
                        print_struct_str += format!(r#"printf("{var}: {type_format}}}\n", {expr_str}.{var});"#).as_str(); 
                    }
                }
                if len == 0 { print_struct_str += r#"printf("}\n");"#; }
                print_struct_str
            }
            Type::Array(item) => {
                let item_type_str = type_format(&item);
                format!(r#"
{item_type_str}_array arr = {expr_str}; 
printf("[");
for (size_t i = 0; i < arr.len; ++i) {{
    if i != arr.len
        printf("{item_type_str}, ", arr.ptr[i]);
    else
        printf("{item_type_str}]\n", arr.ptr[i]);
}}"#)},
            Type::PtrIn(to)     => {format!(r#"printf(">>{}\n", *{expr_str});"#, type_format(&to))},
            Type::PtrOut(from_) => {format!(r#"printf("<<{}\n", &{expr_str});"#, type_format(&from_))},
            Type::Basic(tok) => match tok {
                BoolTy  => format!(r#"printf("%d\n", {expr_str});"#),
                IntTy   => format!(r#"printf("%d\n", {expr_str});"#),
                FloatTy => format!(r#"printf("%f\n", {expr_str});"#),
                StrTy   => format!(r#"printf("%s\n", c_str({expr_str}));"#),
                Nil     => format!(r#"printf("nil\n");"#),
                _ => unreachable!(),
            },
            Type::Stmt => unreachable!(),
        };

        return out;
    }
    
    fn generate_array_literal(&mut self, content: Vec<Box<Ast>>) -> String {
        dbg_assert!(!self.types.is_empty());

        let mut type_str = "".to_string();
        for (ast, pt) in self.types.clone() {
            if ast == *content[0] { 
                type_str = self.generate_type(Box::new(pt));
                break;
            }
        }

        let len_str = content.len().to_string();

        let mut c_array_literal = String::from("{ ");
        let len = content.len();
        for (i, arg) in content.into_iter().enumerate() {
            c_array_literal += &self.generate_ast(arg.assert_expr());
            if i != len - 1 { c_array_literal += ", "; } 
        }
        c_array_literal += " }";

        let item_type_str = type_str.strip_suffix("_array").unwrap();
        return format!("make_{type_str}({len_str}, ({item_type_str}[]){c_array_literal})"); // { expr, ..., expr }
    }
    
    fn generate_struct_literal(&mut self, name: String, content: IndexMap<String, Ast>) -> String {
        let mut constent_str = String::new();
        let len = content.len();
        for (i, (var_name, var_value)) in content.into_iter().enumerate() {
            let value_str = &self.generate_ast(var_value.assert_expr());
            constent_str += format!(".{var_name} = {value_str}").as_str();
            if i != len - 1 { constent_str += ", "; } 
        }
        return format!("(struct {name}){{{constent_str}}}"); 
    }

    fn generate_terminal(&mut self, tok: Token) -> String {
        dbg_assert!(tok.is_terminal());
        return match tok {
            Token::BoolLit(val) => if val {"1".into()} else {"0".into()}, 
            Token::IntLit(val) => val.to_string(), 
            Token::FloatLit(val) => format!("{:?}f", val), // {:?} includes the .0 at the end of 2.0
            Token::StrLit(val) => format!("make_char_array({}, {:?})", val.len(), val), 
            Token::Ident(val) => val,
            Token::Nil => "NULL".to_string(),
            Token::Break => "break".to_string(),
            Token::Continue => "continue".to_string(),
            _ => unreachable!("Expected a terminal, found {}", tok),
        };
    }
    
    
    
    // ---------- helpers ---------- //

    fn generate_type(&mut self, ty: Box<Type>) -> String {
        use Type::*;
        return match *ty {
            Array(content) => {
                let type_str = self.generate_type(content);
                self.include_array_type(type_str.clone());
                type_str + "_array"
            },
            Struct(name, _) => format!("struct {name}"),
            PtrIn(content) => self.generate_type(content) + "*",
            Basic(token) => match token {
                Token::BoolTy  => "int8_t".to_string(),
                Token::IntTy   => "int32_t".to_string(),
                Token::FloatTy => "float".to_string(),
                Token::Nil     => "void*".to_string(),
                Token::StrTy   => {
                    self.include_array_type("char".to_string());
                    "char_array".to_string()
                },
                _ => unreachable!("Invalid Terminal."),
            },
            Stmt => "void".to_string(), // used by fn return type
            PtrOut(_) | Func(..) => unreachable!(),
        };
    }
    
    fn include_array_type(&mut self, type_str: String) -> () {
        let type_str = type_str.replace("*", "ptr");
        if self.array_types.contains_key(&type_str) { return; } // skip unnecesary work
        dbg_assert!(type_str.chars().all(|c| c.is_alphanumeric() || c == '_'), type_str);
        
        let array_type = format!(r#"
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
        "#);

        self.array_types.insert(type_str, array_type);
    }

    
}


