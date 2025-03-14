use indexmap::IndexMap;
use crate::dbg_assert;
use super::Token;


#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Func(Vec<Box<Type>>, Box<Type>),
    Struct(String, IndexMap<String, Type>),
    Array(Box<Type>),
    PtrIn(Box<Type>),
    PtrOut(Box<Type>),
    Basic(Token),
    Stmt,
}
impl Type {
    pub fn to_string(&self) -> String {
        return match self {
            Type::Func(args, ret) => {
                let mut fn_string = String::from("fn (");
                for (i, arg) in args.iter().enumerate() {
                    fn_string += arg.to_string().as_str();
                    if i < args.len() - 1 { fn_string += ", " }
                }
                fn_string += ") ";
                fn_string += ret.to_string().as_str();
                fn_string
            },
            Type::Struct(name, content) => {
                let mut out = name.clone() + " {";
                let last_idx = if content.is_empty() {0} else {content.len() - 1};
                for (i, (var, ty)) in content.iter().enumerate() {
                    let ty_str = ty.to_string();
                    out += format!("{var}: {ty_str}").as_str();
                    if i != last_idx { out += ", "; }
                }
                out += "}";
                out
            },
            Type::Array(content) => "[]".to_string() + content.to_string().as_str(),
            Type::PtrIn(content) => ">>".to_string() + content.to_string().as_str(),
            Type::PtrOut(content) => "<<".to_string() + content.to_string().as_str(),
            Type::Basic(token) => match token {
                Token::BoolTy  => "bool".to_string(),
                Token::IntTy   => "int".to_string(),
                Token::FloatTy => "float".to_string(),
                Token::StrTy   => "str".to_string(),
                Token::Nil     => "nil".to_string(),
                _ => unreachable!("Invalid Terminal."),
            },
            Type::Stmt => "Statement".to_string(),
            
        };
    }

    pub fn vec_box_to_string(v: Vec<Box<Type>>) -> String {
        let mut out = String::from("[");
        for (i, t) in v.iter().enumerate() {
            out += t.to_string().as_str();
            if i != v.len() - 1 { out += ", "; }
        }
        out += "]";
        return out;
    }
}

impl From<Token> for Type { fn from(tok: Token) -> Self { 
    dbg_assert!(tok.is_primitive_type() || matches!(tok, Token::Ident(_)));
    Type::Basic(tok) 
}}
impl From<Token> for Box<Type> { fn from(tok: Token) -> Self { 
    dbg_assert!(tok.is_primitive_type() || matches!(tok, Token::Ident(_)));
    Box::new(Type::Basic(tok))
}}



