use std::fmt;
use crate::{brk, brk_if, dbg_assert, P};


#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Ident(String), 
    Fn, Main, Print, If, Else, While, Do, For, In, Return, Break, Continue, Extern, Nil, Import, Struct, As, // keywords
    ParenL, ParenR, Comma, Colon, Dot, BlockStart, BlockEnd, InitOp, AssignOp, StmtSep, BracketL, BracketR, // system-symbols
    BoolTy, IntTy, FloatTy, StrTy, // types
    BoolLit(bool), IntLit(i128), FloatLit(f64), StrLit(String), // literals
    Not, Minus, PtrOut, PtrIn, // unary_operators
    Add, /*Minus,*/ Mul, Div, Pow, Gt, Gte, Eq, Neq, Lte, Lt, And, Or, // binary_operators (nb: Minus can both be unary and binary)
    End,
}

impl Token {
    pub fn is_primitive_type(&self) -> bool { use Token::*; matches!(self, 
        BoolTy | IntTy | FloatTy | StrTy | Nil
    )}

    pub fn is_binary_op(&self) -> bool { use Token::*; matches!(self, 
        Add | Minus | Mul | Div | Pow | Gt | Gte | Eq | Neq | Lte | Lt | And | Or | As  
    )}

    pub fn is_prefix_op(&self) -> bool { use Token::*; matches!(self, 
        Not | Minus | PtrOut | PtrIn
    )}

    pub fn is_postfix_op(&self) -> bool { use Token::*; matches!(self, 
        PtrOut | PtrIn
    )}

    pub fn is_literal(&self) -> bool { use Token::*; matches!(self, 
        BoolLit(_) | IntLit(_) | FloatLit(_) | StrLit(_) | Nil
    )}
    pub fn is_terminal(&self) -> bool { use Token::*; matches!(self, 
        BoolLit(_) | IntLit(_) | FloatLit(_) | StrLit(_) | Ident(_) | Nil | // expr
        Break | Continue// stmt 
    )}

    pub fn cannot_begin_stmt(&self) -> bool { use Token::*; 
        return self.is_primitive_type() || matches!(self, 
            Else | Do | In | ParenR | Comma | Colon | BlockEnd | InitOp | AssignOp | BracketR | 
            Add /*| Minus */| Mul | Div | Pow | Gt | Gte | Eq | Neq | Lte | Lt | And | Or
        )
    }

    pub fn cannot_begin_expr(&self) -> bool { use Token::*; 
        return matches!(self, 
            Fn | Main | While | For | Return | Break | Continue
        ) || matches!(self, // similar to cannot_begin_stmt except that primitive types are allowed
            Else | Do | In | ParenR | Comma | Colon | BlockEnd | InitOp | AssignOp | BracketR | 
            Add /*| Minus */| Mul | Div | Pow | Gt | Gte | Eq | Neq | Lte | Lt | And | Or
        )
    }

}



impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", &self)
    }
}

// impl Token {
//     pub fn is_unary(self) -> bool {
//         use Token::*;
//         return matches!(self, Not | Minus);
//     }
//     pub fn is_binop(self) -> bool {
//         use Token::*;
//         return matches!(self, Add | Mul | Div | Pow | Gt | Gte | Eq | Neq | Lte | Lt | And | Or);
//     }

// }
