use indexmap::IndexMap;
use crate::dbg_assert;
use super::{Token, Type};

#[derive(Debug, PartialEq, Clone)]
pub enum Ast {
    // stmt
    Script {stmts: Vec<Box<Ast>>, main: Option<String>},
    Block {stmts: Vec<Box<Ast>>},
    Assign {var_name: String, value: Box<Ast>},
    IndexSet {array: String, idx: Box<Ast>, value: Box<Ast>},
    StructSet {struct_: String, var: String, value: Box<Ast>},
    Init {decl: Box<Ast>, value: Box<Ast> },
    StructDecl {name: String, content: Vec<Box<Ast>>}, 
    FnDecl {name: String, args: Vec<Box<Ast>>, ret_type: Box<Type>, body: Box<Ast>},
    Decl {name: String, type_: Box<Type>},
    ExternFnDecl {name: String, args: Vec<Box<Ast>>, ret_type: Box<Type>},
    ExternDecl {name: String, type_: Box<Type>},
    ImportStmt (String),
    ReturnStmt (Option<Box<Ast>>),
    ForLoop {item: String, idx: Option<String>, array: Box<Ast>, step: Box<Ast>},
    WhileLoop {condition: Box<Ast>, step: Box<Ast>},
    IfElse {condition: Box<Ast>, then: Box<Ast>, else_: Option<Box<Ast>>},

    // expr
    BinaryExpr {op: Token, left: Box<Ast>, right: Box<Ast>}, // = BinaryExpr
    UnaryExpr {op: Token, arg: Box<Ast>},
    Call {func_name: String, args: Vec<Box<Ast>>}, 
    IndexGet {array: String, idx: Box<Ast>}, 
    StructGet {struct_: String, var: String},
    PrintExpr {expr: Box<Ast>},
    CastExpr {from: Box<Ast>, to: Type},
    ArrayLit (Vec<Box<Ast>>),
    StructLit {name: String, content: IndexMap<String, Ast>},
    Terminal (Token), // Literal | Identifier | break | continue
}



impl Ast {
    #[inline] pub fn assert_decl(self) -> Self { use Ast::*; dbg_assert!(matches!(self, 
        Decl{..}
    )); return self; }

    #[inline] pub fn assert_block(self) -> Self { use Ast::*; dbg_assert!(matches!(self, 
        Block{..}
    )); return self; }

    #[inline] pub fn assert_script(self) -> Self { use Ast::*; dbg_assert!(matches!(self, 
        Script{..}
    )); return self; }

    #[inline] pub fn assert_expr(self) -> Self { use Ast::*; dbg_assert!(self.is_expr()); return self; }

    #[inline] pub fn is_expr(&self) -> bool { use Ast::*; 
        if let IfElse { condition, then, else_ } = self {
            return then.is_expr();
        }
        if let Terminal(tok) = self {
            return !matches!(tok, Token::Break | Token::Continue);
        }
        return matches!(self, 
            BinaryExpr{..} | UnaryExpr{..} | Call{..} | CastExpr{..} | PrintExpr{..} | WhileLoop{..} 
            | ArrayLit(..) | IndexGet {..} | StructLit {..} | StructGet {..}
        );
    }

}

impl From<Token> for Ast { fn from(tok: Token) -> Self { 
    dbg_assert!(tok.is_terminal());
    Ast::Terminal(tok)
}}


