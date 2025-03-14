use indexmap::IndexMap;
use crate::{brk, brk_if, dbg_assert, func_name, program_error, Fallible, P};
use super::{Ast, Token, Tokenizer, Type};
use super::Token::*;
use super::Ast::*;
/*
entry -> stmt ???

stmt -> (init | assign | block_stmt | fn_decl | expr) stmtSep
assign -> old_identifier = expr
init -> decl := expr
fn_decl -> fn new_identifier(decl, ..., decl) type block
block_stmt -> : (stmt)+ | : \n indent (stmt)+ dedent

expr -> unary_expr | unary_expr binary_operator expr
unary_expr -> nullary_expr | unary_operator unary_expr       nb: unary_operator is prefix
nullary_expr -> literal | old_identifier | call | paren | block_expr
call -> old_identifier(expr, ..., expr)
literal -> int | float | bool | str
paren -> (expr)
block_expr -> : (stmt)* expr | : \n indent (stmt)* expr dedent

block -> block_stmt | block_expr
decl -> new_identifier type
unary_operator -> uop1 | ... | uopN
binary_operator -> binop1 | ... | binopN
type -> ty1 | ... | tyN

*/


pub struct Parser { 
    tok: Token, 
    next_tok: Token, 
    tokenizer: Tokenizer, 
    main: Option<String>,
    available_libs: Vec<String>,
    used_libs: Vec<String>,
    pub use_log: bool 
}

impl Parser {
    pub fn from(mut tokenizer: Tokenizer, avaible_libs: Vec<String>) -> Fallible<Self> {
        dbg_assert!(tokenizer.idx == 0, "Parser must be built with an unused tokenizer.");
        let tok = tokenizer.get_tok()?;
        let next_tok = tokenizer.get_tok()?;
        // let avaible_libs = avaible_libs.iter().map(|al| al.).collect();
        return Ok(Parser { 
            tok, next_tok, tokenizer, main: None, 
            available_libs: avaible_libs, used_libs: Vec::new(), 
            use_log: false 
        });
    }
    
    
    // -------- parse entire ast -------- // 

    pub fn parse(&mut self) -> Fallible<(Ast, Vec<String>)> {
        self.log(func_name!());
        #[cfg(debug_assertions)] if self.use_log { eprintln!("") }
        
        let ast = self.parse_script()?;
        return Ok((ast, self.used_libs.clone()));
    }


    // -------- parse stmt -------- // 
    
    /// script -> stmt*
    fn parse_script(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        // let mut has_err: bool = false;
        
        let mut stmts: Vec<Box<Ast>> = Vec::new();
        while self.tok == StmtSep { self.step()?; } // allow (in parser) scripts with nothing but no-ops.
        while End != self.tok {
            stmts.push(Box::new(self.parse_stmt()?));
            while self.tok == StmtSep { self.step()?; } // stepping over no-ops.
        }
        let main = self.main.clone();
        // if has_err { return program_error!("");}
        return Ok(Script { stmts, main });
    }

    /// stmt -> (init | assign | block_stmt | fn_decl | expr | skip) stmtSep
    fn parse_stmt(&mut self) -> Fallible<Ast> {
        self.log(func_name!());

        while self.tok == StmtSep { self.step()?; } // stepping over no-ops.
        if self.tok.cannot_begin_stmt() { return program_error!(
            "Expected statement, found {}, but this token cannot begin a new statement.", self.tok
        );}

        let stmt = match &self.tok {
            BlockStart => self.parse_block()?,
            Extern if self.next_tok == Fn => self.parse_extern_fn_decl()?,
            Extern => self.parse_extern_decl()?,
            // Import => self.parse_import()?,
            Struct => self.parse_struct_decl()?,
            Main | Fn => self.parse_fn_decl()?,
            Ident(_) if self.next_tok == Colon => self.parse_init()?,
            Ident(_) if self.next_tok == AssignOp => self.parse_assign()?,
            Ident(_) if self.next_tok == BracketL => self.parse_index_set()?,
            Ident(_) if self.next_tok == Dot => self.parse_struct_set()?,
            Import => self.parse_import()?,
            Break | Continue => self.parse_terminal()?,
            Return => self.parse_return()?,
            If => self.parse_if_else(false)?,
            For => self.parse_for()?,
            While => self.parse_while()?,
            _ => self.parse_expr()?,
        };
        // we deviate from C regarding what is or isn't an expr and therefore what requires a semicolon.
        if stmt.is_expr() { self.expect(StmtSep)?; }  //  && self.tok != End
        // while self.tok == StmtSep { self.step()?; } // stepping over no-ops.

        return Ok(stmt); 
    }


    
    /// importStmt -> import string
    fn parse_import(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Import));
        self.step()?; // eat `import`
        let StrLit(library) = self.step()? else { return program_error!(
            r#"Expected "library_name.extension" after import. Found {}"#, self.tok
        )};
        if !self.available_libs.contains(&library) { return program_error!(
"Expected library name after import. Found `{library}`, but the only available libraries are {:?}.
Hint: libraries are expected to be in the <project>/libraries folder. ", self.available_libs
        )};
        self.used_libs.push(library.clone());
        return Ok(ImportStmt(library));
    }


    /// struct_decl -> struct { decl, ..., decl }
    fn parse_struct_decl(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Struct));

        self.step()?; // eat `struct`
        let Ident(name) = self.step()? else { 
            return program_error!("Expected struct name in struct declaration. Found {}", self.tok);
        };
        self.expect(BlockStart)?;

        let mut content: Vec<Box<Ast>> = Vec::new();
        while self.tok != BlockEnd {
            content.push(Box::new(self.parse_decl()?));

            if self.tok == BlockEnd { break; }
            self.expect(Comma)?; // eat ,
        }
        dbg_assert!(self.tok == BlockEnd);
        self.step()?; // eat `}`

        return Ok(StructDecl { name, content });
    }


    /// assign -> old_identifier = expr
    fn parse_assign(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Ident(_)));
        let Ident(var_name) = self.step()? else { 
            return program_error!("Expected variable name in assignment. Found {}", self.tok);
        };
        self.expect(AssignOp)?;
        let value = Box::new(self.parse_expr()?);

        return Ok(Assign {var_name, value});
    }

    /// index_set -> old_identifier[expr] = expr
    fn parse_index_set(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Ident(_)));
        let Ident(array) = self.step()? else { 
            return program_error!("Expected array name. Found {}", self.tok);
        };
        self.expect(BracketL)?;
        let idx = Box::new(self.parse_expr()?);
        self.expect(BracketR)?;

        self.expect(AssignOp)?;
        let value = Box::new(self.parse_expr()?);

        return Ok(IndexSet { array, idx, value });
    }  

    /// struct_set -> identifier.identifier = expr
    fn parse_struct_set(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Ident(_)));
        let Ident(struct_) = self.step()? else { return program_error!(
            "Expected struct name. Found {}", self.tok) 
        };
        self.expect(Dot)?;
        let Ident(var) = self.step()? else { return program_error!(
            "Expected name of variable in struct. Found {}", self.tok) 
        };

        self.expect(AssignOp)?;
        let value = Box::new(self.parse_expr()?);

        return Ok(StructSet { struct_, var, value });
    }  

    /// decl_init -> decl := expr
    fn parse_init(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        let decl = Box::new(self.parse_decl()?);
        self.expect(InitOp)?;
        if let Decl{type_: ty, .. } = decl.as_ref() {
            if matches!(**ty, Type::Struct(..)) && self.tok == BlockStart {
                eprintln!(
"WARNING: Found `{{` when initializing struct. This begins a block. 
If you intended a struct literal, then prefix it by the struct name.
");
            }
        }
        let value = Box::new(self.parse_expr()?);
        return Ok(Init {decl, value });
    }

    /// fn_decl -> fn new_identifier(decl, ..., decl) type block
    fn parse_fn_decl(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Fn | Main));
        let is_main = self.tok == Main;
        if is_main { self.step()?; } // eat main

        self.step()?; // eat fn
        let Ident(name) = self.step()? else { 
            return program_error!("Expected function name in function declaration. Found {}", self.tok);
        };
        self.expect(ParenL)?;

        if is_main { 
            if let Some(main) = &self.main {
                return program_error!("Ecountered an ambiguity between two main functions {} vs {}.", main, &name);
            }
            self.main = Some(name.clone()) 
        };

        let mut args: Vec<Box<Ast>> = Vec::new();
        while self.tok != ParenR {
            args.push(Box::new(self.parse_decl()?));

            if self.tok == ParenR { break; }
            self.expect(Comma)?; // eat ,
        }
        dbg_assert!(self.tok == ParenR);
        self.step()?; // eat )

        let ret_type = if self.tok != BlockStart && self.tok != Nil {
            Box::new(self.parse_type()?)
        } else {
            Box::new(Type::Stmt)
        };
        
        if self.tok != BlockStart {
            return program_error!("Expected {{ at the start of function body. Found {}", self.tok);
        }
        let body = Box::new(self.parse_block()?);

        return Ok(FnDecl {name, args, ret_type, body});
    }

    /// fn_extern_decl -> extern fn new_identifier(decl, ..., decl) type
    fn parse_extern_fn_decl(&mut self) -> Fallible<Ast> {
            self.log(func_name!());
            dbg_assert!(matches!(self.tok, Extern));
            self.step()?; // eat extern
            if self.tok == Main {return program_error!("Main function cannot be extern.");}
    
            self.expect(Fn)?;
            let Ident(name) = self.step()? else { 
                return program_error!("Expected function name in function declaration. Found {}", self.tok);
            };
            self.expect(ParenL)?;
    
    
            let mut args: Vec<Box<Ast>> = Vec::new();
            while self.tok != ParenR {
                args.push(Box::new(self.parse_decl()?));
    
                if self.tok == ParenR { break; }
                self.expect(Comma)?; // eat ,
            }
            dbg_assert!(self.tok == ParenR);
            self.step()?; // eat )
    
            let ret_type = if self.tok != Nil {
                Box::new(self.parse_type()?)
            } else {
                Box::new(Type::Stmt)
            };
    
            return Ok(ExternFnDecl {name, args, ret_type});
        }
    
    
    /// decl -> new_identifier type
    fn parse_decl(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        // no name is a program error, not compiler error, since fn_decl, ekstern_fn_decl and struct_decl
        // will enter parse_decl without detecting the start of a decl.
        let Ident(name) = self.step()? else { return program_error!(
            "Expected a variable name. Found {}", self.tok
        )};

        self.expect(Colon)?;
        let type_ = Box::new(self.parse_type()?);
        return Ok(Decl {name, type_});
    }

    /// extern_decl -> extern new_identifier type
    fn parse_extern_decl(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Extern));
        self.step()?; // eat `extern`

        let Ident(name) = self.step()? else {return program_error!(
            "Expected identifier after extern. Found {}", self.tok)};

        self.expect(Colon)?;
        let type_ = Box::new(self.parse_type()?);
        return Ok(ExternDecl {name, type_});
    }

    // ExternFnDecl {name: String, args: Vec<Box<Ast>>, ret_type: Box<Ast>},
    // ExternDecl {name: String, type_: Box<Ast>},
    // Import (String),


    /// type -> []type | >>type | primitive_type
    fn parse_type(&mut self) -> Fallible<Type> {
        self.log(func_name!());
        
        if self.tok == PtrIn { 
            self.step()?; // eat `>>`
            return Ok(Type::PtrIn(Box::new(self.parse_type()?)));
        }
        else if self.tok == BracketL && self.next_tok == BracketR { 
            self.step()?; // eat `[`
            self.step()?; // eat `]`
            return Ok(Type::Array(Box::new(self.parse_type()?)));
        }
        else if self.tok.is_primitive_type() { 
            let ty = self.step()?; // eat primitive type
            return Ok(Type::Basic(ty.clone()));
        }
        else if let Ident(ty) = self.tok.clone() {
            self.step()?; // eat struct type
            // The content of the hashmap is not yet known. Lets hope empty is ok.
            return Ok(Type::Struct(ty, IndexMap::new())); 
        }
        else {
            return program_error!("Expected type found {}", self.tok);
        }        
        /*
        let mut prefixes: Vec<TypeOperator> = Vec::new();
        loop {
            if self.tok == PtrIn { 
                let op = self.step()?; // eat prefix
                prefixes.push(TypeOperator::PtrIn);
            }
            else if self.tok == BracketL && self.next_tok == BracketR { 
                let op = self.step()?; // eat BracketL
                let op = self.step()?; // eat BracketR
                prefixes.push(TypeOperator::BracketLR);
            }
            else { break; }
        }
        if !self.tok.is_primitive_type() { 
            return program_error!("Expected primitive type found {}", self.tok);
        }
        let primitive = self.step()?; // eat primitive type
        return Ok(TypeAst {prefixes, primitive});
        */
    }

    /// block_stmt -> { (stmt)* (stmt | expr) }
    fn parse_block(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(self.tok == BlockStart);
        self.step()?; // eat {

        let mut stmts: Vec<Box<Ast>> = Vec::new();
        while self.tok == StmtSep { self.step()?; } // allow blocks with nothing but no-ops.
        if self.tok == BlockEnd { self.step()?; return Ok(Block {stmts}) } // allow empty blocks
        loop {
            stmts.push(Box::new(self.parse_stmt()?)); 
            while self.tok == StmtSep { self.step()?; } // stepping over no-ops.
            if self.tok == BlockEnd { 
                self.step()?; 
                break;
            }
        }
        
        return Ok(Block {stmts});
    }

    /// for -> for ident, ident? in (ident | array_literal) stmt
    fn parse_for(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(self.tok == For);

        self.step()?; // eat `for`
        let Ident(item) = self.step()? else { return program_error!(
            "Expected identifier for item in for loop. Found {}", self.tok)};
        let idx = match &self.tok {
            Comma => {
                self.step()?; // eat `,`
                let Ident(idx) = self.step()? else { return program_error!(
                    "Expected index after comma in for loop. Found {}", self.tok)};
                Some(idx)
            }
            _ => None,
        };
        self.expect(In)?;
        let array = match &self.tok {
            Ident(array) => Box::new(self.parse_terminal()?),
            BracketL => Box::new(self.parse_array_literal()?),
            _ => return program_error!("Expected an array to loop over. Found {}", self.tok),
        };
        self.expect(Do)?; // evt. make this optional
        let step = Box::new(self.parse_stmt()?);

        return Ok(ForLoop { item, idx, array, step });
    }

    /// while -> while expr do stmt
    fn parse_while(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(self.tok == While);

        self.step()?; // eat `while`
        let condition = Box::new(self.parse_expr()?);
        self.expect(Do)?;
        let step = Box::new(self.parse_stmt()?);

        return Ok(WhileLoop {condition, step});
    }

    /// if_else -> if expr then stmt else stmt
    fn parse_if_else(&mut self, is_expr: bool) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(self.tok == If);

        self.step()?; // eat `if`
        let condition = Box::new(self.parse_expr()?);

        self.expect(Do)?;
        let then = Box::new(if is_expr {self.parse_expr()?} else {self.parse_stmt()?});

        let else_ = match self.tok {
            Else => {
                self.step()?; 
                Some(Box::new(if is_expr {self.parse_expr()?} else {self.parse_stmt()?})) 
            },
            _ => if is_expr { return program_error!("The if and else clauses doesn't match")} else {None},
        };

        return Ok(IfElse {condition, then, else_});
    }

    /// returnStmt -> return expr
    fn parse_return(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(self.tok == Return);

        self.step()?; // eat `return`
        let expr = match self.tok {
            StmtSep => None,
            _ => Some(Box::new(self.parse_expr()?)),
        };
        return Ok(ReturnStmt(expr));
    }




    // -------- parse expr -------- // 

    /// expr -> if_else_expr | unary_expr binary_rhs
    fn parse_expr(&mut self) -> Fallible<Ast> { // = parse_binary_expr
        self.log(func_name!());
        if self.tok.cannot_begin_expr() { return program_error!(
            "Expected expression, found {}, but this token cannot begin a new expression.", self.tok
        );}

        match self.tok {
            If => return self.parse_if_else(true),
            _ => { // binary_expr
                let left = self.parse_unary_expr()?;
                return self.parse_binary_rhs(Box::new(left), 0); // 0 is the non-binOp precedence
            },
        };
    }


    /// binary_rhs -> (binOp unary_expr)* with precedence resolution
    fn parse_binary_rhs(&mut self, mut left: Box<Ast>, close_paren_prec: u8) -> Fallible<Ast> { // returns binary_expr
        self.log(func_name!());

        loop { // loop over (u op u op ...) until hitting close_paren_prec or end of bin_expr
            let prec = precedence(&self.tok); // non-bin_op's have the minimum precedence and therefore causes an exit.
            if prec <= close_paren_prec { return Ok(*left); } // use <= over < to bind left for equal precedence.
            let op = self.step()?;

            let mut right = self.parse_unary_expr()?;

            let next_prec = precedence(&self.tok);
            if prec < next_prec { // if left op (right next_op ...
                // parse (right next_op ... ) and then loop around to continue past the end paren.
                right = self.parse_binary_rhs(Box::new(right), prec)?; 
            } // else (left op right) next_op ...
            left = Box::new(BinaryExpr { op: op, left: left, right: Box::new(right) });
        }
    }

    /// unary_expr -> prefix_operator unary_expr | nullary_expr (postfix_operator)*
    fn parse_unary_expr(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        // 
        if self.tok.is_prefix_op() { 
            let op = self.step()?; // eat prefix
            let arg = Box::new(self.parse_unary_expr()?);
            return Ok(UnaryExpr { op, arg });
        }

        let mut arg = Box::new(self.parse_nullary_expr()?);
        while self.tok.is_postfix_op() {
            let op = match self.step()? {
                PtrOut => PtrIn,
                PtrIn => PtrOut,
                _ => unreachable!(),
            };
            arg = Box::new(UnaryExpr { op, arg });
        }
        return Ok(*arg);

    }

    /// nullary_expr -> literal | old_identifier | call | index | paren | block_expr
    fn parse_nullary_expr(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        
        return match &self.tok {
            Ident(_) if (self.next_tok == Dot) => self.parse_struct_get(),
            Ident(_) if (self.next_tok == ParenL) => self.parse_call(),
            Ident(_) if (self.next_tok == BracketL) => self.parse_index_get(), // placing it here only allows indexing on ident, not expr
            Ident(_) if (self.next_tok == BlockStart) => self.parse_struct_lit(),
            Ident(_) => self.parse_terminal(),
            tok if tok.is_literal() => self.parse_terminal(),
            Print => self.parse_print_expr(),
            ParenL => self.parse_paren(),
            BlockStart => self.parse_block(),
            BracketL => self.parse_array_literal(),
            As => self.parse_cast_expr(),
            _ => unreachable!("This token cannot begin a nullary expr. token = {}", self.tok),
        };
    }

    /// cast_expr -> As(type) expr
    fn parse_cast_expr(&mut self) -> Fallible<Ast> {
        dbg_assert!(self.tok == As);
        self.step()?; // eat `as`
        self.expect(ParenL)?;
        let to = self.parse_type()?;
        self.expect(ParenR)?;
        let from = Box::new(self.parse_expr()?);
        return Ok(CastExpr { from, to });
    }

    /// print_expr -> P expr
    fn parse_print_expr(&mut self) -> Fallible<Ast> {
        dbg_assert!(self.tok == Print);
        self.step()?; // eat P
        let expr = Box::new(self.parse_unary_expr()?);
        return Ok(PrintExpr { expr });
    }

    /// call -> old_identifier | old_identifier(expr, ..., expr)
    fn parse_call(&mut self) -> Fallible<Ast> {
        self.log(func_name!());

        let Ident(func_name) = self.tok.clone() else { return program_error!("Expected identifier as function name."); };
        self.step()?; // eat func_name
        self.step()?; // eat (
        
        let mut args: Vec<Box<Ast>> = Vec::new();
        while self.tok != ParenR {
            args.push(Box::new(self.parse_expr()?));

            if self.tok == ParenR { break; }
            self.expect(Comma)?; // eat ,
        }
        dbg_assert!(self.tok == ParenR);
        self.step()?; // eat )
            
        return Ok(Call { func_name, args });
    }

    /// index -> ident[expr]
    fn parse_index_get(&mut self) -> Fallible<Ast> {
        dbg_assert!(matches!(self.tok, Ident(_)));
        let Ident(array) = self.step()? else { return program_error!(
            "Expected array name. Found {}", self.tok) 
        };
        self.expect(BracketL)?;
        let idx = Box::new(self.parse_expr()?);
        self.expect(BracketR)?;
        return Ok(IndexGet { array, idx });
    }

    /// struct_get -> identifier.identifier
    fn parse_struct_get(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(matches!(self.tok, Ident(_)));
        let Ident(struct_) = self.step()? else { return program_error!(
            "Expected struct name. Found {}", self.tok) 
        };
        self.expect(Dot)?;
        let Ident(var) = self.step()? else { return program_error!(
            "Expected name of variable in struct. Found {}", self.tok) 
        };
        return Ok(StructGet { struct_, var });
    }

    /// paren -> (expr)
    fn parse_paren(&mut self) -> Fallible<Ast> {
        self.log(func_name!());
        dbg_assert!(self.tok == ParenL);

        self.step()?; // eat (
        let expr = self.parse_expr()?;
        self.expect(ParenR)?; // eat )
        
        return Ok(expr);
    }


    /// array_literal -> [expr, ..., expr]
    fn parse_array_literal(&mut self) -> Fallible<Ast> {
        dbg_assert!(self.tok == BracketL);
        self.step()?; // eat [
        let mut content: Vec<Box<Ast>> = Vec::new();
        while self.tok != BracketR {
            content.push(Box::new(self.parse_expr()?));

            if self.tok == BracketR { break; }
            self.expect(Comma)?; // eat ,
        }
        dbg_assert!(self.tok == BracketR);
        self.step()?; // eat ]

        return Ok(ArrayLit(content));
    }

    /// struct_literal -> {name: expr, ..., name: expr}
    fn parse_struct_lit(&mut self) -> Fallible<Ast> {
        dbg_assert!(matches!(self.tok, Ident(_)));
        let Ident(struct_name) = self.step()? else {unreachable!()};
        self.expect(BlockStart)?;

        let mut content: IndexMap<String, Ast> = IndexMap::new();
        while self.tok != BlockEnd {
            let Ident(var_name) = self.step()? else {return program_error!(
                "Expected variable name in {} struct literal. Found {}", struct_name, self.tok
            )};
            self.expect(Colon)?;
            content.insert(var_name, self.parse_expr()?);

            if self.tok == BlockEnd { break; }
            self.expect(Comma)?; // eat ,
        }
        dbg_assert!(self.tok == BlockEnd);
        self.step()?; // eat }

        return Ok(StructLit {name: struct_name, content});
    }

    /// terminal -> literal | ident
    ///          -> int | float | bool | str | ident
    fn parse_terminal(&mut self) -> Fallible<Ast> { // remove this
        self.log(func_name!());
        dbg_assert!(self.tok.is_terminal());
        return Ok(Terminal(self.step()?.clone())); 
    }
    
    // -------- helpers -------- // 
    #[inline] fn step(&mut self) -> Fallible<Token> { 
        let tok = self.tok.clone();
        self.tok = self.next_tok.clone();
        self.next_tok = self.tokenizer.get_tok()?;
        self.log("step_to");
        // brk_if!(self.tok == BracketR);
        return Ok(tok);
    }


    #[inline] fn log(&self, func_name: &str) {
        if !self.use_log {return;}
        eprintln!("parser.{}: {:?}", func_name, &self.tok);
    }

    #[inline] fn expect(&mut self, tok: Token) -> Fallible<Token> { 
        let found = self.step()?;
        use std::hint::black_box;
        
        if found != tok { 
            return black_box(program_error!("Expected {}, found {}.", tok, found)); 
        }
        return Ok(found);
    }

    // pub fn unused(&self) -> bool { self.tokenizer.idx == 0 }

}

fn precedence(tok: &Token) -> u8 {
    return match tok {
        Pow => 60,
        Mul | Div => 50, // 3 * (5^2)
        Add | Minus  => 40, // 2 + (3 * 5) 
        Gt | Gte | Lte | Lt  => 30, // (2 + 3) >= 5
        And | Or => 20, // (2 + 3 >= 5) and (6 < 7)
        Eq | Neq => 10, // (a and b) == (c or d)
        _ => 0, // non-binOps must have the lowest possible precedence, since they cause an exit from parsing binOp
    };
}

