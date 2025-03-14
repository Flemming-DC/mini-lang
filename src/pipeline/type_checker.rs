use std::collections::HashMap;
use indexmap::IndexMap;
use crate::{dbg_assert, program_error, Fallible, P};
use super::{Ast, Type, Token};
use super::Ast::*;


pub fn check_ast(ast: &Ast, require_main: bool) -> Fallible<Vec<(Ast, Type)>> {
    let mut checker = Checker {
        symbols: vec![HashMap::new()], types: vec![], 
        in_loop: false, in_func: false, return_type: None, require_main: require_main
    };
    checker.check_ast(ast)?;
    return Ok(checker.types);
}

struct Checker {symbols: Vec<HashMap<String, Type>>, types: Vec<(Ast, Type)>, 
    in_loop: bool, in_func: bool, return_type: Option<Type>, require_main: bool
}


impl Checker {
    fn check_ast(&mut self, ast: &Ast) -> Fallible<Type> {
        return match ast {
            // stmt
            Script { stmts, main } => self.check_script(stmts, main),
            Block {stmts } => self.check_block(stmts),
            Assign {var_name, value} => self.check_assign(var_name, value),
            IndexSet { array, idx, value} => self.check_index_set(array, idx, value),
            StructSet {struct_, var, value} => self.check_struct_set(struct_, var, value),
            Init {decl, value } => self.check_init(decl, value),
            StructDecl { name, content } => self.check_struct_decl(name, content),
            FnDecl {name, args, ret_type, body} => self.check_fn_decl(name, args, ret_type, body),
            Decl {name, type_} => self.check_decl(name, type_),
            ExternDecl { name, type_ } => self.check_extern_decl(name, type_),
            ExternFnDecl { name, args, ret_type } => self.check_extern_fn_decl(name, args, ret_type),
            ImportStmt(_) => {Ok(Type::Stmt)},
            ReturnStmt(expr) => self.check_return(expr),
            ForLoop { item, idx, array, step } => self.check_for(item, idx, array, step),
            WhileLoop { condition, step } => self.check_while(condition, step), 
            IfElse { condition, then, else_ } => self.check_if_else(condition, then, else_),
        
            // expr
            BinaryExpr {op, left, right } => self.check_binary(op, left, right),
            UnaryExpr {op, arg } => self.check_unary(op, arg),
            Call {func_name, args } => self.check_call(func_name, args),
            IndexGet {array, idx } => self.check_index_get(array, idx),
            StructGet {struct_, var} => self.check_struct_get(struct_, var),
            CastExpr { from, to } => self.check_cast(from, to),
            PrintExpr {expr, ..} => self.check_print(expr),
            ArrayLit (content) => self.check_array_literal(content),
            StructLit {name, content} => self.check_struct_literal(name, content),
            Terminal (token) => self.check_terminal(token),

            // _ => unimplemented!("Not yet implemented"),
        };
    }


    // ------------------ stmt ------------------ //

    fn check_script(&mut self, stmts: &Vec<Box<Ast>>, main: &Option<String>) -> Fallible<Type> {
        // equivalent to check_block
        self.symbols.push(HashMap::new());
        for stmt in stmts { self.check_ast(stmt)?; }
        if let Some(main) = main { 
            dbg_assert!(matches!(self.type_of(main)?, Type::Func(..)), "main must be a function.");
        } else if self.require_main {
            return program_error!("Missing entrypoint. You must annotate a function with the `main` prefix to provide an entrypoint.");
        }
        self.symbols.pop();
        return Ok(Type::Stmt);
    }

    fn check_block(&mut self, stmts: &Vec<Box<Ast>>) -> Fallible<Type> {
        self.symbols.push(HashMap::new());
        let mut ty = None;
        for stmt in stmts { ty = Some(self.check_ast(stmt)?); }
        self.symbols.pop();

        return match ty {
            None => Ok(Type::Stmt),
            Some(ty) => Ok(ty),
        } 
    }

    fn check_assign(&mut self, var_name: &String, value: &Ast) -> Fallible<Type> {
        let value_type = self.check_ast(value)?;
        let name_type = self.type_of(var_name)?;
        if name_type != value_type { return program_error!(
            "Expected {}, found {}", name_type.to_string(), value_type.to_string()) 
        };
        return Ok(Type::Stmt);
    }

    fn check_index_set(&mut self, array: &String, idx: &Ast, value: &Ast) -> Fallible<Type> {
        let idx_type = self.check_ast(idx)?;
        let value_type = self.check_ast(value)?;
        let array_type = self.type_of(array)?;

        if !matches!(idx_type, Type::Basic(Token::IntTy)) {return program_error!(
            "Index must be an int, found {}", idx_type.to_string()
        )};
        let Type::Array(item_type) = array_type else { return program_error!(
            "Can only index into array. Found {}.", array_type.to_string()); 
        };
        if *item_type != value_type { return program_error!(
            "Expected {}, found {}", item_type.to_string(), value_type.to_string()) 
        };

        return Ok(Type::Stmt);
    }

    fn check_struct_set(&mut self, struct_: &String, var: &String, value: &Ast) -> Fallible<Type> {
        let value_type = self.check_ast(value)?;
        let struct_type = self.full_type_of(struct_)?;

        let Type::Struct(declared_name, content) = struct_type else { return program_error!(
            "Expected struct in `{}.{}`. Found {}.", struct_, var, struct_type.to_string()
        )};
        dbg_assert!(declared_name == struct_);

        match content.get(var) {
            None => return program_error!("{struct_} has no member {var}"),
            Some(var_type) => if *var_type != value_type { return program_error!(
                "Expected {}, found {}", var_type.to_string(), value_type.to_string()
            )},
        };
        
        return Ok(Type::Stmt);
    }

    fn check_init(&mut self, decl: &Ast, value: &Ast) -> Fallible<Type> {
        dbg_assert!(matches!(decl, Ast::Decl{..}));

        let value_type = self.check_ast(value)?;
        self.check_ast(decl)?; // could evt. get decl ty here

        let Ast::Decl { name, type_ } = decl else { unreachable!("Must receive declaration."); };
        let type_ = &**type_;
        let type_ = match type_ {
            Type::Struct(struct_name, _) => &self.type_of(struct_name)?,
            _ => type_,
        };

        if *type_ != value_type { return program_error!(
            "Expected {}, found {}", type_.to_string(), value_type.to_string()) 
        };
        
        return Ok(Type::Stmt);
    }

    fn check_decl(&mut self, name: &String, type_: &Type) -> Fallible<Type> {
        let type_ = match type_ {
            // this compensates for the fact that struct and declared with incomplete type in parser.
            Type::Struct(struct_name, _) => &self.full_type_of(struct_name)?,
            _ => type_,
        };
        self.make_name(name.clone(), type_.clone())?;
        return Ok(Type::Stmt); 
    }

    fn check_fn_decl(&mut self, name: &String, args: &Vec<Box<Ast>>, ret_type: &Type, body: &Ast) -> Fallible<Type> {
        // we permit shadowing
        self.symbols.push(HashMap::new());
        let mut args_types: Vec<Box<Type>> = Vec::with_capacity(args.len());
        for arg in args {
            let Ast::Decl{type_, ..} = *arg.clone() else { unreachable!() };
            self.check_ast(&*arg)?;
            args_types.push(type_);
        }

        self.return_type = None;
        self.in_func = true;
        self.check_ast(body)?; // body_type unused, because reconstructed from recorded return.
        self.in_func = false;

        let body_type = match self.return_type.clone() {
            Some(body_return_type) => body_return_type,
            None => Type::Stmt,
        };
        self.return_type = None;
        self.symbols.pop();

        if *ret_type != body_type { return program_error!(
            "Expected {}, found {}", ret_type.to_string(), body_type.to_string());
        }

        let func_type = Type::Func(args_types, Box::new(ret_type.clone()));
        self.make_name(name.clone(), func_type)?;

        return Ok(Type::Stmt); // evt. return func_type
    }
    
    fn check_extern_decl(&mut self, name: &String, type_: &Type) -> Fallible<Type> {
        self.make_name(name.clone(), type_.clone())?;
        return Ok(Type::Stmt); // evt. return type_
    }

    fn check_extern_fn_decl(&mut self, name: &String, args: &Vec<Box<Ast>>, ret_type: &Type) -> Fallible<Type> {
        // copy of check_fn_decl
        self.symbols.push(HashMap::new());
        let mut args_types: Vec<Box<Type>> = Vec::with_capacity(args.len());
        for arg in args {
            let Ast::Decl{type_, ..} = *arg.clone() else { unreachable!() };
            self.check_ast(&*arg)?;
            args_types.push(type_);
        }
        self.symbols.pop();

        let func_type = Type::Func(args_types, Box::new(ret_type.clone()));
        self.make_name(name.clone(), func_type)?;

        return Ok(Type::Stmt); // evt. return func_type
    }

    fn check_struct_decl(&mut self, struct_name: &String, content: &Vec<Box<Ast>>) -> Fallible<Type> {

        self.symbols.push(HashMap::new());
        let mut content_types: IndexMap<String, Type> = IndexMap::with_capacity(content.len());
        for var in content {
            let Ast::Decl{name: var_name, type_} = *var.clone() else { unreachable!() };
            let short_type = if let Type::Struct(n, _) = *type_ {Type::Struct(n, IndexMap::new())} else {*type_};
        
            self.check_ast(&*var)?;
            content_types.insert(var_name, short_type);
        }
        self.symbols.pop();

        let struct_type = Type::Struct(struct_name.clone(), content_types);
        self.make_name(struct_name.clone(), struct_type)?;

        return Ok(Type::Stmt); // evt. return struct_type
    }

    fn check_return(&mut self, value: &Option<Box<Ast>>) -> Fallible<Type> {
        if !self.in_func { return program_error!(
            "Cannot use return outside of a function."
        )};
        let ty = match value {
            None => Type::Stmt,
            Some(value) => self.check_ast(value)?,
        };
        self.return_type = Some(ty.clone());
        return Ok(ty);
    }

    fn check_for(&mut self, item: &String, idx: &Option<String>, array: &Ast, step: &Ast) -> Fallible<Type> {
        // we allow item, idx to shadow

        let array_type = self.check_ast(array)?; 
        let Type::Array(item_type) = array_type else {return program_error!(
            "Can only loop over array. Found {}", array_type.to_string()
        )};
        let idx_str = idx.clone().map_or("i".to_string(), |idx| idx);

        // create names
        let shadowed_by_item = self.make_name(item.clone(), *item_type.clone())?;
        let shadowed_by_idx = self.make_name(idx_str.clone(), Token::IntTy.into())?;
    
        self.in_loop = true;
        self.check_ast(step)?;
        self.in_loop = false;
        // we dont check that step_type is stmt, since expr is treated as a sub catagory of stmt

        // cleanup names
        if let Some(old) = shadowed_by_item { self.make_name(item.clone(), old)?; }; 
        if let Some(old) = shadowed_by_idx  { self.make_name(idx_str, old)?; }; 
        return Ok(Type::Stmt);
    }

    fn check_while(&mut self, condition: &Ast, step: &Ast) -> Fallible<Type> {
        let condition_type = self.check_ast(condition)?; 

        self.in_loop = true;
        let step_type = self.check_ast(step)?; 
        self.in_loop = false;

        if !matches!(condition_type, Type::Basic(Token::BoolTy)) {return program_error!(
            "expected boolean condition in while loop. Found {}", condition_type.to_string());
        }
        if !matches!(step_type, Type::Stmt) {return program_error!(
            "The loop body must be a statement, not an expression. Found {}", step_type.to_string());
        }
        return Ok(Type::Stmt);
    }

    fn check_if_else(&mut self, condition: &Ast, then: &Ast, else_: &Option<Box<Ast>>) -> Fallible<Type> {
        let condition_type = self.check_ast(condition)?; 
        let then_type = self.check_ast(then)?; 

        if !matches!(condition_type, Type::Basic(Token::BoolTy)) {return program_error!(
            "expected boolean condition in while loop. Found {}", condition_type.to_string());
        }
        if let Some(else_) = else_ {
            let else_type = self.check_ast(else_)?; 
            if then_type != else_type {return program_error!(
                r"The do and else branches of an if expression must have the same type. 
                Found {} and {}", then_type.to_string(), else_type.to_string());
            }
        }
    
        return Ok(then_type);
    }



    // ------------------ expr ------------------ //

    fn check_binary(&mut self, op: &Token, left: &Ast, right: &Ast) -> Fallible<Type> {
        dbg_assert!(op.is_binary_op());
        dbg_assert!(left.is_expr());
        dbg_assert!(right.is_expr());
        let left_type = self.check_ast(left)?; 
        let right_type = self.check_ast(right)?;
        
        use Token::*;
        match op {
            Add | Minus | Mul | Div | Pow | Gt | Gte | Lte | Lt => {
                if !matches!(left_type, Type::Basic(IntTy | FloatTy)) {return program_error!(
                    "Expected int or float as operands to {}. Found {} and {}", op, left_type.to_string(), right_type.to_string()
                );}
            },
            And | Or => {
                if !matches!(left_type, Type::Basic(BoolTy)) {return program_error!(
                    "Expected bool as operands to {}. Found {} and {}", op, left_type.to_string(), right_type.to_string()
                );}
            },
            Eq | Neq => {},
            As => { }, // cast is handled separately
            _ => unreachable!("Binary operator must be binary operator."),
        };

        if left_type != right_type {
            let left_is_number = matches!(left_type, Type::Basic(IntTy | FloatTy));
            let right_is_number = matches!(right_type, Type::Basic(IntTy | FloatTy));
            if !(left_is_number && right_is_number) { return program_error!(
                "Expected that as operands to {} has the same type. Found {} and {}", op, left_type.to_string(), right_type.to_string()
            );
        }}
        
        let ret_type = match op {
            Add | Minus | Mul | Div | Pow => left_type,
            Gt | Gte | Eq | Neq | Lte | Lt | And | Or => Type::Basic(BoolTy),
            _ => unreachable!("Binary operator must be binary operator."),
        };
        return Ok(ret_type);
    }

    fn check_unary(&mut self, op: &Token, arg: &Ast) -> Fallible<Type> {
        dbg_assert!(op.is_prefix_op() || op.is_postfix_op());
        dbg_assert!(arg.is_expr());
        let arg_type = self.check_ast(arg)?;

        use Token::*;
        match op {
            Minus => {
                if !matches!(arg_type, Type::Basic(IntTy | FloatTy)) {return program_error!(
                    "Expected int or float as operand to {}. Found {}", op, arg_type.to_string()
                );}
            },
            Not => {
                if !matches!(arg_type, Type::Basic(BoolTy)) {return program_error!(
                    "Expected bool as operand to {}. Found {}", op, arg_type.to_string()
                );}
            },
            PtrOut | PtrIn => {},
            _ => unreachable!("Unary operator must be unary operator."),
        };
        
        let ret_type = match op {
            Minus | Not => arg_type,
            PtrIn => match arg_type {
                Type::PtrOut(from_) => *from_,
                _ => Type::PtrIn(Box::new(arg_type)),
            },
            PtrOut => match arg_type {
                Type::PtrIn(to) => *to,
                _ => Type::PtrOut(Box::new(arg_type)),
            },
            _ => unreachable!("Unary operator must be unary operator."),
        };
        return Ok(ret_type);
    }

    fn check_call(&mut self, func_name: &String, args: &Vec<Box<Ast>>) -> Fallible<Type> {
        let func_type = self.type_of(func_name)?.clone();
        
        let Type::Func(expected_args_types, ret_type) = func_type else { return program_error!(
            "Expected function to call. Found {}", func_type.to_string()
        )};
        
        let mut args_types: Vec<Box<Type>> = Vec::with_capacity(args.len());
        for arg in args {
            dbg_assert!(arg.is_expr());
            let arg_type = self.check_ast(&*arg)?;
            args_types.push(Box::new(arg_type));
        }

        if args_types != *expected_args_types { return program_error!(
            "Expected {} as input to {}. found {}.", 
            Type::vec_box_to_string(args_types), 
            func_name,
            Type::vec_box_to_string(expected_args_types), 
        )};

        return Ok(*ret_type);
    }
    
    fn check_index_get(&mut self, array: &String, idx: &Ast) -> Fallible<Type> {
        let array_type = self.type_of(array)?.clone();
        let idx_type = self.check_ast(idx)?;
        
        if !matches!(idx_type, Type::Basic(Token::IntTy)) { return program_error!(
            "Index should be an int, Found {}", idx_type.to_string()
        )};
        let Type::Array(item_type) = array_type else { return program_error!(
            "Can only Index into Array. Found {}", array_type.to_string()
        )};
    
        return Ok(*item_type.clone());
    }

    fn check_struct_get(&mut self, struct_inst: &String, var: &String) -> Fallible<Type> {
        let struct_type = self.full_type_of(struct_inst)?;

        let Type::Struct(type_name, content) = struct_type else { return program_error!(
            "Expected struct in `{}.{}`. Found {}.", struct_inst, var, struct_type.to_string()
        )};

        match content.get(var) {
            None => return program_error!("{struct_inst} has no member {var}"),
            Some(var_type) => return Ok(var_type.clone()),
        };
    }

    fn check_cast(&mut self, from: &Ast, to: &Type) -> Fallible<Type> {
        dbg_assert!(from.is_expr());
        let from_ty = self.check_ast(from)?;
        
        use Type::*;
        use Token::{BoolTy, IntTy, FloatTy, StrTy, Nil};
        // Nil may be cast to ptr
        if from_ty == Basic(Nil) && matches!(to, PtrIn(_)) { return Ok(to.clone()); }

        fn allowed_targets(from_type: &Type) -> Vec<Type> { match from_type {
            Array(ty)  => allowed_targets(ty).iter().map(|tar| Array (Box::new(tar.clone()))).collect(),
            PtrIn(ty)  => allowed_targets(ty).iter().map(|tar| PtrIn (Box::new(tar.clone()))).collect(),
            PtrOut(ty) => allowed_targets(ty).iter().map(|tar| PtrOut(Box::new(tar.clone()))).collect(),
            Stmt | Func(..) | Struct(..) => Vec::new(),
            Basic(tok) => { match tok.clone() {
                BoolTy  => vec![Basic(BoolTy), Basic(StrTy)],
                IntTy   => vec![Basic(BoolTy), Basic(IntTy), Basic(FloatTy), Basic(StrTy)],
                FloatTy => vec![Basic(IntTy), Basic(FloatTy), Basic(StrTy)],
                StrTy   => vec![Basic(StrTy)],
                // Nil     => vec![], Nil is already checked
                _ => unreachable!(),
            }},
        }}

        if !allowed_targets(&from_ty).contains(&to) { return program_error!(
            "Cannot cast {} to {}", from_ty.to_string(), to.to_string()
        )};
        
        return Ok(to.clone());
    }

    fn check_print(&mut self, expr: &Ast) -> Fallible<Type> {
        dbg_assert!(expr.is_expr());
        let ty = self.check_ast(expr)?;

        if self.types.iter().map(|t| &t.0).collect::<Vec<&Ast>>().contains(&&expr) { return program_error!(
            "Cannot print two variables with the same AST. Attempted to print {:?} and {:?}", expr, expr
        )};
        self.types.push((expr.clone(), ty.clone()));
        return Ok(ty);
    }

    fn check_array_literal(&mut self, content: &Vec<Box<Ast>>) -> Fallible<Type> {
        if content.is_empty() { return Ok(Type::Array(Box::new(Type::Basic(Token::Nil)))); }

        dbg_assert!(content[0].is_expr());
        let first_item_type = self.check_ast(&*content[0])?;

        for item in content[1..].iter() {
            dbg_assert!(item.is_expr());
            let item_type = self.check_ast(&*item)?;
            if item_type != first_item_type { return program_error!(
                "Every element of array literal must have the same type. Found {} and {}.",
                item_type.to_string(), first_item_type.to_string()
            )};
        }
        self.types.push((*content[0].clone(), Type::Array(Box::new(first_item_type.clone()))));
        return Ok(Type::Array(Box::new(first_item_type)));
    }

    fn check_struct_literal(&mut self, name: &String, content: &IndexMap<String, Ast>) -> Fallible<Type> {
        
        let mut name_to_type: IndexMap<String, Type> = IndexMap::with_capacity(content.len());
        for (var_name, var_value) in content {
            dbg_assert!(var_value.is_expr());
            let var_type = self.check_ast(&*var_value)?;
            let short_var_type = if let Type::Struct(n, _) = var_type {Type::Struct(n, IndexMap::new())} else {var_type};
            name_to_type.insert(var_name.clone(), short_var_type);
        }

        let expected_type = self.full_type_of(name)?;
        let actual_full_type = Type::Struct(name.clone(), name_to_type);

        if actual_full_type != *expected_type {return program_error!(
            "Expected {}. Found {}", expected_type.to_string(), actual_full_type.to_string()
        )};

        let actual_short_type = Type::Struct(name.clone(), IndexMap::new());
        return Ok(actual_short_type);
    }

    fn check_terminal(&mut self, token: &Token) -> Fallible<Type> {
        dbg_assert!(token.is_terminal());
        use Token::*;
        let tok_ty = match token {
            BoolLit(_)  => Type::Basic(BoolTy),
            IntLit(_)   => Type::Basic(IntTy),
            FloatLit(_) => Type::Basic(FloatTy),
            StrLit(_)   => Type::Basic(StrTy),
            Nil         => Type::Basic(Nil),
            Ident(name) => self.type_of(name)?.clone(),
            Break => {
                if !self.in_loop { return program_error!("Can only break inside a loop.");}
                Type::Stmt
            },
            Continue => {
                if !self.in_loop { return program_error!("Can only continue inside a loop.");}
                Type::Stmt
            },
            _ => unreachable!("Invalid Terminal."),
        };
        return Ok(tok_ty);
    }



    // ------------------ helpers ------------------ //
    fn type_of(&self, name: &str) -> Fallible<Type> {
        for block in self.symbols.iter().rev() {
            if block.contains_key(name) {
                let shorted_type = match &block[name] {
                    Type::Struct(name, _) => Type::Struct(name.clone(), IndexMap::new()),
                    ty => ty.clone(), 
                };
                return Ok(shorted_type);
            }
        }
        return program_error!("The name '{name}' is unrecognized.");
    }

    fn full_type_of(&self, name: &str) -> Fallible<&Type> {
        for block in self.symbols.iter().rev() {
            if block.contains_key(name) {
                return Ok(&block[name]);
            }
        }
        return program_error!("The name '{name}' is unrecognized.");
    }


    fn make_name(&mut self, name: String, type_: Type) -> Fallible<Option<Type>> {
        let block = self.symbols.last_mut().expect("is never empty");
        // we prohibit shadowing in a block for easier c generation (avoids unique name creation)
        if block.contains_key(&name) {
            return program_error!("The name '{name}' already exists in this block.");
        }
        return Ok(block.insert(name, type_));
    }

}