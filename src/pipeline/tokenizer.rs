use std::collections::HashMap;
use std::sync::LazyLock;
use super::Token;
use crate::{brk, brk_if, dbg_assert, program_error, Fallible, P};


static reserved_words: LazyLock<HashMap<&str, Token>> = LazyLock::new(|| HashMap::from([
    ("fn",      Token::Fn),
    ("bool",    Token::BoolTy),
    ("int",     Token::IntTy),
    ("float",   Token::FloatTy),
    ("str",     Token::StrTy),
    ("and",     Token::And),
    ("or",      Token::Or),
    ("true",    Token::BoolLit(true)),
    ("false",   Token::BoolLit(false)),
    ("main",    Token::Main),
    ("P",       Token::Print),
    ("if",      Token::If),
    ("else",    Token::Else),
    ("while",   Token::While),
    ("do",      Token::Do),
    ("for",     Token::For),
    ("in",      Token::In),
    ("return",  Token::Return),
    ("break",   Token::Break),
    ("continue",Token::Continue),
    ("extern",  Token::Extern),
    ("import",  Token::Import),
    ("nil",     Token::Nil),
    ("struct",  Token::Struct),
    ("as",      Token::As),    
]));

pub struct Tokenizer { 
    pub idx: usize, 
    pub source: Vec<char>, 
    // indention: usize, 
    // line_nr, char_nr
    #[cfg(debug_assertions)] prev_idx: usize 
}

impl Tokenizer {
    pub fn from(source: String) -> Self {
        let source_chars: Vec<char> = source.chars().collect();
        return Tokenizer { idx: 0, source: source_chars, #[cfg(debug_assertions)] prev_idx: 0 };
    }

    pub fn tokenize(source: String) -> Fallible<Vec<Token>> {
        let mut tokenizer = Tokenizer::from(source);
        let mut toks: Vec<Token> = Vec::with_capacity(32);

        loop {
            let tok = tokenizer.get_tok()?;
            toks.push(tok);
            if toks.last() == Some(&Token::End) { break; }
        };

        return Ok(toks);
    }
    
    pub fn get_tok(&mut self) -> Fallible<Token> {
        // P!(self.idx);
        dbg_assert!(self.idx <= self.source.len()); // we allow it to hit .len in order to catch End
        
        if self.idx >= self.source.len() { return Ok(Token::End); }
        self.step_while(|c|c.is_whitespace());
        // self.step_while(|c|c == ' ');
        if self.idx >= self.source.len() { return Ok(Token::End); }
        // handle comments
        while self.source[self.idx] == '#' {
            self.step_while(|c|c != '\r' && c != '\n');
            self.step_while(|c|c.is_whitespace());
            if self.idx >= self.source.len() { return Ok(Token::End); }
        }

        let char_: char = self.source[self.idx];
        dbg_assert!(!char_.is_whitespace());
        
        let tok = match char_ {
            // operators
            '+' => self.step_get(Token::Add),
            '-' => self.step_get(Token::Minus),
            // '*' => self.step_get(Token::Mul),
            '/' => self.step_get(Token::Div),
            '*' => if self.step_if_next_is('*') { self.step_get(Token::Pow) } else { self.step_get(Token::Mul) },
            // '^' => self.step_get(Token::PtrOut),
            '>' =>   if self.step_if_next_is('=') { self.step_get(Token::Gte) } 
                else if self.step_if_next_is('>') { self.step_get(Token::PtrIn) } 
                else                              { self.step_get(Token::Gt) },
            '<' =>   if self.step_if_next_is('=') { self.step_get(Token::Lte) } 
                else if self.step_if_next_is('<') { self.step_get(Token::PtrOut) } 
                else                              { self.step_get(Token::Lt) },
            '!' => if self.step_if_next_is('=') { self.step_get(Token::Neq) } else { self.step_get(Token::Not) },
            '=' => if self.step_if_next_is('=') { self.step_get(Token::Eq ) } else { self.step_get(Token::AssignOp) },
            ':' => if self.step_if_next_is('=') { self.step_get(Token::InitOp)} else { self.step_get(Token::Colon) },
            ',' => self.step_get(Token::Comma), 
            '.' => self.step_get(Token::Dot), 
            '(' => self.step_get(Token::ParenL),
            ')' => self.step_get(Token::ParenR), 
            '[' => self.step_get(Token::BracketL),
            ']' => self.step_get(Token::BracketR),
            '{' => self.step_get(Token::BlockStart), 
            '}' => self.step_get(Token::BlockEnd), 
            ';' => self.step_get(Token::StmtSep), 
            // strings
            '"' => { self.process_string()? },
            // literals and words
            '0'..='9' => { self.process_number() }, 
            'a'..='z'|'A'..='Z'|'_' => { self.process_word() },
            // '\r'|'\n' => { self.indention = self.process_newline()?; }
            _ => return program_error!("Unrecognized token '{}' at source-idx '{}'", char_, self.idx)
        };

        dbg_assert!(self.idx > self.prev_idx);
        #[cfg(debug_assertions)] {self.prev_idx = self.idx;}
        return Ok(tok);
    }

    // ------ processing ------ //

    fn process_string(&mut self) -> Fallible<Token> {
        self.step_assert('"');
        let str_content = self.step_while(|c| c != '"');
        if self.idx >= self.source.len() {
            return program_error!("Unterminated string literal at index {}", self.idx);
        }
        self.step_assert('"');
        return Ok(Token::StrLit(str_content));
    }

    fn process_number(&mut self) -> Token {
        let mut numberic_str = self.step_while(|c| c.is_digit(10));
        if self.idx >= self.source.len() || self.source[self.idx] != '.' { 
            return Token::IntLit(numberic_str.parse::<i128>().unwrap());
        }
        self.idx += 1;
        let after_dot = self.step_while(|c| c.is_digit(10));
        
        numberic_str.push_str(".");
        numberic_str.push_str(&after_dot);
        return Token::FloatLit(numberic_str.parse::<f64>().unwrap());
    }

    fn process_word(&mut self) -> Token {
        let word = self.step_while(|c| c.is_alphanumeric() || c == '_');
        dbg_assert!(word != "");
        return if reserved_words.contains_key(word.as_str()) { 
            reserved_words[word.as_str()].clone() } 
            else { Token::Ident(word) };
    }



    // ------ helpers ------ //
    fn step_if_next_is(&mut self, c: char) -> bool {
        if self.idx + 1 >= self.source.len() { return false; }
        if self.source[self.idx + 1] != c { return false; }
        self.idx += 1;
        return true;
    }
    fn step_while<F>(&mut self, condition: F) -> String where F: Fn(char) -> bool {
        let num_start_idx = self.idx;
        for &c in &self.source[num_start_idx..] {
            if !condition(c) { break; }
            self.idx += 1; // to next char
        };
        dbg_assert!(self.idx == self.source.len() || !condition(self.source[self.idx]), "");
        let val_str: String = self.source[num_start_idx .. self.idx].iter().collect();
        return val_str;
    }
    
    #[inline] fn step_assert(&mut self, expected: char) {
        dbg_assert!(self.source[self.idx] == expected);
        self.idx += 1;
    }

    #[inline] fn step_get(&mut self, tok: Token) -> Token {
        self.idx += 1;
        return tok;
    }

}