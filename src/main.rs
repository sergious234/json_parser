use parser::JsonParser;
use tokenizer::{JsonTokenizer, Token, TokenType};

mod tokenizer {
    use std::{
        io::{Bytes, Read},
        iter::Peekable,
    };

    #[allow(dead_code, clippy::upper_case_acronyms)]
    #[derive(Debug)]
    pub enum Token {
        Bool(bool),
        Num(f64),
        String(String),
        Colon,
        Comma,
        LBracket,
        RBracket,
        LCurly,
        RCurly,
        White,
        Text(String),
        EOF,
    }

    #[allow(clippy::upper_case_acronyms)]
    #[derive(Debug, PartialEq, Eq)]
    pub enum TokenType {
        Bool,
        Num,
        String,
        Colon,
        Comma,
        LBracket,
        RBracket,
        LCurly,
        RCurly,
        White,
        Text,
        EOF,
    }

    impl Token {
        pub fn kind(&self) -> TokenType {
            match self {
                Token::Bool(_) => TokenType::Bool,
                Token::Num(_) => TokenType::Num,
                Token::String(_) => TokenType::String,
                Token::Colon => TokenType::Colon,
                Token::Comma => TokenType::Comma,
                Token::LBracket => TokenType::LBracket,
                Token::RBracket => TokenType::RBracket,
                Token::LCurly => TokenType::LCurly,
                Token::RCurly => TokenType::RCurly,
                Token::White => TokenType::White,
                Token::Text(_) => TokenType::Text,
                Token::EOF => TokenType::EOF,
            }
        }
    }

    pub struct JsonTokenizer<R: Read> {
        lexer: Lexer<R>,
    }

    impl<R: Read> JsonTokenizer<R> {
        pub fn new(reader: R) -> Self {
            JsonTokenizer {
                lexer: Lexer::new(reader),
            }
        }

        pub fn get_position(&self) -> (usize, usize) {
            (self.lexer.col, self.lexer.position)
        }
        pub fn next_token(&mut self) -> Option<Token> {
            self.lexer.next_token()
        }

        fn tokenize(c: &str) -> Option<Token> {
            match c {
                "[" => Some(Token::LBracket),
                "]" => Some(Token::RBracket),
                "{" => Some(Token::LCurly),
                "}" => Some(Token::RCurly),
                "true" => Some(Token::Bool(true)),
                "false" => Some(Token::Bool(false)),
                ":" => Some(Token::Colon),
                "," => Some(Token::Comma),
                s if s.starts_with('"') && s.ends_with('"') && s.len() > 2 => {
                    Some(Token::String(s.to_owned()))
                }
                s if s.trim().is_empty() => Some(Token::White),
                s => {
                    if let Ok(n) = s.parse::<f64>() {
                        Some(Token::Num(n))
                    } else {
                        None
                    }
                }
            }
        }
    }

    pub struct Lexer<R: Read> {
        bytes: Peekable<Bytes<R>>,
        buff: Option<char>,
        pub position: usize,
        pub col: usize,
    }

    impl<R: Read> Lexer<R> {
        pub fn new(reader: R) -> Self {
            Self {
                bytes: reader.bytes().peekable(),
                buff: None,
                position: 1,
                col: 1,
            }
        }

        fn get_kind(str: &str) -> Option<Token> {
            match str {
                "true" => Some(Token::Bool(true)),
                "false" => Some(Token::Bool(false)),
                _ => Some(Token::Text(str.to_string())),
            }
        }

        fn parse_identifier(&mut self) -> Option<Token> {
            let mut name = String::new();

            loop {
                let c = self.get_next_char()?;
                if c == '"' {
                    return Some(Token::String(name.to_string()));
                }
                name.push(c)
            }
        }

        fn parse_text(&mut self, first: char) -> Option<Token> {
            let mut text = String::from(first);

            loop {
                let c = self.get_next_char()?;
                self.buff = Some(c);
                if !c.is_alphanumeric() {
                    return Self::get_kind(&text);
                }
                text.push(c)
            }
        }

        fn parse_number(&mut self, first: char) -> Option<Token> {
            let mut text = String::from(first);

            loop {
                let c = self.get_next_char()?;
                if !c.is_numeric() && c != '.' {
                    self.buff = Some(c);
                    if let Ok(num) = text.parse::<f64>() {
                        return Some(Token::Num(num));
                    } else {
                        return None;
                    }
                }
                text.push(c)
            }
        }

        fn get_next_char(&mut self) -> Option<char> {
            self.position += 1;
            Some(char::from(self.bytes.next()?.ok()?))
        }

        pub fn next_token(&mut self) -> Option<Token> {
            if let Some(c) = self.buff.take() {
                match c {
                    x @ ('a'..='z' | 'A'..='Z') => {
                        return self.parse_text(x);
                    }
                    '"' => {
                        return self.parse_identifier();
                    }
                    x @ '0'..='9' => return self.parse_number(x),
                    ',' => return Some(Token::Comma),
                    '{' => return Some(Token::LCurly),
                    '}' => return Some(Token::RCurly),
                    '[' => return Some(Token::LBracket),
                    ']' => return Some(Token::RBracket),
                    ':' => return Some(Token::Colon),
                    ' ' | '\r' => {}
                    '\n' => {
                        self.col += 1;
                        self.position = 1;
                    }
                    x => {
                        println!("Token: {x}")
                    }
                };
            }

            while let Some(c) = self.get_next_char() {
                match c {
                    x @ ('a'..='z' | 'A'..='Z') => {
                        return self.parse_text(x);
                    }
                    '"' => {
                        return self.parse_identifier();
                    }
                    x @ '0'..='9' => return self.parse_number(x),
                    ',' => return Some(Token::Comma),
                    '{' => return Some(Token::LCurly),
                    '}' => return Some(Token::RCurly),
                    '[' => return Some(Token::LBracket),
                    ']' => return Some(Token::RBracket),
                    ':' => return Some(Token::Colon),
                    ' ' | '\r' => {}
                    '\n' => {
                        self.col += 1;
                        self.position = 0;
                    }
                    x => {
                        println!("Token: {x}")
                    }
                };
            }
            Some(Token::EOF)
        }
    }
}

mod parser {
    use std::{collections::HashMap, io::Read};

    use crate::tokenizer::{JsonTokenizer, Lexer, Token, TokenType};

    type Result<T> = std::result::Result<T, String>;

    #[allow(dead_code)]
    #[derive(Debug)]
    pub enum ASTElement {
        List(Vec<ASTElement>),
        Object(HashMap<String, ASTElement>),
        Number(f64),
        String(String),
        Bool(bool),
    }

    pub struct JsonParser<R: Read> {
        tokenizer: JsonTokenizer<R>,
        next_token: Token,
    }

    impl<R: Read> JsonParser<R> {
        pub fn new(reader: R) -> Self {
            let mut tokenizer = JsonTokenizer::new(reader);
            let next_token = tokenizer.next_token().unwrap();

            Self {
                tokenizer,
                next_token,
            }
        }

        fn match_token(&mut self, token_type: TokenType) -> Result<Token> {
            if self.next_token.kind() == token_type {
                let next_token = self
                    .tokenizer
                    .next_token()
                    .ok_or("No more tokens".to_owned())?;

                let old_token = std::mem::replace(&mut self.next_token, next_token);
                Ok(old_token)
            } else {
                Err(format!(
                    "Error, expectec {token_type:?} got {:?}",
                    self.next_token.kind()
                ))
            }
        }

        pub fn parse(mut self) -> Result<ASTElement> {
            match self.next_token {
                Token::LCurly => {
                    self.match_token(TokenType::LCurly)?;
                    self.parse_object()
                }
                Token::LBracket => {
                    self.match_token(TokenType::LBracket)?;
                    self.parse_array()
                }
                _ => panic!("Error parsing"),
            }
        }

        pub fn parse_object(&mut self) -> Result<ASTElement> {
            let mut object = ASTElement::Object(HashMap::new());
            let fields = if let ASTElement::Object(ref mut fields) = object {
                fields
            } else {
                unreachable!();
            };

            if self.next_token.kind() == TokenType::RCurly {
                self.match_token(TokenType::RCurly)?;
                return Ok(object);
            }

            let (field_name, value) = self.parse_field()?;
            fields.insert(field_name, value);

            let object = self.parse_next_object_field(object)?;

            Ok(object)
        }

        pub fn parse_next_object_field(&mut self, mut object: ASTElement) -> Result<ASTElement> {
            let fields = if let ASTElement::Object(ref mut fields) = object {
                fields
            } else {
                unreachable!();
            };

            if self.next_token.kind() == TokenType::RCurly {
                self.match_token(TokenType::RCurly)?;
                return Ok(object);
            }

            self.match_token(TokenType::Comma)?;

            let (field_name, value) = self.parse_field()?;

            fields.insert(field_name, value);
            self.parse_next_object_field(object)
        }

        pub fn parse_field(&mut self) -> Result<(String, ASTElement)> {
            let field_name = match &self.next_token {
                Token::String(f_name) => {
                    let field_name = f_name.clone();
                    self.match_token(TokenType::String)?;
                    self.match_token(TokenType::Colon)?;
                    field_name
                }
                _ => Err("Expected field name".to_string())?,
            };

            let value = match self.next_token.kind() {
                TokenType::String => {
                    let old = self.match_token(TokenType::String)?;
                    if let Token::String(v) = old {
                        ASTElement::String(v)
                    } else {
                        unreachable!()
                    }
                }

                TokenType::Num => {
                    let old = self.match_token(TokenType::Num)?;
                    if let Token::Num(v) = old {
                        ASTElement::Number(v)
                    } else {
                        unreachable!()
                    }
                }

                TokenType::Bool => {
                    let old = self.match_token(TokenType::Bool)?;
                    if let Token::Bool(v) = old {
                        ASTElement::Bool(v)
                    } else {
                        unreachable!()
                    }
                }

                TokenType::LBracket => {
                    self.match_token(TokenType::LBracket)?;
                    self.parse_array()?
                }

                TokenType::LCurly => {
                    self.match_token(TokenType::LCurly)?;
                    self.parse_object()?
                }

                other => Err(format!("Expected field value, found {other:?}"))?,
            };
            Ok((field_name, value))
        }

        pub fn parse_array(&mut self) -> Result<ASTElement> {
            let list = ASTElement::List(vec![]);
            self.parse_next_array_element(list)
        }

        pub fn parse_next_array_element(&mut self, mut list: ASTElement) -> Result<ASTElement> {
            if let ASTElement::List(ref mut v) = list {
                match &self.next_token {
                    Token::String(s) => {
                        let copy = s.clone();
                        self.match_token(TokenType::String)?;
                        v.push(ASTElement::String(copy));
                    }

                    Token::LCurly => {
                        self.match_token(TokenType::LCurly)?;
                        let object = self.parse_object()?;
                        v.push(object);
                    }

                    Token::LBracket => {
                        self.match_token(TokenType::LBracket)?;
                        let in_list = self.parse_array()?;
                        v.push(in_list);
                    }

                    Token::RBracket => Err(format!(
                        "Failed to parse array, trailing comma detected in position: {:?}",
                        self.tokenizer.get_position()
                    ))?,

                    Token::Bool(b) => {
                        v.push(ASTElement::Bool(*b));
                        self.match_token(TokenType::Bool)?;
                    }

                    Token::Num(n) => {
                        v.push(ASTElement::Number(*n));
                        self.match_token(TokenType::Num)?;
                    }

                    //Token::Comma => {
                    //    Err(format!("Failed to parse array, trailing comma detected in position: {}", self.tokenizer.get_position()))?
                    //}
                    other => Err(format!("Failed to parse array, found: {other:?}"))?,
                };
            }

            match &self.next_token {
                Token::Comma => {
                    self.match_token(TokenType::Comma)?;
                    self.parse_next_array_element(list)
                }

                Token::RBracket => {
                    self.match_token(TokenType::RBracket)?;
                    Ok(list)
                }
                other => Err(format!(
                    "Failed to parse array, found: {other:?} in position {:?}",
                    self.tokenizer.get_position()
                ))?,
            }
        }
    }
}

fn main() {
    let reader = std::fs::File::open("files/test3.json").unwrap();
    let parser = JsonParser::new(reader);
    let ast = parser.parse();
    println!("{ast:?}")
}

#[cfg(test)]
mod tests {
    use super::*;


    #[test]
    fn test4() {
        let reader = std::fs::File::open("files/test4.json").unwrap();
        let parser = JsonParser::new(reader);
        let ast = parser.parse();
        assert!(ast.is_err())
    }

    #[test]
    fn test3() {
        let reader = std::fs::File::open("files/test3.json").unwrap();
        let parser = JsonParser::new(reader);
        let ast = parser.parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn test2() {
        let reader = std::fs::File::open("files/test2.json").unwrap();
        let parser = JsonParser::new(reader);
        let ast = parser.parse();
        assert!(ast.is_ok())
    }

    #[test]
    fn test1() {
        let reader = std::fs::File::open("files/test1.json").unwrap();
        let parser = JsonParser::new(reader);
        let ast = parser.parse();
        assert!(ast.is_ok())
    }
}
