use std::{
    cell::RefCell,
    error::Error,
    fmt::Display,
    fs::File,
    io::{BufReader, Read},
    rc::Rc,
};

use crate::{
    ast::{SpanInterval, AST},
    core::{Lexer, Parser},
    error::{LexicalError, RuntimeError, SyntaxError},
    eval::Context,
    prelude::TypeCache,
    types::Value,
};

use super::Environment;

#[derive(Debug)]
pub enum EngineError {
    SyntaxError(SyntaxError),
    LexicalError(LexicalError),
    RuntimeError(RuntimeError),
    Other(Box<dyn Error>),
}

impl Error for EngineError {}
impl Display for EngineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SyntaxError(e) => match e {
                SyntaxError::LogicalError(e) => write!(f, "LogicalError: {e}"),
                _ => write!(f, "SyntaxError: {e}"),
            },
            Self::LexicalError(e) => write!(f, "LexicalError: {e}"),
            Self::RuntimeError(e) => write!(f, "RuntimeError: {e}"),
            Self::Other(e) => write!(f, "Error: {e}"),
        }
    }
}

impl From<SyntaxError> for EngineError {
    fn from(value: SyntaxError) -> Self {
        Self::SyntaxError(value)
    }
}

impl From<LexicalError> for EngineError {
    fn from(value: LexicalError) -> Self {
        Self::LexicalError(value)
    }
}

impl From<RuntimeError> for EngineError {
    fn from(value: RuntimeError) -> Self {
        Self::RuntimeError(value)
    }
}

impl From<Box<dyn Error>> for EngineError {
    fn from(value: Box<dyn Error>) -> Self {
        Self::Other(value)
    }
}

#[derive(Debug, Clone)]
pub struct Engine {
    pub ast: AST,
    env: Rc<RefCell<Environment>>,
    pub parser: Parser,
}

impl Default for Engine {
    fn default() -> Self {
        Engine {
            ast: AST::new("script"),
            env: Rc::new(RefCell::new(Environment::new())),
            parser: Parser::new(vec![], TypeCache::new_empty()),
        }
    }
}

impl Engine {
    pub fn new() -> Engine {
        Engine::default()
    }

    pub fn from_source(source: &str) -> Result<Self, Box<dyn Error>> {
        let lex = Lexer::new(source).lex()?;
        let mut parser = Parser::new(lex, TypeCache::new_empty());
        let ast = parser.parse_ast()?;

        Ok(Self {
            ast,
            env: Rc::new(RefCell::new(Environment::new())),
            parser,
        })
    }

    pub fn load_source(&mut self, source: &str) -> Result<(), EngineError> {
        let lex = Lexer::new(source).lex()?;

        self.parser.load_tokens(lex);
        let ast = self.parser.parse_ast()?;

        self.ast = ast;
        Ok(())
    }

    pub fn load_file(&mut self, filepath: &str) -> Result<bool, EngineError> {
        let Ok(file) = File::open(filepath) else {
            return Ok(false);
        };

        let mut buf_reader = BufReader::new(file);
        let mut source = String::with_capacity(1024);
        buf_reader
            .read_to_string(&mut source)
            .map_err(|e| EngineError::Other(e.into()))?;

        let lex = Lexer::new(source).lex()?;

        self.parser.load_tokens(lex);
        let ast = self.parser.parse_ast()?;

        self.ast = ast;

        Ok(true)
    }

    pub fn context(&self) -> Context {
        Context::new(SpanInterval::EOF, self.env.clone())
    }

    pub fn eval_ast(&self) -> Result<Value, RuntimeError> {
        self.ast.eval_ast(self.context())
    }
}
