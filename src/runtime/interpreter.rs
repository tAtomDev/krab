use crate::{
    common::{tokens::Operator, ControlFlow, Type, Value},
    core::{
        ast::*,
        lexer::{Lexer, LexicalError},
        parser::{Parser, ParserError},
    },
};

use super::Environment;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum RuntimeError {
    #[error("Lexical Error: {0}")]
    LexicalError(LexicalError),
    #[error("Parser Error: {0}")]
    ParserError(ParserError),

    #[error("Runtime Error: illegal control flow syntax")]
    IllegalControlFlowStatement,

    #[error("Runtime Error: invalid type arithmetic")]
    InvalidType,
    #[error("Runtime Error: expected a boolean")]
    ExpectedBoolean,
    #[error("Runtime Error: expected a valid boolean condition value.")]
    ConditionShouldBeABoolean,
    #[error("Runtime Error: invalid operator provided")]
    InvalidOperator,

    #[error("Runtime Error: unknown type")]
    UnknownType,
    #[error("Runtime Error: incorrect argument type: `{0}` expected")]
    IncorrectArgumentType(Type),
    #[error("Runtime Error: expected {0} arguments, but {1} were found.")]
    IncorrectAmountOfArguments(usize, usize),

    #[error("Runtime Error: expected a valid value")]
    ExpectedAValidValue,
    #[error("Runtime Error: cannot negate this type")]
    CannotNegateThisType,

    #[error("Runtime Error: function `{0}` not found in this scope")]
    FunctionNotFound(String),
    #[error("Runtime Error: cannot redeclare function `{0}`")]
    CannotRedeclareFunction(String),
    #[error("Runtime Error: invalid function body")]
    InvalidFunctionBody,

    #[error("Runtime Error: variable `{0}` not found in this scope")]
    VariableNotFound(String),
    #[error("Runtime Error: cannot redeclare variable `{0}`")]
    CannotRedeclareVariable(String),
    #[error("Runtime Error: cannot assign to constant variable `{0}`")]
    CannotReassignConstVariable(String),
    #[error("Runtime Error: cannot assign a value of a different type to variable `{0}`.")]
    CannotReassignDifferentType(String),
}

#[derive(Debug)]
pub enum EvalResult {
    Value(Value),
    ControlFlow(ControlFlow, Option<Value>),
}

impl EvalResult {
    pub fn parse_value(self) -> Result<Value, RuntimeError> {
        match self {
            Self::Value(value) => Ok(value),
            _ => Err(RuntimeError::ExpectedAValidValue),
        }
    }
}

pub struct Interpreter {
    pub environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new(None),
        }
    }

    pub fn evaluate_source(&mut self, src: &str) -> Result<EvalResult, RuntimeError> {
        let lex = match Lexer::new(src).lex() {
            Ok(lex) => lex,
            Err(e) => return Err(RuntimeError::LexicalError(e)),
        };

        let program = match Parser::new(lex).parse() {
            Ok(program) => program,
            Err(e) => return Err(RuntimeError::ParserError(e)),
        };

        self.evaluate(program)
    }

    pub fn evaluate(&mut self, program: Body) -> Result<EvalResult, RuntimeError> {
        let mut last_value = EvalResult::Value(Value::Nothing);

        for node in program {
            last_value = match node {
                Node::Statement(statement) => {
                    let result = self.evaluate_statement(statement)?;
                    match &result {
                        EvalResult::Value(_) => EvalResult::Value(Value::Nothing),
                        EvalResult::ControlFlow(..) => {
                            let not_within_a_block = self.environment.parent.is_none();
                            if not_within_a_block {
                                return Err(RuntimeError::IllegalControlFlowStatement);
                            }

                            return Ok(result);
                        },
                    }
                }
                Node::Expression(expression) => self.evaluate_expression(expression)?,
                Node::Empty => EvalResult::Value(Value::Nothing),
            };
        }

        Ok(last_value)
    }

    /*
     ** // scope 0
     ** if true {
     **    // scope 1
     **    { let x = 0; /* scope 2 */ }
     ** }
     **
     */
    fn downgrade_environment_scope(&mut self) {
        if let Some(parent) = &self.environment.parent {
            self.environment = (**parent).clone();
        }
    }

    fn upgrade_environment_scope(&mut self) {
        let env = Environment::new(Some(Box::new(self.environment.clone())));
        self.environment = env;
    }

    fn evaluate_statement(&mut self, statement: Statement) -> Result<EvalResult, RuntimeError> {
        let value = match statement {
            Statement::Expression(expression) => self.evaluate_expression(expression)?,
            Statement::VariableDeclaration {
                is_const,
                ty,
                name,
                value_expression,
            } => {
                let value = self.evaluate_expression(*value_expression)?.parse_value()?;
                if ty != value.ty().map_err(|_| RuntimeError::UnknownType)? {
                    return Err(RuntimeError::CannotReassignDifferentType(name));
                }

                self.environment
                    .declare_variable(name, ty, value.clone(), is_const)?;

                EvalResult::Value(value)
            }
            Statement::Assignment(identifier, expression) => {
                let value = self.evaluate_expression(*expression)?.parse_value()?;
                self.environment
                    .assign_variable(identifier, value.clone())?;

                EvalResult::Value(value)
            }
            Statement::Return(expression) => {
                let result = self.evaluate_expression(expression)?.parse_value()?;
                EvalResult::ControlFlow(ControlFlow::Return, Some(result))
            }
            Statement::FunctionDeclaration { name, args, body } => {
                self.environment.declare_function(name, args, body)?;

                EvalResult::Value(Value::Nothing)
            }
        };

        Ok(value)
    }

    fn evaluate_expression(&mut self, expression: Expression) -> Result<EvalResult, RuntimeError> {
        let result = match expression {
            Expression::Literal(literal) => EvalResult::Value(literal.into()),
            Expression::Binary(..) => {
                EvalResult::Value(self.evaluate_binary_expression(expression)?)
            }
            Expression::Body(body) => {
                self.upgrade_environment_scope();
                let res = self.evaluate(body)?;
                self.downgrade_environment_scope();

                res
            }
            Expression::Unary(op, expression) => {
                let evaluated = self.evaluate_expression(*expression)?.parse_value()?;
                if op == Operator::Subtract {
                    EvalResult::Value((-evaluated).map_err(|_| RuntimeError::CannotNegateThisType)?)
                } else if op == Operator::Not {
                    if let Value::Boolean(bool) = evaluated {
                        EvalResult::Value(Value::Boolean(!bool))
                    } else {
                        return Err(RuntimeError::ExpectedBoolean);
                    }
                } else {
                    EvalResult::Value(evaluated)
                }
            }
            Expression::Identifier(identifier) => {
                EvalResult::Value(
                    self.environment
                        .get_variable(&identifier.name)?
                        .value
                        .clone(),
                )
            }
            Expression::If {
                condition,
                body,
                else_branch,
            } => {
                let condition = self.evaluate_expression(*condition)?.parse_value()?;
                let condition = condition
                    .as_boolean()
                    .ok_or(RuntimeError::ConditionShouldBeABoolean)?;

                if condition {
                    return self.evaluate_expression(*body);
                }

                if let Some(else_branch) = else_branch {
                    return self.evaluate_expression(*else_branch);
                }

                EvalResult::Value(Value::Nothing)
            }
            Expression::ControlFlow(flow, expression) => {
                if let Some(expression) = expression {
                    let expression = self.evaluate_expression(*expression)?.parse_value()?;

                    EvalResult::ControlFlow(flow, Some(expression))
                } else {
                    EvalResult::ControlFlow(flow, None)
                }
            }
            Expression::While { condition, body } => {
                let mut while_value = None;
                loop {
                    let condition = self
                        .evaluate_expression(*(condition.clone()))?
                        .parse_value()?;
                    let condition = condition
                        .as_boolean()
                        .ok_or(RuntimeError::ConditionShouldBeABoolean)?;
                    if !condition {
                        break;
                    }

                    match self.evaluate_expression(*(body.clone()))? {
                        EvalResult::Value(value) => {
                            while_value = Some(value);
                        }
                        EvalResult::ControlFlow(flow, value) => {
                            if let Some(value) = value {
                                while_value = Some(value);
                            }

                            match flow {
                                ControlFlow::Break => break,
                                ControlFlow::Continue => continue,
                                ControlFlow::Return => {
                                    return Ok(EvalResult::ControlFlow(
                                        ControlFlow::Return,
                                        while_value,
                                    ))
                                }
                            }
                        }
                    };
                }

                EvalResult::Value(while_value.unwrap_or(Value::Nothing))
            }
            Expression::Call(name, function_args) => {
                let mut args = vec![];
                for arg in function_args {
                    let value = self.evaluate_expression(*arg)?.parse_value()?;
                    let ty = value.ty().map_err(|_| RuntimeError::InvalidType)?;

                    args.push((ty, value));
                }

                // TODO:
                // Function arguments
                // Function return type
                //  ^
                // / \
                //  |
                //  |

                let function = self.environment.get_function(name)?.clone();
                let function_body = *function.body;

                let result = match function_body {
                    Expression::Body(body) => {
                        // Provided insufficient/too much args
                        if function.args.len() != args.len() {
                            return Err(RuntimeError::IncorrectAmountOfArguments(
                                function.args.len(),
                                args.len(),
                            ));
                        }

                        self.upgrade_environment_scope();

                        // Declare function arguments
                        for (func_arg, arg) in function.args.into_iter().zip(args) {
                            // Compare argument type
                            if arg.0 != func_arg.0 {
                                self.downgrade_environment_scope();
                                return Err(RuntimeError::IncorrectArgumentType(func_arg.0));
                            }

                            self.environment
                                .declare_variable(func_arg.1, func_arg.0, arg.1, false)?;
                        }
                        let res = self.evaluate(body)?;
                        self.downgrade_environment_scope();

                        res
                    }
                    _ => return Err(RuntimeError::InvalidFunctionBody),
                };

                match result {
                    EvalResult::Value(value) => EvalResult::Value(value),
                    EvalResult::ControlFlow(flow, value) => {
                        if flow != ControlFlow::Return {
                            return Err(RuntimeError::IllegalControlFlowStatement);
                        }

                        EvalResult::Value(value.unwrap_or(Value::Nothing))
                    }
                }
            }
        };

        Ok(result)
    }

    fn evaluate_binary_expression(
        &mut self,
        expression: Expression,
    ) -> Result<Value, RuntimeError> {
        let Expression::Binary(lhs, op, rhs) = expression else {
            unreachable!()
        };

        let lhs = self.evaluate_expression(*lhs)?.parse_value()?;
        let rhs = self.evaluate_expression(*rhs)?.parse_value()?;

        match op {
            Operator::Add => (lhs + rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Subtract => (lhs - rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Multiply => (lhs * rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Divide => (lhs / rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Modulo => (lhs % rhs).map_err(|_| RuntimeError::InvalidType),
            Operator::Equal => Ok(Value::Boolean(lhs == rhs)),
            Operator::And => {
                let lhs = lhs.as_boolean().ok_or(RuntimeError::ExpectedBoolean)?;
                let rhs = rhs.as_boolean().ok_or(RuntimeError::ExpectedBoolean)?;

                Ok(Value::Boolean(lhs && rhs))
            }
            Operator::Or => {
                let lhs = lhs.as_boolean().ok_or(RuntimeError::ExpectedBoolean)?;
                let rhs = rhs.as_boolean().ok_or(RuntimeError::ExpectedBoolean)?;

                Ok(Value::Boolean(lhs || rhs))
            }
            Operator::Less => Ok(Value::Boolean(lhs < rhs)),
            Operator::LessOrEqual => Ok(Value::Boolean(lhs <= rhs)),
            Operator::Greater => Ok(Value::Boolean(lhs > rhs)),
            Operator::GreaterOrEqual => Ok(Value::Boolean(lhs >= rhs)),
            Operator::NotEqual => Ok(Value::Boolean(lhs != rhs)),
            _ => Err(RuntimeError::InvalidOperator),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs::File,
        io::{BufReader, Read},
    };

    use super::*;

    fn _create_interpreter_and_read_file(file_name: &str) -> (Interpreter, String) {
        let file = File::open(file_name).unwrap();

        let mut buf_reader = BufReader::new(file);
        let mut content = String::with_capacity(1024);

        buf_reader.read_to_string(&mut content).unwrap();

        (Interpreter::new(), content)
    }

    #[test]
    fn basic_math() {
        let (mut interpreter, content) =
            _create_interpreter_and_read_file("./examples/basic_math.krab");
        let evaluated = interpreter
            .evaluate_source(&content)
            .unwrap()
            .parse_value()
            .unwrap();

        assert_eq!(evaluated, Value::Integer(1))
    }

    #[test]
    fn basic_if() {
        let (mut interpreter, content) =
            _create_interpreter_and_read_file("./examples/basic_if.krab");
        let evaluated = interpreter
            .evaluate_source(&content)
            .unwrap()
            .parse_value()
            .unwrap();

        assert_eq!(evaluated, Value::String("10".into()))
    }

    #[test]
    fn basic_logical() {
        let (mut interpreter, content) =
            _create_interpreter_and_read_file("./examples/basic_logical.krab");
        let evaluated = interpreter
            .evaluate_source(&content)
            .unwrap()
            .parse_value()
            .unwrap();

        assert_eq!(evaluated, Value::Boolean(true))
    }
}
