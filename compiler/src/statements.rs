use crate::lexer::{Token, TokenKind, OperatorKind, PunctuationKind};
use crate::parser::{Parser, ParserError};
use crate::ast::Expression; 
use crate::visitor::Visitor;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum PrimitiveType {
    Number,
    String,
    Boolean,
    Void,
    Any
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    VarDeclarationStatement { type_annotation: PrimitiveType, name: String, value: Option<Expression> },
    VarAssignmentStatement { name: String, value: Expression },
    IfStatement { condition: Box<Expression>, valid: Vec<Statement>, invalid: Option<Box<Statement>> },
    WhileStatement { condition: Box<Expression>, body: Vec<Statement> },
    BlockStatement { statements: Vec<Statement> },
    FunctionDeclarationStatement { name: String, parameters: Vec<(PrimitiveType, String)>, body: Vec<Statement>, return_type_annotation: Option<PrimitiveType> },
    ReturnStatement { expression: Box<Expression> }
}

impl Statement {
    pub fn accept(&self, visitor: &mut dyn Visitor) -> String {
        match self {
          Statement::VarDeclarationStatement { type_annotation, name, value } => {
            let type_annotation = self.translate_type_annotation(*type_annotation);
            let name = name.clone();
            let value = match value {
              Some(expr) => format!(" = {}", expr.accept(visitor)),
              None => "".to_string(),
            };
            format!("let {}: {}{};",name, type_annotation, value)
          },
          Statement::VarAssignmentStatement { name, value } => {
            let name = name.clone();
            let value = value.accept(visitor);
            format!("{} = {};", name, value)
          },
          Statement::IfStatement { condition, valid, invalid } => {
            let condition = condition.accept(visitor);
            let valid = valid.iter().map(|s| s.accept(visitor)).collect::<Vec<String>>().join("\n");
            let invalid = match invalid {
              Some(stmt) => format!("else {}", stmt.accept(visitor)),
              None => "".to_string(),
            };
            format!("if ({}) {{\n{}\n}} {}", condition, valid, invalid)
          },
          Statement::WhileStatement { condition, body } => {
            let condition = condition.accept(visitor);
            let body = body.iter().map(|s| s.accept(visitor)).collect::<Vec<String>>().join("\n");
            format!("while ({}) {{\n{}\n}}", condition, body)
          },
          Statement::BlockStatement { statements } => {
            let statements = statements.iter().map(|s| s.accept(visitor)).collect::<Vec<String>>().join("\n");
            format!("{{\n{}\n}}", statements)
          },
          Statement::FunctionDeclarationStatement { name, parameters, body, return_type_annotation } => {
            let name = name.clone();
            let parameters = parameters.iter().map(|(type_annotation, name)| {
              let type_annotation = self.translate_type_annotation(*type_annotation);
              format!("{}: {}", name, type_annotation)
            }).collect::<Vec<String>>().join(", ");
            let body = body.iter().map(|s| s.accept(visitor)).collect::<Vec<String>>().join("\n");
            let return_type_annotation = match return_type_annotation {
              Some(type_annotation) => format!(": {}", self.translate_type_annotation(*type_annotation)),
              None => "".to_string(),
            };
            format!("function {}({}){} {{\n{}\n}}", name, parameters, return_type_annotation, body)
          },
          Statement::ReturnStatement { expression } => {
            let expression = expression.accept(visitor);
            format!("return {};", expression)
          }
        }
      }

      fn translate_type_annotation(&self, type_annotation: PrimitiveType) -> String {
        match type_annotation {
          PrimitiveType::Number => "number".to_string(),
          PrimitiveType::String => "string".to_string(),
          PrimitiveType::Boolean => "boolean".to_string(),
          PrimitiveType::Void => "void".to_string(),
          PrimitiveType::Any => "any".to_string(),
        }
      }
}

impl<'a, I> Parser<'a, I> where I: Iterator<Item = Token> {
    pub fn statement(&mut self) -> Result<Statement, ParserError> {
        match self.peek() {
            type_annotation @ TokenKind::Boolean 
            | type_annotation @ TokenKind::Number 
            | type_annotation  @ TokenKind::String => {
                self.next();
                let name = {
                    let token = self.try_consume(TokenKind::Identifier);
                    self.text(token).to_string()
                };

                if self.at(TokenKind::Operator(OperatorKind::Equals)) {
                    self.try_consume(TokenKind::Operator(OperatorKind::Equals));

                    let value = self.parse_expression()?;
    
                    self.try_consume(TokenKind::Punctuation(PunctuationKind::SemiColon));
                    return Ok(Statement::VarDeclarationStatement { type_annotation: self.map_to_primitive_type(type_annotation)?, name, value: Some(value) })
                } else {
                    self.try_consume(TokenKind::Punctuation(PunctuationKind::SemiColon));
                    return Ok(Statement::VarDeclarationStatement { type_annotation: self.map_to_primitive_type(type_annotation)?, name, value: None })
                }
            },
            TokenKind::Identifier => {
                let name = {
                    let token = self.try_consume(TokenKind::Identifier);
                    self.text(token).to_string()
                };

                self.try_consume(TokenKind::Operator(OperatorKind::Equals));
                let value = self.parse_expression()?;
                self.try_consume(TokenKind::Punctuation(PunctuationKind::SemiColon));

                Ok(Statement::VarAssignmentStatement { name, value })
            },
            TokenKind::If => {
                self.try_consume(TokenKind::If);
                self.try_consume(TokenKind::Punctuation(PunctuationKind::LParenthesize));
                let condition = self.parse_expression()?;
                self.try_consume(TokenKind::Punctuation(PunctuationKind::RParenthesize));
            
                let body = self.statement()?;
                let body = match body {
                    Statement::BlockStatement { statements } => statements,
                    _ => unreachable!(),
                };
            
                let else_stmt = if self.at(TokenKind::Else) {
                    self.try_consume(TokenKind::Else);
                    assert!(
                        self.at(TokenKind::If) || self.at(TokenKind::Punctuation(PunctuationKind::LCurlyBracket)),
                        "Expected a block or an `if` after `else` statement"
                    );
                    Some(Box::new(self.statement()?))
                } else {
                    None
                };
            
                Ok(Statement::IfStatement { condition: Box::new(condition), valid: body, invalid: else_stmt })
            },
            TokenKind::While => {
                self.try_consume(TokenKind::While);
                self.try_consume(TokenKind::Punctuation(PunctuationKind::LParenthesize));
                let condition = self.parse_expression()?;
                self.try_consume(TokenKind::Punctuation(PunctuationKind::RParenthesize));
            
                let body = self.statement()?;
                let body: Vec<Statement> = match body {
                    Statement::BlockStatement { statements } => statements,
                    _ => unreachable!(),
                };
            
                Ok(Statement::WhileStatement { condition: Box::new(condition), body })
            },
            TokenKind::Punctuation(PunctuationKind::LCurlyBracket) => {
                self.try_consume(TokenKind::Punctuation(PunctuationKind::LCurlyBracket));
                let mut statements = Vec::new();
                while !self.at(TokenKind::Punctuation(PunctuationKind::RCurlyBracket)) {
                    let statement = self.statement()?;
                    statements.push(statement);
                }
                self.try_consume(TokenKind::Punctuation(PunctuationKind::RCurlyBracket));
                Ok(Statement::BlockStatement { statements })
            },
            TokenKind::Function => {
                self.try_consume(TokenKind::Function);
                let name = {
                    let token = self.try_consume(TokenKind::Identifier);
                    self.text(token).to_string()
                };
                self.try_consume(TokenKind::Punctuation(PunctuationKind::LParenthesize));
                let parameters = self.parse_params()?;
                self.try_consume(TokenKind::Punctuation(PunctuationKind::RParenthesize));
                self.try_consume(TokenKind::Punctuation(PunctuationKind::Colon));
                let return_type_annotation = self.parse_type_annotation()?;
                assert!(self.at(TokenKind::Punctuation(PunctuationKind::LCurlyBracket)), "Expected a block after function header");
                let body = match self.statement()? {
                    Statement::BlockStatement { statements } => statements,
                    _ => unreachable!(),
                };
                Ok(Statement::FunctionDeclarationStatement { name, parameters, body, return_type_annotation: Some(return_type_annotation) })
            },
            TokenKind::Return => {
                self.try_consume(TokenKind::Return);
                let expression =self.parse_expression()?;
                self.try_consume(TokenKind::Punctuation(PunctuationKind::SemiColon));
                Ok(Statement::ReturnStatement { expression: Box::new(expression) })
            },
            _ => todo!(),
        }
    }

    fn map_to_primitive_type(&self, kind: TokenKind) -> Result<PrimitiveType, ParserError> {
        match kind {
            TokenKind::Boolean => Ok(PrimitiveType::Boolean),
            TokenKind::String => Ok(PrimitiveType::String),
            TokenKind::Number => Ok(PrimitiveType::Number),
            TokenKind::Void => Ok(PrimitiveType::Void),
            _ => Err(ParserError::InvalidTypeAnnotation)
        }
    }

    fn parse_params(&mut self) -> Result<Vec<(PrimitiveType, String)>, ParserError> {
        let mut params = Vec::new();
        while !self.at(TokenKind::Punctuation(PunctuationKind::RParenthesize)) {
            let type_annotation = self.parse_type_annotation()?;
            let name = {
                let token = self.try_consume(TokenKind::Identifier);
                self.text(token).to_string()
            };
            params.push((type_annotation, name));
    
            if self.at(TokenKind::Punctuation(PunctuationKind::Comma)) {
                self.try_consume(TokenKind::Punctuation(PunctuationKind::Comma));
            }
        }
        Ok(params)
    }

    fn parse_type_annotation(&mut self) -> Result<PrimitiveType, ParserError> {
        if let Some(token) = self.next() {
            match token.kind {
                TokenKind::Boolean | TokenKind::Number | TokenKind::String | TokenKind::Void => {
                    return Ok(self.map_to_primitive_type(token.kind)?)
                },
                _ => return Err(ParserError::InvalidTypeAnnotation)
            }
        } else {
            Ok(PrimitiveType::Any)
        }
    }
}

#[cfg(test)]
mod tests {
    use unindent::unindent;
    use crate::lexer::{TokenKind, OperatorKind};
    use crate::ast::{Expression, FunctionCallExpression, InfixOpExpression};
    use crate::statements::PrimitiveType;

    use super::{Parser, Statement};


    fn parse(input: &str) -> Statement {
        let mut parser = Parser::new(input);
        parser.statement().unwrap()
    }

    #[test]
    fn parse_statements() {
        let statement = parse(
            unindent(
            r#"
                {
                    entier a = 0;
                    entier b;
                    entier c = 3;
                    a = 1;
                    si (a > 0 ET c > 2) {
                        b = a * 2;
                    }
                    sinon si (a < 2 OU c == 2) {
                        b = 2;
                    }
                    sinon {
                        a = 3;
                    }
                }
            "#,
            )
            .as_str(),
        );

        let statements = match statement {
            Statement::BlockStatement { statements } => statements,
            _ => unreachable!()
        };

        assert_eq!(statements.len(), 5);

        let declaration_statement = statements[0].clone();

        match declaration_statement {
            Statement::VarDeclarationStatement { 
                type_annotation, 
                name,
                .. 
            } => {
                assert_eq!(type_annotation, PrimitiveType::Number);
                assert_eq!(name, "a");
            },
            _ => unreachable!()
        }

        let assignment_statement = statements[3].clone();

        match assignment_statement {
            Statement::VarAssignmentStatement { name, .. } => {
                assert_eq!(name, "a")
            },
            _ => unreachable!()
        }

        let if_statement = statements[4].clone();

        match if_statement {
            Statement::IfStatement { 
                condition, 
                valid, 
                invalid 
            } => {
                assert!(matches!(
                    *condition,
                    Expression::InfixOpExpression(InfixOpExpression {
                        op: TokenKind::Operator(OperatorKind::LogicalAnd),
                        lhs: _lhs,
                        rhs: _rhs,
                    })
                ));
                assert_eq!(valid.len(), 1);

                let assignment_statement = &valid[0];

                match assignment_statement {
                    Statement::VarAssignmentStatement { name, value } => {
                        assert_eq!(name, "b");

                        match &value.clone() {
                            Expression::InfixOpExpression(InfixOpExpression { op, .. }) => {
                                assert_eq!(*op, TokenKind::Operator(OperatorKind::Asterisk));
                            },
                            _ => unreachable!()
                        }
                    },
                    _ => unreachable!()
                }

                let nested_if = match invalid {
                    Some(statement) => statement,
                    _ => unreachable!()
                };

                match *nested_if {
                    Statement::IfStatement{
                        condition, 
                        valid, 
                        invalid
                    } => {
                        assert!(matches!(
                            *condition,
                            Expression::InfixOpExpression(InfixOpExpression {
                                op: TokenKind::Operator(OperatorKind::LogicalOr),
                                lhs: _lhs,
                                rhs: _rhs,
                            })
                        ));
                        
                        assert_eq!(valid.len(), 1);

                        let assignment_statement = &valid[0];

                        match assignment_statement {
                            Statement::VarAssignmentStatement { name, .. } => {
                                assert_eq!(name, "b");
                            },
                            _ => unreachable!()
                        }

                        let invalid_statement = match invalid {
                            Some(stmt) => *stmt,
                            None => unreachable!(),
                        };

                        let statements = match invalid_statement {
                            Statement::BlockStatement { statements } => statements,
                            _ => unreachable!()
                        };

                        let invalid_assignment_statement = &statements[0];

                        match invalid_assignment_statement {
                            Statement::VarAssignmentStatement { name, .. } => {
                                assert_eq!(name, "a");
                            },
                            _ => unreachable!()
                        } 
                    },
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }
    }

    #[test]
    fn parse_function_declaration() {
        let statement = parse(
            unindent(
                r#"
                fonction applyLowerCase(bool is_lower): string {
                    string str;
                    si (is_lower == true) {
                        str = toLowerCase(str);
                    }
                    retourner str;
                }
                "#
            ).as_str()
        );

        match statement {
            Statement::FunctionDeclarationStatement { 
                name, 
                parameters, 
                body, 
                return_type_annotation 
            } => {
                assert_eq!(name, "applyLowerCase");
                assert_eq!(parameters[0], (PrimitiveType::Boolean, "is_lower".to_string()));
                assert_eq!(body.len(), 3);

                match body[0].clone() {
                    Statement::VarDeclarationStatement { type_annotation, name, value } => {
                        assert_eq!(type_annotation, PrimitiveType::String);
                        assert_eq!(name, "str");
                        assert_eq!(value, None);
                    },
                    _ => unreachable!()
                };

                match body[1].clone() {
                    Statement::IfStatement { condition, valid, invalid } => {
                        assert!(matches!(
                            *condition,
                            Expression::InfixOpExpression(InfixOpExpression {
                                op: TokenKind::Operator(OperatorKind::DoubleEquals),
                                lhs: _lhs,
                                rhs: _rhs,
                            })
                        ));

                        assert_eq!(valid.len(), 1);

                        let assignment_statement = &valid[0];

                        match assignment_statement {
                            Statement::VarAssignmentStatement { name, value } => {
                                assert_eq!(name, "str");

                                match &value {
                                    Expression::FunctionCallExpression(FunctionCallExpression { name, arguments }) => {
                                        assert_eq!(name, "identifier");
                                        assert_eq!(arguments.len(), 1);
                                        
                                        match arguments[0].clone() {
                                            Expression::Ident(name) => assert_eq!(name, "identifier"),
                                            _ => unreachable!()
                                        }
                                    },
                                    _ => unreachable!()
                                }
                            },
                            _ => unreachable!()
                        }
                        assert_eq!(invalid, None);
                    },
                    _ => unreachable!()
                };
                match return_type_annotation {
                    Some(type_annotation) => assert_eq!(type_annotation, PrimitiveType::String),
                    _ => unreachable!()
                }
            },
            _ => unreachable!()
        }
    }
}