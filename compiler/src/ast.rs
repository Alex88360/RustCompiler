#![allow(dead_code)]

use std::fmt::Display;

use crate::lexer::TokenKind;
use crate::visitor::Visitor;


#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOperator {
    PostfixIncrement,
    PrefixIncrement,
    PostfixDecrement,
    PrefixDecrement
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOperator {
    Equality,
    Inequality,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixOpExpression { 
    pub op: TokenKind, 
    pub expression: Box<Expression> 
}

#[derive(Debug, PartialEq, Clone)]
pub struct PostfixOpExpression { 
    pub op: TokenKind, 
    pub expression: Box<Expression> 
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixOpExpression { 
    pub lhs: Box<Expression>, 
    pub op: TokenKind, 
    pub rhs: Box<Expression> 
}


#[derive(Debug, PartialEq, Clone)]
pub struct FunctionCallExpression { 
    pub name: String, 
    pub arguments: Vec<Expression>
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    String(String),
    Int(usize),
    Float(f64),
    Boolean(bool)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Ident(String),
    PrefixOpExpression(PrefixOpExpression),
    PostfixOpExpression(PostfixOpExpression),
    InfixOpExpression(InfixOpExpression),
    FunctionCallExpression(FunctionCallExpression),
    Literal(Literal)
}

impl Expression {
    pub fn accept(&self, visitor: &mut dyn Visitor) -> String {
        match self {
          Expression::Ident(name) => visitor.visit_identifier(name),
          Expression::Literal(lit) => visitor.visit_literal(lit),
          Expression::PrefixOpExpression(PrefixOpExpression { .. }) => visitor.visit_unary_expression(&self),
          Expression::PostfixOpExpression(PostfixOpExpression { .. }) => visitor.visit_unary_expression(&self),
          Expression::InfixOpExpression(InfixOpExpression { .. }) => visitor.visit_binary_expression(&self),
          Expression::FunctionCallExpression(FunctionCallExpression { .. }) => visitor.visit_function_call_expression(&self),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
           Expression::Ident(name) => write!(f, "{}", name),
           Expression::Literal(lit) => write!(f, "{}", lit),
           Expression::PrefixOpExpression(PrefixOpExpression { op, expression }) => write!(f, "({}{})", op, expression),
           Expression::PostfixOpExpression(PostfixOpExpression{ op, expression }) => write!(f, "({}{})", expression, op),
           Expression::InfixOpExpression(InfixOpExpression { lhs, op, rhs }) => write!(f, "({} {} {})", lhs, op, rhs),
           Expression::FunctionCallExpression(FunctionCallExpression { name, arguments }) => {
            write!(f, "{}(", name)?;

            for arg in arguments {
                write!(f, "{},", arg)?;
            }

            write!(f, ")")
           }
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::String(s) => write!(f, r#""{}""#, s),
            Literal::Boolean(bool) => write!(f, "{}", bool),
        }
    }
}

