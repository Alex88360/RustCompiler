use crate::ast::{Expression, Literal, InfixOpExpression, PrefixOpExpression, PostfixOpExpression, FunctionCallExpression}; 
use crate::lexer::{OperatorKind, TokenKind};
pub trait Visitor {
    fn visit(&mut self, node: &Expression) -> String;
    fn visit_unary_expression(&mut self, node: &Expression) -> String;
    fn visit_binary_expression(&mut self, node: &Expression) -> String;
    fn visit_function_call_expression(&mut self, node: &Expression) -> String;
    fn visit_literal(&mut self, node: &Literal) -> String;
    fn visit_identifier(&mut self, node: &str) -> String;
    fn translate_unary_operator(&self, operator: TokenKind) -> String;
    fn translate_binary_operator(&self, operator: TokenKind) -> String;
  }
  
  pub struct TypeScriptVisitor;
  
  impl Visitor for TypeScriptVisitor {
    fn visit(&mut self, node: &Expression) -> String {
        match node {
            Expression::Ident(name) => self.visit_identifier(name),
            Expression::Literal(lit) => self.visit_literal(lit),
            Expression::PrefixOpExpression(PrefixOpExpression { .. }) => self.visit_unary_expression(node),
            Expression::PostfixOpExpression(PostfixOpExpression { .. }) => self.visit_unary_expression(node),
            Expression::InfixOpExpression(InfixOpExpression { .. }) => self.visit_binary_expression(node),
            Expression::FunctionCallExpression(FunctionCallExpression { .. }) => self.visit_function_call_expression(node),
        }
    }
    fn visit_unary_expression(&mut self, node: &Expression) -> String {
      match node {
        Expression::PrefixOpExpression(PrefixOpExpression { op, expression }) 
        |  Expression::PostfixOpExpression(PostfixOpExpression { op, expression }) => {
            let operator = self.translate_unary_operator(*op);
            let expression = self.visit(expression);
            return format!("{}{}", operator, expression)
        },
        _ => unreachable!()
      }
    }
  
    fn visit_binary_expression(&mut self, node: &Expression) -> String {
        match node {
            Expression::InfixOpExpression(InfixOpExpression { op, lhs, rhs }) => {
                let operator = self.translate_binary_operator(*op);
                let lhs = self.visit(lhs);
                let rhs = self.visit(rhs);
                return format!("({} {} {})", lhs, operator, rhs)
            },
            _ => unreachable!()
        }
    }
  
    fn visit_function_call_expression(&mut self, node: &Expression) -> String {
        match node {
            Expression::FunctionCallExpression(FunctionCallExpression{ name, arguments }) => {
                let arguments = arguments.iter().map(|p| self.visit(p)).collect::<Vec<String>>().join(", ");
                format!("{}({})", name, arguments)
            },
            _ => unreachable!()   
        }
    }
  
    fn visit_literal(&mut self, node: &Literal) -> String {
      match node {
        Literal::String(s) => format!(r#""{}""#, s),
        Literal::Int(i) => format!("{}", i),
        Literal::Float(f) => format!("{}", f),
        Literal::Boolean(b) => format!("{}", b),
      }
    }
  
    fn visit_identifier(&mut self, node: &str) -> String {
      node.to_string()
    }
  
    fn translate_unary_operator(&self, operator: TokenKind) -> String {
      match operator {
        TokenKind::Operator(OperatorKind::DoublePlus) => "++".to_string(),
        TokenKind::Operator(OperatorKind::DoubleMinus) => "--".to_string(),
        _ => unreachable!()
      }
    }
  
    fn translate_binary_operator(&self, operator: TokenKind) -> String {
      match operator {
        TokenKind::Operator(OperatorKind::Equals) => "=".to_string(),
        TokenKind::Operator(OperatorKind::DoubleEquals) => "==".to_string(),
        TokenKind::Operator(OperatorKind::BangEquals) => "!=".to_string(),
        TokenKind::Operator(OperatorKind::LessThan) => "<".to_string(),
        TokenKind::Operator(OperatorKind::LessThanOrEqual) => "<=".to_string(),
        TokenKind::Operator(OperatorKind::GreaterThan) => ">".to_string(),
        TokenKind::Operator(OperatorKind::GreaterThanOrEqual) => ">=".to_string(),
        TokenKind::Operator(OperatorKind::LogicalAnd) => "&&".to_string(),
        TokenKind::Operator(OperatorKind::LogicalOr) => "||".to_string(),
        TokenKind::Operator(OperatorKind::Asterisk) => "*".to_string(),
        TokenKind::Operator(OperatorKind::Plus) => "*".to_string(),
        _ => unreachable!()
      }
    }
  }
  