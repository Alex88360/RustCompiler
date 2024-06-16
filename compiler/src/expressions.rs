use crate::lexer::{Token, TokenKind, PunctuationKind, OperatorKind, NumericHint};
use crate::ast::{Expression, FunctionCallExpression, Literal, PrefixOpExpression, InfixOpExpression};
use crate::parser::*;

impl<'a, I> Parser<'a, I> where I : Iterator<Item=Token> {
    pub(crate) fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_binary_expression(1)
    }

    // create me an operator precedence function
    fn operator_precedence(&self, op: TokenKind) -> u32 {
        match op {
            TokenKind::Identifier | TokenKind::Number | TokenKind::Boolean | TokenKind::String => 7,
            TokenKind::Operator(OperatorKind::Asterisk) 
            | TokenKind::Operator(OperatorKind::ForwardSlash) 
            | TokenKind::Operator(OperatorKind::Modulo) => 6,
            TokenKind::Operator(OperatorKind::Plus) | TokenKind::Operator(OperatorKind::Minus) => 5,
            TokenKind::Operator(OperatorKind::GreaterThan)
            | TokenKind::Operator(OperatorKind::GreaterThanOrEqual)
            | TokenKind::Operator(OperatorKind::LessThan)
            | TokenKind::Operator(OperatorKind::LessThanOrEqual)
            | TokenKind::Operator(OperatorKind::BangEquals) => 4,
            TokenKind::Operator(OperatorKind::DoubleEquals) => 3,
            TokenKind::Operator(OperatorKind::LogicalAnd) => 2,
            TokenKind::Operator(OperatorKind::LogicalOr) => 1,
            _ => 0
        }
    }
     
    
    fn parse_primary_expression(&mut self) -> Result<Expression, ParserError> {
        match self.peek() {
            TokenKind::Identifier => {
                let name = try_consume_token!(self, TokenKind::Identifier).unwrap().to_string();
                if self.at(TokenKind::Punctuation(PunctuationKind::LParenthesize)) {
                    self.try_consume(TokenKind::Punctuation(PunctuationKind::LParenthesize));
                    let arguments = self.parse_args()?;
                    self.try_consume(TokenKind::Punctuation(PunctuationKind::RParenthesize));
                    Ok(Expression::FunctionCallExpression(FunctionCallExpression { name, arguments }))
                } else {
                    Ok(Expression::Ident(name))
                }
            }
            TokenKind::Punctuation(PunctuationKind::LParenthesize) => {
                self.try_consume(TokenKind::Punctuation(PunctuationKind::LParenthesize));
                let expression = self.parse_expression()?;
                self.try_consume(TokenKind::Punctuation(PunctuationKind::RParenthesize));
                Ok(expression)
            }
            TokenKind::Operator(OperatorKind::Plus)
            | TokenKind::Operator(OperatorKind::Minus)
            | TokenKind::Operator(OperatorKind::Bang) => {
                let op = self.next().unwrap().kind;
                let expression = self.parse_primary_expression()?;
                Ok(Expression::PrefixOpExpression(PrefixOpExpression {
                    op,
                    expression: Box::new(expression),
                }))
            }
            lit @ TokenKind::Str | lit @ TokenKind::Num(_) | lit @ TokenKind::True | lit @ TokenKind::False => {
                self.parse_literal_expression(lit)
            },
            _ => Err(ParserError::InvalidLiteral),
        }
    }
    
    fn parse_binary_expression(
        &mut self,
        min_precedence: u32,
    ) -> Result<Expression, ParserError> {
        let mut lhs = self.parse_primary_expression()?;
        let mut operators = Vec::new();
    
        loop {
            let op = self.peek();
            if op == TokenKind::EOF {
                break
            }

            let precedence = self.operator_precedence(op);
            if precedence < min_precedence {
                break;
            }
    
            self.next();
    
            let rhs = self.parse_binary_expression(precedence + 1)?;
    
            while let Some((prev_op, prev_rhs)) = operators.last().cloned() {
                if precedence <= self.operator_precedence(prev_op) {
                    operators.pop();
                    lhs = Expression::InfixOpExpression(InfixOpExpression {
                        lhs: Box::new(lhs),
                        op: prev_op,
                        rhs: Box::new(prev_rhs),
                    });
                } else {
                    break;
                }
            }
    
            operators.push((op, rhs));
        }
    
        for (op, prev_rhs) in operators.into_iter().rev() {
            lhs = Expression::InfixOpExpression(InfixOpExpression {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(prev_rhs),
            });
        }
    
        Ok(lhs)
    }
    
                
    fn parse_literal_expression(&mut self, kind: TokenKind) -> Result<Expression, ParserError> {
        let token = self.next().unwrap();
        let literal = match kind {
            TokenKind::Str => Literal::String(self.text(token).to_string()),
            TokenKind::Num(hint) => self.parse_number(&self.text(token), hint)?,
            TokenKind::True | TokenKind::False => {
                let value = self.text(token).parse().expect("Invalid boolean literal");
                Literal::Boolean(value)
            }
            _ => return Err(ParserError::InvalidLiteral),
        };
    
        Ok(Expression::Literal(literal))
    }
    
    fn parse_number(&self, raw: &str, hint: NumericHint) -> Result<Literal, ParserError> {
        match hint {
            NumericHint::Integer => raw
                .parse()
                .map(Literal::Int)
                .map_err(|_| ParserError::InvalidLiteral),
            NumericHint::FloatingPoint => raw
                .parse()
                .map(Literal::Float)
                .map_err(|_| ParserError::InvalidLiteral),
            _ => Err(ParserError::InvalidLiteral),
        }
    }
    
    fn parse_args(&mut self) -> Result<Vec<Expression>, ParserError> {
        let mut args = Vec::new();
        while !self.at(TokenKind::Punctuation(PunctuationKind::RParenthesize)) {
            let arg = self.parse_expression()?;
            args.push(arg);
    
            if self.at(TokenKind::Punctuation(PunctuationKind::Comma)) {
                self.try_consume(TokenKind::Punctuation(PunctuationKind::Comma));
            }
        }
        Ok(args)
    }

}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::ast::*;
    use crate::lexer::*;

    fn parse(input: &str) -> Expression {
        let mut parser = Parser::new(input);
        parser.parse_expression().unwrap()
    }

    #[test]
    fn parse_function_call_expression() {
        assert_eq!(parse("call(a, b)"), Expression::FunctionCallExpression(FunctionCallExpression { 
            name: "identifier".to_string(), 
            arguments: vec![
                Expression::Ident("identifier".to_string()),
                Expression::Ident("identifier".to_string())
            ]
        }));
    }

    #[test]
    fn parse_prefix_op_expression() {
        assert_eq!(parse("-13"), Expression::PrefixOpExpression(PrefixOpExpression { 
            op: TokenKind::Operator(OperatorKind::Minus), 
            expression: Box::new(Expression::Literal(Literal::Int(13)))
        }));

        assert_eq!(parse("+13.4"), Expression::PrefixOpExpression(PrefixOpExpression { 
            op: TokenKind::Operator(OperatorKind::Plus), 
            expression: Box::new(Expression::Literal(Literal::Float(13.4)))
        }));

        assert_eq!(parse("!b"), Expression::PrefixOpExpression(PrefixOpExpression { 
            op: TokenKind::Operator(OperatorKind::Bang), 
            expression: Box::new(Expression::Ident("identifier".to_string()))
        }));
    }

    #[test]
    fn parse_binary_expressions() {
        let expr = parse("5 + 2");
        assert_eq!(expr.to_string(), "(5 + 2)");
        
        let expr = parse("3 * (7 + 2)");
        assert_eq!(expr.to_string(), "(3 * (7 + 2))");
        
        let expr = parse("(6 - 2) - 1");
        assert_eq!(expr.to_string(), "((6 - 2) - 1)");
        
        let expr = parse("2 * (9 - 3) + 5");
        assert_eq!(expr.to_string(), "((2 * (9 - 3)) + 5)");
        
        let expr = parse("8 - (4 * 2 + 1)");
        assert_eq!(expr.to_string(), "(8 - ((4 * 2) + 1))");
        
        let expr = parse("1 + (5 - 2) * 6 + 3");
        assert_eq!(expr.to_string(), "((1 + ((5 - 2) * 6)) + 3)");

        let expr = parse("2 + 3 - 4 * 5 / 2");
        assert_eq!(expr.to_string(), "((2 + 3) - ((4 * 5) / 2))");

        let expr = parse("10 == 5 + 5");
        assert_eq!(expr.to_string(), "(10 == (5 + 5))");

        let expr = parse("true ET false OU true");
        assert_eq!(expr.to_string(), "((true ET false) OU true)");

        let expr = parse("a < 2 ET b > 4");
        assert_eq!(expr.to_string(), "((identifier < 2) ET (identifier > 4))");

        let expr = parse("!false ET (3 > 2)");
        assert_eq!(expr.to_string(), "((!false) ET (3 > 2))");

        let expr = parse("7 % 3 + 2 * 4");
        assert_eq!(expr.to_string(), "((7 % 3) + (2 * 4))");

        let expr = parse("-(2 + 3) * 4");
        assert_eq!(expr.to_string(), "((-(2 + 3)) * 4)");

        let expr = parse("call(a, b) + 4");
        assert_eq!(expr.to_string(), "(identifier(identifier,identifier,) + 4)");

        let expr = parse("2.0 / ((3.0 + 4.0) * (5.0 - 6.0)) * 7.0");
        assert_eq!(expr.to_string(), "((2 / ((3 + 4) * (5 - 6))) * 7)");
        
    }
}