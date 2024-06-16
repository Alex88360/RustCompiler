#![feature(iter_intersperse)]

#[macro_use]
mod macros;
mod lexer;
mod ast;
mod parser;
mod expressions;
mod statements;
mod visitor;
use crate::lexer::TokenKind;
use crate::parser::Parser;
use crate::visitor::TypeScriptVisitor;
use crate::statements::Statement;
use unindent::unindent;

fn main() {
    fn parse(input: &str) -> Vec<Statement> {
        let mut parser = Parser::new(input);
        let mut ast = Vec::<Statement>::new();
        
        while !parser.at(TokenKind::EOF) {
            if let Ok(statement) = parser.statement() {
                ast.push(statement);
            }
        }

        ast
    }

    let input = unindent(
        r#"
        fonction addition (entier tmp1, entier tmp2) : entier
        {
            retourner tmp1 + tmp2;
        }

        fonction main(): rien {
            entier a = 0;
            entier b;
            entier c = 3;
            a = 1;
            si (a < 2 ET c == 3) {
                b = a * 2;
            } sinon si (a > 2 OU c == 2) {
                b = 2;
            } sinon {
                a = 3;
            }
            tantque (a < 100) {
                a = 2;
            }
        }
        "#,
    );

    let mut visitor = TypeScriptVisitor;

    let ts_code = parse(&input)
        .iter()
        .map(|statement| statement.accept(&mut visitor))
        .intersperse("\n".to_string())
        .collect::<String>();
    
    println!("{}", ts_code);
}