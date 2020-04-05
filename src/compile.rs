use crate::error::*;
use crate::fmt::Formatter;
use crate::lex::Lexer;
use crate::parse::Parser;
use crate::resolver::Resolver;
use crate::session::Session;
use crate::sexp_emitter::SexpEmitter;
use crate::type_check::TypeChecker;
use std::io;

pub fn compile<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let mut session = Session::new(&src, None);
    let mut lexer = Lexer::new(&mut session);
    let tokens = lexer.lex()?;
    let mut parser = Parser::new(&session, &tokens);
    let stmts = parser.parse()?;
    let mut types = parser.types;

    let mut resolver = Resolver::new(&session);
    let resolution = resolver.resolve(&stmts)?;

    let mut type_checker = TypeChecker::new(&session, &resolution, &mut types);
    let types = type_checker.check_types(&stmts)?;

    let emitter = SexpEmitter::new(&session, &types);
    emitter.statements(&stmts, w)
}

pub fn fmt<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let mut session = Session::new(&src, None);
    let mut lexer = Lexer::new(&mut session);
    let tokens = lexer.lex()?;
    let mut parser = Parser::new(&session, &tokens);
    let stmts = parser.parse()?;
    let mut types = parser.types;

    let mut resolver = Resolver::new(&session);
    let resolution = resolver.resolve(&stmts)?;

    let mut type_checker = TypeChecker::new(&session, &resolution, &mut types);
    let types = type_checker.check_types(&stmts)?;
    let mut formatter = Formatter::new(&session, &types);

    formatter.statements(&stmts, w)
}
