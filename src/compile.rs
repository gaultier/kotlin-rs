use crate::error::*;
use crate::lex::Lexer;
// use crate::mir::MirTransformer;
use crate::fmt::Formatter;
use crate::parse::Parser;
use crate::resolver::Resolver;
use crate::sexp_emitter::SexpEmitter;
use crate::type_check::TypeChecker;
use std::io;

pub fn compile<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let stmts = parser.parse()?;
    let mut types = parser.types;

    let mut resolver = Resolver::new(&lexer);
    let resolution = resolver.statements(&stmts)?;

    let mut type_checker = TypeChecker::new(&lexer, &resolution, &mut types);
    let types = type_checker.check_types(&stmts)?;

    // let mut mir_transformer = MirTransformer::new(current_id);
    // let stmts = mir_transformer.statements(stmts);

    let emitter = SexpEmitter::new(&lexer, &types);
    emitter.statements(&stmts, w)
}

pub fn fmt<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let stmts = parser.parse()?;
    let mut types = parser.types;

    let mut resolver = Resolver::new(&lexer);
    let resolution = resolver.statements(&stmts)?;

    let mut type_checker = TypeChecker::new(&lexer, &resolution, &mut types);
    let types = type_checker.check_types(&stmts)?;
    let mut formatter = Formatter::new(&lexer, &types);

    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    formatter.statements(&stmts, &mut handle)
}
