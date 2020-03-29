use crate::error::*;
use crate::lex::Lexer;
use crate::mir::MirTransformer;
use crate::parse::Parser;
use crate::resolver::Resolver;
use crate::sexp_emitter::SexpEmitter;
use crate::type_check::TypeChecker;
use std::collections::BTreeMap;
use std::io;

pub fn compile<W: io::Write>(src: String, w: &mut W) -> Result<(), Error> {
    let mut lexer = Lexer::new(src);
    let mut parser = Parser::new(&mut lexer);
    let stmts = parser.parse()?;
    let current_id = parser.current_id;

    let mut resolver = Resolver::new(&lexer);
    let resolution = resolver.statements(&stmts)?;

    let mut types = BTreeMap::new();
    let mut type_checker = TypeChecker::new(&lexer, &resolution, &mut types);
    let types = type_checker.check_types(&stmts)?;

    let mut mir_transformer = MirTransformer::new(current_id);
    let stmts = mir_transformer.statements(stmts);

    let emitter = SexpEmitter::new(&lexer, &types);
    emitter.statements(&stmts, w)
}
