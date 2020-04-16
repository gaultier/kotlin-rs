use crate::error::*;
use crate::fmt::Formatter;
use crate::jvm_emitter::JvmEmitter;
use crate::lex::Lexer;
use crate::mir::MirTransformer;
use crate::parse::Parser;
use crate::resolver::Resolver;
use crate::session::Session;
use crate::sexp_emitter::SexpEmitter;
use crate::type_check::TypeChecker;
use heck::CamelCase;
use log::debug;
use std::io;
use std::path::{Path, PathBuf};

pub fn compile(src: &str, file_name: &Path) -> Result<(), Error> {
    let session = Session::new(&src, None);
    let mut lexer = Lexer::new(&session);
    let (tokens, session) = lexer.lex()?;
    let mut parser = Parser::new(&session, &tokens);
    let stmts = parser.parse()?;
    let mut types = parser.types;

    let mut mir_transformer = MirTransformer::new(parser.current_id);
    let stmts = mir_transformer.statements(stmts);

    let mut resolver = Resolver::new(&session);
    let resolution = resolver.resolve(&stmts)?;

    let mut type_checker = TypeChecker::new(&session, &resolution, &mut types);
    let types = type_checker.check_types(&stmts)?;

    let class_name = file_name
        .to_path_buf()
        .file_stem()
        .unwrap()
        .to_string_lossy()
        .to_camel_case();
    let fully_qualified_class_name_parent = file_name
        .parent()
        .unwrap_or(PathBuf::from("/").as_path())
        .to_string_lossy()
        .replace("/", ".")
        .to_lowercase();

    let mut class_file_name = PathBuf::from(file_name);
    class_file_name.set_file_name(&class_name);
    class_file_name.set_extension("class");
    let mut class_file = std::fs::File::create(class_file_name)?;

    debug!(
        "file_name={} class_name={} fully_qualified_class_name={}",
        file_name.display(),
        class_name,
        fully_qualified_class_name_parent
    );
    let mut emitter = JvmEmitter::new(&session, &types, &resolution, &file_name, &class_name);
    emitter.main(&stmts, &mut class_file)
}

pub fn fmt<W: io::Write>(src: &str, w: &mut W) -> Result<(), Error> {
    let session = Session::new(&src, None);
    let mut lexer = Lexer::new(&session);
    let (tokens, session) = lexer.lex()?;
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

pub fn sexp<W: io::Write>(src: &str, w: &mut W) -> Result<(), Error> {
    let session = Session::new(&src, None);
    let mut lexer = Lexer::new(&session);
    let (tokens, session) = lexer.lex()?;
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
