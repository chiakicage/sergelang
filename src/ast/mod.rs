pub mod ast;

pub use ast::*;

use std::fmt;
use crate::error::{Span, Spanned};

type SpannedExpr<'src> = Spanned<Expr<'src>>;

#[derive(Debug)]
pub enum Ast<'src> {
    Module(Spanned<Module<'src>>),
    Decl(Spanned<Decl<'src>>),
	Expr(Spanned<Expr<'src>>),
	Stmt(Spanned<Stmt<'src>>),
}

#[derive(Debug)]
pub struct AstPrinter<'src> {
	ast_type: Ast<'src>,
	level: usize,
}

impl AstPrinter<'_> {
	pub fn new<'src>(module: Spanned<Module<'src>>) -> AstPrinter<'src> {
		AstPrinter {
			ast_type: Ast::Module(module),
			level: 0
		}
	}
}

impl<'src> fmt::Display for AstPrinter<'src> {
	fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
		write!(f, "{{ a: {{ b : 123}} }}")
	}
}
