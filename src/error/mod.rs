use chumsky::prelude::*;

pub type Span = SimpleSpan<usize>;
pub type Error<'src, T> = extra::Err<Rich<'src, T, Span>>;

pub type Spanned<T> = (T, Span);
