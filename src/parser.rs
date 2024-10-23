use purring_macros::prs;

use crate::ast::*;
use crate::lexer;
use crate::lexer::Token as T;
use crate::lexer::Token;

macro_rules! b {
    ($e:expr) => {
        Box::new($e)
    };
}

macro_rules! t {
    ($name:ident, $token:ident, $thing:ident) => {
        fn $name<'t>(p: &mut P<'t>) -> Option<$thing<'t>> {
            match p.next() {
                (Some(T::$token(x)), s) => Some($thing(S(x, s))),
                _ => p.raise_(stringify!($thing)),
            }
        }
    };
}

t!(name, Lower, Name);
t!(proper, Upper, ProperName);
t!(qual, Qual, Qual);
t!(number, Number, Number);
t!(char, Char, Char);
t!(op, Symbol, Op);
t!(hole, Hole, Hole);

fn boolean<'t>(p: &mut P<'t>) -> Option<Boolean<'t>> {
    match p.next() {
        (Some(T::Lower(x @ ("true" | "false"))), s) => Some(Boolean(S(x, s))),
        _ => p.raise_("Boolean"),
    }
}

fn string<'t>(p: &mut P<'t>) -> Option<Str<'t>> {
    match p.next() {
        (Some(T::String(x) | T::RawString(x)), s) => Some(Str(S(x, s))),
        _ => p.raise_("String | RawString"),
    }
}

macro_rules! kw {
    ($name:ident, $kw:pat) => {
        fn $name<'t>(p: &mut P<'t>) -> Option<()> {
            p.expect(|x| matches!(x, $kw), stringify!($kw))
        }
    };
}

kw!(kw_lp, Token::LeftParen);
kw!(kw_rp, Token::RightParen);
kw!(kw_lb, Token::LeftBrace);
kw!(kw_rb, Token::RightBrace);
kw!(kw_ls, Token::LeftSquare);
kw!(kw_rs, Token::RightSquare);
kw!(kw_left_arrow, Token::LeftArrow);
kw!(kw_right_arrow, Token::RightArrow);
kw!(kw_right_imply, Token::RightFatArrow);
kw!(kw_coloncolon, Token::DoubleColon);
kw!(kw_tick, Token::Tick);
kw!(kw_comma, Token::Comma);

kw!(kw_left_imply, T::Symbol("<="));
kw!(kw_at, T::Symbol("@"));
kw!(kw_pipe, T::Symbol("|"));
kw!(kw_dot, T::Symbol("."));
kw!(kw_eq, T::Symbol("="));
kw!(kw_dotdot, T::Symbol(".."));
kw!(kw_underscore, T::Symbol("_"));

kw!(kw_module, T::Lower("module"));
kw!(kw_where, T::Lower("where"));
kw!(kw_class, T::Lower("class"));
kw!(kw_as, T::Lower("as"));
kw!(kw_import, T::Lower("import"));
kw!(kw_hiding, T::Lower("hiding"));
kw!(kw_type, T::Lower("type"));
kw!(kw_newtype, T::Lower("newtype"));
kw!(kw_data, T::Lower("data"));
kw!(kw_true, T::Lower("true"));
kw!(kw_false, T::Lower("false"));

// prs!(header -> Header<'t> | .kw_module qproper exports imports => Header);

enum Serror<'s> {
    Unexpected(Span, Option<Token<'s>>, &'static str),
}

struct P<'s> {
    fi: usize,
    i: usize,
    tokens: Vec<(Result<Token<'s>, ()>, std::ops::Range<usize>)>,
    errors: Vec<Serror<'s>>,
    panic: bool,
}

impl<'s> P<'s> {
    fn new(fi: usize, source: &'s str) -> Self {
        Self {
            fi,
            i: 0,
            panic: false,
            errors: Vec::new(),
            tokens: lexer::lex(source),
        }
    }

    fn peek_(&self) -> (Option<Token<'s>>, Span) {
        (
            self.tokens.get(self.i + 1).and_then(|x| x.0.ok()),
            self.span(),
        )
    }

    fn peek(&self) -> (Option<Token<'s>>, Span) {
        if self.panic {
            (None, self.span())
        } else {
            self.peek_()
        }
    }

    fn peekt(&self) -> Option<Token<'s>> {
        if self.panic {
            None
        } else {
            self.peek_().0
        }
    }

    fn eof(&self) -> bool {
        self.i >= self.tokens.len()
    }

    fn next(&mut self) -> (Option<Token<'s>>, Span) {
        if self.panic {
            return (None, self.span());
        }
        let out = self.peek_();
        self.skip();
        out
    }

    fn span(&self) -> Span {
        Span {
            lo: self.tokens[self.i].1.start,
            hi: self.tokens[self.i].1.end,
            fi: self.fi,
        }
    }

    fn skip(&mut self) {
        self.i += 1;
        // NOTE: For now we skipp some of the tokens in this parser
        while matches!(
            self.peek_().0,
            Some(Token::LineComment(_) | Token::BlockComment(_))
        ) {
            self.i += 1;
        }
    }

    fn recover<F>(&mut self, f: F) -> Result<(), ()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if !self.panic {
            return Ok(());
        }
        while let (Some(x), _) = self.peek_() {
            if f(x) {
                self.panic = false;
                return Ok(());
            } else {
                self.i += 1
            }
        }
        Err(())
    }

    fn expect<F>(&mut self, f: F, err: &'static str) -> Option<()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if let (Some(x), _) = self.peek() {
            if f(x) {
                return Some(());
            }
        }
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn expect_<F, A>(&mut self, f: F, err: &'static str) -> Option<A>
    where
        F: Fn(Token<'s>) -> Option<A>,
    {
        if let (Some(x), _) = self.peek() {
            if let Some(a) = f(x) {
                return Some(a);
            }
        }
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn raise_<A>(&mut self, err: &'static str) -> Option<A> {
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn raise(&mut self, err: Serror<'s>) {
        self.errors.push(err)
    }

    fn panic(&mut self) {
        self.panic = true;
    }

    fn raise_and_panic(&mut self, err: Serror<'s>) {
        self.raise(err);
        self.panic();
    }
}

#[cfg(blargh)]
mod tests {
    use insta::assert_snapshot;

    macro_rules! gen_parser {
        ($a:ident, $p:ident) => {
            pub fn $a<'t>(src: &'t str) -> String {
                pub fn inner<'t>(src: &'t str) -> Result<String, String> {
                    use crate::{ast::Ast, grammer, lexer};
                    use std::io::BufWriter;

                    let parser = grammer::$p::new();
                    let out = parser
                        .parse(
                            lexer::lex(&src)
                                .into_iter()
                                .map(|(token, span)| Ok((span.start, token?, span.end))),
                        )
                        .map_err(|x| format!("{:?}", x))?;

                    let mut buf = BufWriter::new(Vec::new());
                    out.show(0, &mut buf).map_err(|x| format!("{:?}", x))?;
                    let inner = buf.into_inner().map_err(|x| format!("{:?}", x))?;
                    String::from_utf8(inner).map_err(|x| format!("{:?}", x))
                }
                match inner(src) {
                    Ok(s) | Err(s) => s,
                }
            }
        };
    }

    gen_parser!(p_header, HeaderParser);

    #[test]
    fn empty_string() {
        assert_snapshot!(p_header(""));
    }

    #[test]
    fn normal_definition() {
        assert_snapshot!(p_header("module A (a, b, c) where"));
    }

    #[test]
    fn everything_header() {
        assert_snapshot!(p_header(
            r#"
module A.B.C (a, class B, C, D(..), E(F, G), 
 (+), type (+), module H) where

import A as A
import A.B.C as A.C
import A.B.C (a, class B, 
 C, D(..), E(F, G), (+), type (+))
import A.B.C hiding (foo)
        "#
        ));
    }

    gen_parser!(p_import, ImportParser);

    #[test]
    fn simple_import() {
        assert_snapshot!(p_import("import A as A"))
    }

    #[test]
    fn import_with_newlines() {
        assert_snapshot!(p_import("import A (foo\n , bar\n , baz)"))
    }

    gen_parser!(p_worstacase, WorstCaseParser);

    #[test]
    fn worstcase_expr() {
        assert_snapshot!(p_worstacase("1 <- 1"))
    }

    #[test]
    fn worstcase_expr2() {
        assert_snapshot!(p_worstacase("1"))
    }
}
