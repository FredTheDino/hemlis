use crate::ast::*;
use crate::lexer;
use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span {
    lo: usize,
    hi: usize,
    fi: usize,
}

impl Span {
    fn merge(self, other: Self) -> Self {
        assert_eq!(other.fi, self.fi, "Cannot merge spans files!");
        Self {
            lo: self.lo.min(other.lo),
            hi: self.hi.min(other.hi),
            fi: self.fi,
        }
    }
}

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
            Some(
                    Token::LineComment(_)
                    | Token::BlockComment(_)
            )
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
        };
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
        assert_snapshot!(p_header(r#"
module A.B.C (a, class B, C, D(..), E(F, G), 
 (+), type (+), module H) where

import A as A
import A.B.C as A.C
import A.B.C (a, class B, 
 C, D(..), E(F, G), (+), type (+))
import A.B.C hiding (foo)
        "#));
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
