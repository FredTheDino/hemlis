use crate::ast::*;
use crate::lexer;
use crate::lexer::Token;

/*
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

fn parse_module<'s>(fi: usize, source: &'s str) -> () {
    let mut p = P::new(fi, source);
    header(&mut p);
    ()
}

fn header<'s>(p: &mut P<'s>) -> Option<Header<'s>> {
    p.expect(
        |t| matches!(t, Token::Lower((0, "module"))),
        "Modules start with 'module'",
    )?;
    let name = qproper_name(p)?;

    let exports = if matches!(p.peek(), Token::LeftParen) {
        p.next();
        let mut exports = Vec::new();
        while match p.peek() {
            
            
        }
        {
            p.recover()
        };
        
    } else {
    }

    let imports = todo!();

    // Header(name, exports, imports)
}

fn qproper_name<'s>(p: &mut P<'s>) -> Option<QProperName<'s>> {
    let mut qual = Vec::new();
    loop {
        match p.next() {
            (Some(Token::Qual((_, name))), _) => qual.push(Qual(name)),
            (Some(Token::Upper((_, name))), _) => return Some(QProperName(qual, ProperName(name))),
            (t, at) => {
                p.raise(Serror::Unexpected(at, t, "Not a qualified propper name"));
                return None;
            }
        }
    }
}

fn name<'s>(p: &mut P<'s>) -> Option<Name<'s>> {
    todo!();
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
            tokens: lexer::lex(source).spanned().collect::<Vec<_>>(),
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
                Token::Whitespace(_)
                    | Token::Newline
                    | Token::LineComment(_)
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
            if Some(a) = f(x) {
                return Some();
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
*/
