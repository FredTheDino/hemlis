#![allow(unused)]

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

kw!(kw_begin, T::LayBegin);
kw!(kw_end, T::LayEnd);
kw!(kw_sep, T::LaySep);
kw!(kw_top, T::LayTop);

fn symbol<'t>(p: &mut P<'t>) -> Option<Symbol<'t>> {
    kw_lp(p);
    let sym = op(p)?.0;
    kw_rp(p);
    Some(Symbol::<'t>(sym))
}

macro_rules! q {
    ($name:ident, $inner:expr, $token:expr, $thing:ident) => {
        fn $name<'t>(p: &mut P<'t>) -> Option<$thing<'t>> {
            let start = p.span();
            let xs = quals(p);
            let end = p.span();
            let x = $inner(p)?;
            Some($token(S(xs, start.merge(end)), x))
        }
    };
}

fn quals<'t>(p: &mut P<'t>) -> Vec<Qual<'t>> {
    let mut out = Vec::new();
    loop {
        match p.peek().0 {
            Some(T::Qual(_)) => out.push(qual(p).unwrap()),
            _ => break,
        }
    }
    out
}

q!(qname, name, QName, QName);
q!(qproper, proper, QProperName, QProperName);
q!(qsymbol, symbol, QSymbol, QSymbol);
q!(qop, op, QOp, QOp);

fn header<'t>(p: &mut P<'t>) -> Option<Header<'t>> {
    while matches!(p.peek().0, Some(T::LayTop)) {
        p.skip();
    }
    kw_module(p)?;
    let name = qproper(p)?;
    let exports = exports(p);
    // TODO: recover on `where`
    p.recover(|x| matches!(x, T::Lower("where") | T::LayTop))
        .ok()?;
    kw_where(p)?;
    let imports = imports(p);
    Some(Header(name, exports, imports))
}

macro_rules! choice {
    ($p:ident, $($t:expr),*) => {
        {
            let (p__, x) = match () {
                // TODO: If this was a proc-macro the code would be twice as fast
                // TODO: Rust doesn't have if-let guards on matches
            $(
                _ if matches!({
                    let mut p_ = $p.fork();
                    let out = $t(&mut p_);
                    (p_, out)
                }, (_, Some(_))) =>  {
                    let mut p_ = $p.fork();
                    let out = $t(&mut p_);
                    (p_, out)
                }
            ),*
                _ => {
                    let mut p_ = $p.fork();
                    p_.raise_::<usize>(stringify!($($t),*));
                    (p_, None)
                }
            };
            *$p = p__;
            x
        }
    };
}

fn sep<'t, FS, FE, E, S>(p: &mut P<'t>, s: FS, e: FE) -> Vec<E>
where
    FS: Fn(&mut P<'t>) -> Option<S>,
    FE: Fn(&mut P<'t>) -> Option<E>,
{
    let mut out = Vec::new();
    loop {
        if let Some(ee) = e(p) {
            out.push(ee);
        } else {
            break;
        }
        if choice!(p, s, |_| None::<S>).is_some() {
            continue;
        } else {
            break;
        }
    }
    out
}

fn exports<'t>(p: &mut P<'t>) -> Vec<Export<'t>> {
    fn f<'t>(p: &mut P<'t>) -> Option<Vec<Export<'t>>> {
        kw_lp(p)?;
        let exports = sep(p, kw_comma, export);
        kw_rp(p)?;
        Some(exports)
    }
    choice!(p, f, |_| { Some(Vec::<Export<'t>>::new()) }).unwrap()
}

fn export<'t>(p: &mut P<'t>) -> Option<Export<'t>> {
    choice!(
        p,
        |p: &mut _| {
            kw_type(p)?;
            Some(Export::TypSymbol(symbol(p)?))
        },
        |p: &mut _| {
            kw_class(p)?;
            Some(Export::Class(proper(p)?))
        },
        |p: &mut _| {
            kw_module(p)?;
            Some(Export::Module(qproper(p)?))
        },
        |p: &mut _| { Some(Export::Value(name(p)?)) },
        |p: &mut _| { Some(Export::Symbol(symbol(p)?)) },
        |p: &mut _| {
            let n = proper(p)?;
            let ms = data_members(p)?;
            Some(Export::TypDat(n, ms))
        },
        |p: &mut _| { Some(Export::Typ(proper(p)?)) }
    )
}

fn data_members<'t>(p: &mut P<'t>) -> Option<DataMember<'t>> {
    kw_lp(p)?;
    let out = match p.peek().0 {
        Some(T::Symbol("..")) => {
            p.skip();
            DataMember::All
        }
        Some(T::LeftParen) => DataMember::Some(Vec::new()),
        Some(_) => DataMember::Some(sep(p, kw_comma, proper)),
        _ => p.raise_("EoF")?,
    };
    kw_rp(p)?;
    Some(out)
}

fn skip_to_top<'t>(p: &mut P<'t>) -> Option<()> {
    p.recover(|t| matches!(t, T::LayTop)).ok()
}

fn imports<'t>(p: &mut P<'t>) -> Vec<ImportDecl<'t>> {
    let mut out = Vec::new();
    while matches!(p.peek2t(), (Some(T::LayTop), Some(T::Lower("import")))) {
        p.skip();
        if let Some(import) = import_decl(p) {
            out.push(import);
        }
        if skip_to_top(p).is_none() {
            break;
        }
    }
    out
}

fn import_decl<'t>(p: &mut P<'t>) -> Option<ImportDecl<'t>> {
    kw_import(p)?;
    let name = qproper(p)?;
    Some(match p.peekt() {
        Some(T::Lower("as")) => {
            kw_as(p)?;
            let as_name = qproper(p)?;
            ImportDecl::As(name, as_name)
        }
        Some(T::Lower("hiding")) => {
            kw_hiding(p)?;
            kw_lp(p)?;
            let imports = sep(p, kw_comma, import);
            kw_rp(p)?;
            ImportDecl::Hiding(name, imports)
        }
        Some(T::LeftParen) => {
            kw_lp(p)?;
            let imports = sep(p, kw_comma, import);
            kw_rp(p)?;
            ImportDecl::Multiple(name, imports)
        }
        Some(_) => ImportDecl::Bulk(name),
        _ => p.raise_("EoF")?,
    })
}

fn import<'t>(p: &mut P<'t>) -> Option<Import<'t>> {
    choice!(
        p,
        |p: &mut _| {
            kw_type(p)?;
            Some(Import::TypSymbol(symbol(p)?))
        },
        |p: &mut _| {
            kw_class(p)?;
            Some(Import::Class(proper(p)?))
        },
        |p: &mut _| { Some(Import::Value(name(p)?)) },
        |p: &mut _| { Some(Import::Symbol(symbol(p)?)) },
        |p: &mut _| {
            let n = proper(p)?;
            let ms = data_members(p)?;
            Some(Import::TypDat(n, ms))
        },
        |p: &mut _| { Some(Import::Typ(proper(p)?)) }
    )
}

#[derive(Clone, Debug)]
enum Serror<'s> {
    Unexpected(Span, Option<Token<'s>>, &'static str),
}

#[derive(Clone, Debug)]
struct P<'s> {
    fi: usize,
    i: usize,
    tokens: &'s Vec<(Result<Token<'s>, ()>, std::ops::Range<usize>)>,
    errors: Vec<Serror<'s>>,
    panic: bool,
}

impl<'s> P<'s> {
    fn new(fi: usize, tokens: &'s Vec<(Result<Token<'s>, ()>, std::ops::Range<usize>)>) -> Self {
        Self {
            fi,
            i: 0,
            panic: false,
            errors: Vec::new(),
            tokens,
        }
    }

    fn peek_(&self) -> (Option<Token<'s>>, Span) {
        (self.tokens.get(self.i).and_then(|x| x.0.ok()), self.span())
    }

    fn peek(&self) -> (Option<Token<'s>>, Span) {
        if self.panic {
            (None, self.span())
        } else {
            self.peek_()
        }
    }

    fn peek2t(&self) -> (Option<Token<'s>>, Option<Token<'s>>) {
        if self.panic {
            (None, None)
        } else {
            (
                self.tokens.get(self.i).and_then(|x| x.0.ok()),
                self.tokens.get(self.i + 1).and_then(|x| x.0.ok()),
            )
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
        let s = self.tokens.get(self.i).or_else(|| self.tokens.last());
        Span {
            lo: s.map(|x| x.1.start).unwrap_or(0),
            hi: s.map(|x| x.1.end).unwrap_or(0),
            fi: self.fi,
        }
    }

    fn fork(&self) -> Self {
        self.clone()
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

    fn iff<F>(&mut self, f: F, err: &'static str) -> Option<()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if let (Some(x), _) = self.peek() {
            if f(x) {
                self.next();
                return Some(());
            }
        }
        self.raise(Serror::Unexpected(self.span(), self.peekt(), err));
        None
    }

    fn expect<F>(&mut self, f: F, err: &'static str) -> Option<()>
    where
        F: Fn(Token<'s>) -> bool,
    {
        if let (Some(x), _) = self.peek() {
            if f(x) {
                self.next();
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
                self.next();
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

#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    macro_rules! gen_parser {
        ($a:ident, $p:ident) => {
            pub fn $a<'t>(src: &'t str) -> String {
                use super::*;
                use crate::lexer;
                use std::io::BufWriter;

                let l = lexer::lex(&src);
                let mut p = P::new(0, &l);
                if let Some(out) = $p(&mut p) {
                    let mut buf = BufWriter::new(Vec::new());
                    out.show(0, &mut buf).unwrap();
                    let inner = buf.into_inner().map_err(|x| format!("{:?}", x)).unwrap();
                    String::from_utf8(inner)
                        .map_err(|x| format!("{:?}", x))
                        .unwrap()
                } else {
                    p.errors
                        .iter()
                        .map(|x| format!("{:?}", x))
                        .collect::<Vec<_>>()
                        .join("\n")
                }
            }
        };
    }

    gen_parser!(p_header, header);

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

import A
import A as A
import A.B.C as A.C
import A.B.C (a, class B, 
 C, D(..), E(F, G), (+), type (+))
import A.B.C hiding (foo)
        "#
        ));
    }

    gen_parser!(p_import, import_decl);

    #[test]
    fn simple_joe_import() {
        assert_snapshot!(p_import("import Joe"))
    }

    #[test]
    fn simple_import() {
        assert_snapshot!(p_import("import A as A"))
    }

    #[test]
    fn import_with_newlines() {
        assert_snapshot!(p_import("import A (foo\n , bar\n , baz)"))
    }
}

/*
#[cfg(test)]
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
*/
