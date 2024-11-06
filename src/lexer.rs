use std::ops::Range;

use logos::{FilterResult, Logos, Source};
// NOTE: Might not support unicode?

fn lex_string<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    while let Some(at) = lex.remainder().find("\"") {
        if at == 0 {
            lex.bump(at + 1);
            update_newline(lex);
            return Some(lex.slice());
        }
        match lex.remainder().get(at - 1..at + 1) {
            Some("\\\"") => {
                lex.bump(at + 1);
            }
            _ => {
                lex.bump(at + 1);
                update_newline(lex);
                return Some(lex.slice());
            }
        }
    }
    None
}

fn lex_raw_string<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    while let Some(at) = lex.remainder().find("\"\"\"") {
        match lex.remainder().get(at + 3..at + 4) {
            Some("\"") => {
                lex.bump(at + 1);
            }
            _ => {
                lex.bump(at + 3);
                update_newline(lex);
                return Some(lex.slice());
            }
        }
    }
    None
}

fn lex_block_comment<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    if let Some(at) = lex.remainder().find("-}") {
        lex.bump(at + 2);
        update_newline(lex);
        return Some(lex.slice());
    }
    None
}

fn lex_line_comment<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    if let Some(at) = lex.remainder().find("\n") {
        lex.bump(at + 1);
        update_newline(lex);
        return Some(lex.slice());
    }
    None
}

fn lex_lay<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> FilterResult<Lay, ()> {
    use self::Lay::*;
    use FilterResult::*;

    let at = lex.slice().rfind("\n").unwrap_or(0);
    let indent = lex.span().end - lex.span().start - at - 1;

    update_newline(lex);
    if indent == 0 && lex.remainder().starts_with(|c: char| c.is_lowercase()) {
        lex.extras.1 = Vec::new();
        return Emit(Top);
    }
    let implies_start = lex
        .source()
        .slice(0..lex.span().start)
        .map(|s| {
            s.ends_with(".do")
                || s.ends_with(" do")
                || s.ends_with(" ado")
                || s.ends_with(".ado")
                || s.ends_with(" where")
                || s.ends_with(" let")
                || s.ends_with(" of")
        })
        .unwrap_or(false);
    if indent > lex.extras.1.last().copied().unwrap_or(0) && implies_start {
        update_newline(lex);
        lex.extras.1.push(indent);
        return Emit(Begin(lex.extras.1.len()));
    }
    if indent < lex.extras.1.last().copied().unwrap_or(0) {
        update_newline(lex);
        while indent < lex.extras.1.last().copied().unwrap_or(0) {
            lex.extras.1.pop();
        }
        return Emit(End(lex.extras.1.len()));
    }
    if Some(&indent) == lex.extras.1.last() {
        update_newline(lex);
        return Emit(Sep(lex.extras.1.len()));
    }
    Skip
}

fn update_newline<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) {
    lex.extras.0 = lex.span().start + lex.slice().rfind("\n").unwrap_or(0) + 1;
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Lay {
    Top,
    Sep(usize),
    Begin(usize),
    End(usize),
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
#[logos(extras = (usize, Vec<usize>))]
pub enum Token<'t> {
    #[regex("\\s+", logos::skip)]
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftSquare,
    #[token("]")]
    RightSquare,
    #[token("<-")]
    LeftArrow,
    #[token("->", priority = 10000)]
    RightArrow,
    #[token("=>")]
    RightFatArrow,
    #[token("`")]
    Tick,
    #[token(",")]
    Comma,

    #[token("class")]
    Class,
    #[token("data")]
    Data,
    #[token("infixr")]
    Infixr,
    #[token("infixl")]
    Infixl,
    #[token("infix")]
    Infix,
    #[token("derive")]
    Derive,
    #[token("type")]
    Type,
    #[token("newtype")]
    Newtype,
    #[token("foreign")]
    Foreign,
    #[token("instance")]
    Instance,


    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("case")]
    Case,
    #[token("of")]
    Of,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("where")]
    Where,
    #[token("do")]
    Do,
    #[token("ado")]
    Ado,
    #[token("as")]
    As,

    // TODO: We need to parse this with a custom function, We can eat greadily if we tokenize
    // ourselves here.
    #[regex("[A-Z][[:alnum:]]*\\.")]
    Qual(&'t str),

    // TODO: Might be too much of a wuzz when only extending the uncide with swedish
    #[regex("[_a-zåäö][[:alnum:]'åäöÅÄÖ]*")]
    Lower(&'t str),

    #[regex("[A-ZÅÄÖ][[:alnum:]'åäöÅÄÖ]*", priority = 20)]
    Upper(&'t str),

    #[regex(r"[!|#|$|%|&|*|+|.|/|<|=|>|?|@|\\|^||\\|\-|~|:|¤]+")]
    Symbol(&'t str),

    #[regex("\\?[_a-z][[:alnum:]]*")]
    Hole(&'t str),

    #[regex("0x[[:xdigit:]]+")]
    HexInt(&'t str),

    // contains e or . => Number, otherwise => Int
    #[regex(r"([\d][\d|_]*|([\d]+\.[\d]*|[\d]*\.[\d]+))|[\d]+e(-|\+)?[\d]+")]
    Number(&'t str),

    #[regex(r#"'.'|'\\x.{1,8}'|'\\[trn"\\]'"#)]
    Char(&'t str),

    #[regex("\"", |lex| lex_string(lex))]
    String(&'t str),

    #[regex("\"\"\"", |lex| lex_raw_string(lex))]
    RawString(&'t str),

    #[token("--", |lex| lex_line_comment(lex))]
    LineComment(&'t str),

    #[token("{-", |lex| lex_block_comment(lex))]
    BlockComment(&'t str),

    #[regex("\\s*\n\\s*", lex_lay, priority = 12)]
    Lay(Lay),

    LayBegin,
    LayEnd,
    LaySep,
    LayTop,
}

#[allow(dead_code)]
pub fn contains_lex_errors(content: &str) -> bool {
    lex(content).into_iter().any(|x| x.0.is_err())
}

pub fn lex<'s>(content: &'s str) -> Vec<(Result<Token<'s>, ()>, Range<usize>)> {
    let mut state = 0;
    Token::lexer(content)
        .spanned()
        .map(|(x, s)| match x {
            Ok(ok) => spread(&mut state, ok)
                .into_iter()
                .map(|y| (Ok::<_, ()>(y), s.clone()))
                .collect::<Vec<_>>(),
            _ => [(x, s)].into_iter().collect(),
        })
        .flatten()
        .filter(|(x, _)| !matches!(x, Ok(Token::LineComment(_) | Token::BlockComment(_))))
        .collect::<Vec<_>>()
}

fn spread<'t, 's>(prev: &mut usize, t: Token<'s>) -> Vec<Token<'s>> {
    use self::Lay::*;
    match match t {
        Token::Lay(l) => l,
        x => return vec![x],
    } {
        Top => {
            let out = (1..=*prev)
                .rev()
                .into_iter()
                .map(|_| Token::LayEnd)
                .chain([Token::LayTop].into_iter())
                .collect();
            *prev = 0;
            out
        }
        End(n) => {
            let out = (n + 1..=*prev)
                .rev()
                .into_iter()
                .map(|_| Token::LayEnd)
                .chain([Token::LaySep].into_iter())
                .collect();
            *prev = n;
            out
        }
        Begin(x) => {
            assert!(x == *prev + 1, "A");
            *prev = x;
            vec![Token::LayBegin]
        }
        Sep(x) => {
            assert!(x == *prev, "B");
            *prev = x;
            vec![Token::LaySep]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    fn p(s: &'static str) -> String {
        lex(s)
            .iter()
            .map(|x| format!("{:?}", x.0))
            .collect::<Vec<_>>()
            .join("\n")
    }

    #[test]
    fn empty_string() {
        assert_snapshot!(p(""));
    }

    #[test]
    fn some_tokens() {
        assert_snapshot!(p("module A where (a, b, c)\nimport B as B\nfoo = 1 + B.t"));
    }

    #[test]
    fn lower() {
        assert_snapshot!(p("variable"));
    }

    #[test]
    fn string() {
        assert_snapshot!(p(r#""I like pie""#))
    }

    #[test]
    fn string_with_quote() {
        assert_snapshot!(p(r#""Qotes are \"rad\"""#))
    }

    #[test]
    fn raw_string_6() {
        assert_snapshot!(p("\"\"\"\"\"\""))
    }

    #[test]
    fn raw_string_7() {
        assert_snapshot!(p("\"\"\"\"\"\"\""))
    }

    #[test]
    fn raw_string_8() {
        assert_snapshot!(p("\"\"\"\"\"\"\"\""))
    }

    #[test]
    fn int_0() {
        assert_snapshot!(p("123"))
    }

    #[test]
    fn int_1() {
        assert_snapshot!(p("1_111_1"))
    }

    #[test]
    fn int_2() {
        assert_snapshot!(p("0xFEE"))
    }

    #[test]
    fn number_0() {
        assert_snapshot!(p("12.3"))
    }

    #[test]
    fn number_1() {
        assert_snapshot!(p("1e+10"))
    }

    #[test]
    fn number_2() {
        assert_snapshot!(p("1e+10"))
    }

    #[test]
    fn line_comment() {
        assert_snapshot!(p("a + -- this is a comment\nb"))
    }

    #[test]
    fn block_comment() {
        assert_snapshot!(p("a + {- this\n is\n a\n comment\n -} b"))
    }

    #[test]
    fn block_comment2() {
        assert_snapshot!(p("a +{- this\n is\n a\n comment\n -}b"))
    }

    #[test]
    fn symbol() {
        assert_snapshot!(p("<#>"))
    }

    #[test]
    fn hole() {
        assert_snapshot!(p("a = ?foo 1 2"))
    }

    #[test]
    fn forall() {
        assert_snapshot!(p("forall f. Monad f => f a -> f Unit"))
    }

    #[test]
    fn upper() {
        assert_snapshot!(p("Unit"))
    }

    #[test]
    fn qual() {
        assert_snapshot!(p("A.B.Unit A.B.var A.B.### A.B...."))
    }

    #[test]
    fn lower_with_single_quote() {
        assert_snapshot!(p("myFunction'"))
    }

    #[test]
    fn funky_string() {
        assert_snapshot!(p("\"\""))
    }

    #[test]
    fn char_dash() {
        assert_snapshot!(p("'-'"))
    }

    #[test]
    fn char_a() {
        assert_snapshot!(p("'A'"))
    }

    #[test]
    fn char_hex() {
        assert_snapshot!(p(r"'\x00'"))
    }

    #[test]
    fn simple_int() {
        assert_snapshot!(p(r"1"))
    }

    #[test]
    fn simple_number() {
        assert_snapshot!(p(r"1.0"))
    }

    #[test]
    fn symbol_paren_plus() {
        assert_snapshot!(p("(+)"))
    }

    #[test]
    fn symbol_paren_minus() {
        assert_snapshot!(p("(-)"))
    }

    #[test]
    fn this_wacky_thing_is_an_opperator() {
        assert_snapshot!(p("¤"))
    }

    #[test]
    fn some_char_special_cases() {
        assert_snapshot!(p(r#"'\n''\r''\t''\"''\\''\xF00'"#))
    }

    #[test]
    fn symbol_minus() {
        assert_snapshot!(p("-"))
    }

    #[test]
    fn swedish_lower() {
        assert_snapshot!(p("överförning"))
    }

    #[test]
    fn swedish_upper() {
        assert_snapshot!(p("Överförning"))
    }

    #[test]
    fn knows_if_start_of_line() {
        assert_snapshot!(p(r#"
import A as A

foo :: Int
foo = bar + baz
        "#))
    }

    #[test]
    fn tricky_do_notation() {
        assert_snapshot!(p(r#"
f = 
    do
        do
            a <- f b
            do
                x <- y
        e v

foo = bar + baz
        "#))
    }
}
