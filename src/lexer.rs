use logos::Logos;

// TODO: Count indent and offset
// TODO: Might not support unicode?
// TODO: Understand indentation 

fn parse_string<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    while let Some(at) = lex.remainder().find("\"") {
        if at == 0 {
            lex.bump(at + 1);
            return Some(lex.slice());
        }
        match lex.remainder().get(at - 1..at + 1) {
            Some("\\\"") => {
                lex.bump(at + 1);
            }
            _ => {
                lex.bump(at + 1);
                return Some(lex.slice());
            }
        }
    }
    None
}

fn parse_raw_string<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    while let Some(at) = lex.remainder().find("\"\"\"") {
        match lex.remainder().get(at+3..at + 4) {
            Some("\"") => {
                lex.bump(at + 1);
            }
            _ => {
                lex.bump(at + 3);
                return Some(lex.slice());
            }
        }
    }
    None
}

fn parse_block_comment<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    if let Some(at) = lex.remainder().find("-}") {
        lex.bump(at + 2);
        return Some(lex.slice());
    }
    None
}

#[derive(Logos, Debug, PartialEq, Eq, Clone, Copy)]
pub enum Token<'t> {
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
    #[token("->", priority=10000)]
    RightArrow,
    #[token("=>")]
    RightFatArrow,
    #[token("::")]
    DoubleColon,
    #[token("`")]
    Tick,
    #[token(",")]
    Comma,

    #[token("forall")]
    Forall,

    // TODO: We need to parse this with a custom function, We can eat greadily if we tokenize
    // ourselves here.
    #[regex("[A-Z][[:alnum:]]*\\.")]
    Qual(&'t str),

    #[regex("[_a-z][[:alnum:]']*")]
    Lower(&'t str),

    #[regex("[A-Z][[:alnum:]]*", priority=200000)]
    Upper(&'t str),

    #[regex(r"[!|#|$|%|&|*|+|.|/|<|=|>|?|@|\\|^||\\|\-|~|:]+")]
    Symbol(&'t str),

    #[regex("\\?[_a-z][[:alnum:]]*")]
    Hole(&'t str),

    #[regex("0x[[:xdigit:]]+")]
    HexInt(&'t str),

    // contains e or . => Number, otherwise => Int 
    #[regex(r"([\d][\d|_]*|([\d]+\.[\d]*|[\d]*\.[\d]+))|[\d]+e(-|\+)?[\d]+")]
    Number(&'t str),

    // TODO: We need to parse this with a custom function, I have trubble expressing this with
    // regex 
    #[regex(r"'.{1,4}'")]
    Char(&'t str),

    #[regex("\"", |lex| parse_string(lex))]
    String(&'t str),

    #[regex("\"\"\"", |lex| parse_raw_string(lex))]
    RawString(&'t str),

    #[regex("--.*\n")]
    LineComment(&'t str),

    #[regex(r"\{-", |lex| parse_block_comment(lex))]
    BlockComment(&'t str),

    #[regex(r"\s+")]
    Whitespace(&'t str),
}

pub fn contains_lex_errors(content: &str) -> bool {
    lex(content).any(|x| x.is_err())
}


pub fn lex<'s>(content: &'s str) -> logos::Lexer<'s, Token<'s>> {
    Token::lexer(content)
}
#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    fn p(s: &'static str) -> String {
        Token::lexer(s)
            .map(|x| format!("{:?}", x))
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
    fn symbol_minus() {
        assert_snapshot!(p("-"))
    }
}
