use logos::Logos;

// TODO: Count indent and offset
// TODO: Might not support unicode?

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
            Some(_) => {
                lex.bump(at + 1);
                return Some(lex.slice());
            }
            None => return None,
        }
    }
    None
}

fn parse_raw_string<'t>(lex: &mut logos::Lexer<'t, Token<'t>>) -> Option<&'t str> {
    while let Some(at) = lex.remainder().find("\"\"\"") {
        match dbg!(lex.remainder().get(at+3..at + 4)) {
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
    #[token("->")]
    RightArrow,
    #[token("=>")]
    RightFatArrow,
    #[token("::")]
    DoubleColon,
    #[token("`")]
    Tick,
    #[token(",")]
    Comma,
    #[token("(->)")]
    SymbolArr,
    #[token("(..)")]
    SymbolName,

    #[token("forall")]
    Forall,

    #[regex("([A-Z][[:alnum:]]*\\.)+")]
    Qual(&'t str),

    #[regex("[_a-z][[:alnum:]]*")]
    Lower(&'t str),

    #[regex("[A-Z][:alnum:]*")]
    Upper(&'t str),

    #[regex(r"[!|#|$|%|&|*|+|.|/|<|=|>|?|@|\\|^||\\|-|~|:]+")]
    Symbol(&'t str),

    #[regex("\\?[_a-z][:alnum:]*")]
    Hole(&'t str),

    #[regex("0x[[:xdigit:]]+")]
    HexInt(&'t str),

    #[regex("[[:digit:]][[:digit:]|_]+")]
    Int(&'t str),

    #[regex(r"([+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?|[+-]?|[+-]?)")]
    Number(&'t str),

    #[regex(r"'[(.)|(\\x[:xdigit:])]'")]
    Char(&'t str),

    #[regex("\"", |lex| parse_string(lex))]
    String(&'t str),

    #[regex("\"\"\"", |lex| parse_raw_string(lex))]
    RawString(&'t str),

    #[regex("--.*\n")]
    LineComment(&'t str),

    #[regex(r"\{-.*-\}")]
    BlockComment(&'t str),

    #[regex(r"\s+")]
    Whitespace(&'t str),
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
}
