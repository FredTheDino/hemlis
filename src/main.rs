use std::{
    env,
    fs::{self, File},
    hash::{DefaultHasher, Hash, Hasher},
};

use ast::{Ast, Span};
use lexer::Token;

mod ast;
mod lexer;
mod parser;

fn main() {
    if env::var("PURRING_GEN").is_ok() {
        linear_parse_generate_test();
    } else {
        parse_single_decl();
    }
}

fn format_decl_from_tokens<'s>(
    (lo, hi): (usize, usize),
    toks: &'s Vec<(Result<Token<'s>, ()>, std::ops::Range<usize>)>,
) -> String {
    use std::io::BufWriter;
    use std::io::Write;

    let mut out = BufWriter::new(Vec::new());
    let ii = 2;
    let mut indent = 2;
    for (t, _) in &toks[lo..hi] {
        match t.unwrap() {
            Token::LeftParen => write!(out, "("),
            Token::RightParen => write!(out, ")"),
            Token::LeftBrace => write!(out, "{{"),
            Token::RightBrace => write!(out, "}}"),
            Token::LeftSquare => write!(out, "["),
            Token::RightSquare => write!(out, "]"),
            Token::LeftArrow => write!(out, "<-"),
            Token::RightArrow => write!(out, "->"),
            Token::RightFatArrow => write!(out, "=>"),
            Token::ColonColon => write!(out, "::"),
            Token::Slash => write!(out, "\\"),
            Token::At => write!(out, "@"),
            Token::Pipe => write!(out, "|"),
            Token::Tick => write!(out, "`"),
            Token::Comma => write!(out, ","),
            Token::Class => write!(out, "class"),
            Token::Data => write!(out, "data"),
            Token::Infixr => write!(out, "infixr"),
            Token::Infixl => write!(out, "infixl"),
            Token::Infix => write!(out, "infix"),
            Token::Derive => write!(out, "derive"),
            Token::Type => write!(out, "type"),
            Token::Newtype => write!(out, "newtype"),
            Token::Foreign => write!(out, "foreign"),
            Token::Instance => write!(out, "instance"),
            Token::If => write!(out, "if"),
            Token::Then => write!(out, "then"),
            Token::Else => write!(out, "else"),
            Token::Case => write!(out, "case"),
            Token::Of => write!(out, "of"),
            Token::Let => write!(out, "let"),
            Token::In => write!(out, "in"),
            Token::Where => write!(out, "where"),
            Token::Do => write!(out, "do"),
            Token::Ado => write!(out, "ado"),
            Token::As => write!(out, "as"),
            Token::Qual(_) => {
                write!(out, "A.").unwrap();
                continue;
            }
            Token::Lower(
                l @ ("module" | "import" | "hiding" | "role" | "nominal" | "representational"
                | "phantom" | "true" | "false" | "forall"),
            ) => {
                write!(out, "{}", l)
            }

            Token::Lower(_) => write!(out, "a"),
            Token::Upper(_) => write!(out, "A"),
            Token::Op(s) => write!(out, "{}", s),
            Token::Symbol(s) => write!(out, "{}", s),
            Token::Hole(_) => write!(out, "?a"),
            Token::HexInt(_) => write!(out, "0xAA"),
            Token::Number(_) => write!(out, "0"),
            Token::Char(_) => write!(out, "'a'"),
            Token::String(_) => write!(out, "\"a\""),
            Token::RawString(_) => write!(out, "\"\"\"a\"\"\""),
            Token::LineComment(_) => {
                write!(out, "-- A\n{:indent$}", "", indent = indent).unwrap();
                continue;
            }
            Token::BlockComment(_) => write!(out, "{{- -}}"),
            Token::Indent(_) => panic!("Should pre translated to LayBegin and friends"),
            Token::LayBegin => {
                indent += ii;
                write!(out, "\n{:indent$}", "", indent = indent).unwrap();
                continue;
            }
            Token::LayEnd => {
                indent -= ii;
                write!(out, "\n{:indent$}", "", indent = indent).unwrap();
                continue;
            }
            Token::LaySep => {
                write!(out, "\n{:indent$}", "", indent = indent).unwrap();
                continue;
            }
            Token::LayTop => {
                indent -= ii;
                write!(out, "\n{:indent$}", "", indent = indent).unwrap();
                continue;
            }
        }
        .unwrap();
        write!(out, " ").unwrap();
    }
    String::from_utf8(out.into_inner().map_err(|x| format!("{:?}", x)).unwrap()).unwrap()
}

fn linear_parse_generate_test() {
    let args = env::args()
        .map(|x| x.to_string())
        .skip(1)
        .collect::<Vec<_>>();
    args.into_iter()
        .for_each(|arg| match fs::read_to_string(arg.clone()) {
            Err(e) => {
                panic!("ERR: {} {:?}", arg, e);
            }
            Ok(src) => {
                let l = lexer::lex(&src);
                let mut p = parser::P::new(0, &l);
                parser::module(&mut p);
                if p.i != p.tokens.len() {
                    p.errors.push(parser::Serror::NotAtEOF(p.span(), p.peekt()))
                }
                if p.errors.is_empty() {
                    return;
                }
                println!("{} {}", arg, p.errors.len());
                for e in p.errors.iter() {
                    match e {
                        parser::Serror::FailedToParseDecl(Span::Known(from, to, _), _, lo, hi) => {
                            let xx = &src[*from..*to];
                            let lxx = lexer::lex(xx);
                            let mut pxx = parser::P::new(0, &lxx);
                            let x = parser::decl(&mut pxx).is_some();

                            let aa = format_decl_from_tokens((*lo, *hi), &l);
                            let laa = lexer::lex(&aa);
                            let mut paa = parser::P::new(0, &laa);
                            let a = parser::decl(&mut paa).is_some();

                            if x != a {
                                continue;
                            }

                            if paa.errors.len() != pxx.errors.len() {
                                continue;
                            }

                            if paa.errors.iter().zip(pxx.errors.iter()).any(|(a, b)| {
                                    a.same_kind_of_error(b)
                            }) {
                                return;
                            }


                            println!("{}", xx);
                            println!("{}", aa);

                            for (a, b) in paa.errors.iter().zip(p.errors.iter()) {
                                println!("A: {:?}", a);
                                println!("B: {:?}", b);
                                assert!(
                                    a.same_kind_of_error(b),
                                    "Got different errors from the original test"
                                )
                            }

                            let mut h = DefaultHasher::new();
                            (&arg, *lo, *hi).hash(&mut h);
                            let name = h.finish();
                            let name = format!("tests/from_lesslie/{}.purs", name);

                            use std::io::Write;

                            let mut file = File::create_new(name.clone()).expect(&name.clone());
                            write!(file, "{}", aa).unwrap();
                            drop(file);

                            println!("Created new test: {}", name);
                            panic!("Stopping");
                        }
                        _ => continue,
                    }
                }
            }
        });
}

fn parse_single_decl() {
    let args = env::args()
        .map(|x| x.to_string())
        .skip(1)
        .collect::<Vec<_>>();
    args.into_iter()
        .for_each(|arg| match fs::read_to_string(arg.clone()) {
            Err(e) => {
                panic!("ERR: {} {:?}", arg, e);
            }
            Ok(src) => {
                use std::io::BufWriter;

                let l = lexer::lex(&src);
                let mut p = parser::P::new(0, &l);

                let out = parser::decl(&mut p);
                if p.i != p.tokens.len() {
                    p.errors.push(parser::Serror::NotAtEOF(p.span(), p.peekt()))
                }

                if !p.errors.is_empty() {
                    let mut buf = BufWriter::new(Vec::new());
                    out.show(0, &mut buf).unwrap();
                    let inner = String::from_utf8(
                        buf.into_inner().map_err(|x| format!("{:?}", x)).unwrap(),
                    )
                    .map_err(|x| format!("{:?}", x))
                    .unwrap();
                    println!(
                        "{} of {}\n===\n{}\n===\n{}",
                        p.i,
                        p.tokens.len(),
                        p.errors
                            .iter()
                            .map(|x| format!("{:?}", x))
                            .collect::<Vec<_>>()
                            .join("\n"),
                        p.tokens.iter().map(|(a, s)| format!("{:?} {:?}", a, s)).collect::<Vec<_>>().join("\n"),
                        // inner,
                    );
                }
            }
        });
}
