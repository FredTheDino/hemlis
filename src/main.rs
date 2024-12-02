use std::{
    env,
    fs::{self},
};

use ast::Ast;
use dashmap::DashMap;
use rayon::prelude::*;

pub mod ast;
pub mod lexer;
pub mod parser;
pub mod symtab;

#[allow(dead_code)]
fn main() {
    if env::var("HEMLIS_GEN").is_ok() {
        linear_parse_generate_test();
    } else {
        parse_modules();
    }
}

fn linear_parse_generate_test() {
    let args = env::args()
        .map(|x| x.to_string())
        .skip(1)
        .collect::<Vec<_>>();
    args.into_iter()
        .enumerate()
        .for_each(|(i, arg)| match fs::read_to_string(arg.clone()) {
            Err(e) => {
                panic!("ERR: {} {:?}", arg, e);
            }
            Ok(src) => {
                let l = lexer::lex(&src, ast::Fi(i));
                let n = DashMap::new();
                let mut p = parser::P::new(&l, &n);
                parser::module(&mut p);
                if p.i < p.tokens.len() {
                    p.errors.push(parser::Serror::NotAtEOF(p.span(), p.peekt()))
                }
                if p.errors.is_empty() {
                    return;
                }
                println!("{} {}", arg, p.errors.len());
            }
        });
}

fn parse_modules() {
    let args = env::args()
        .map(|x| x.to_string())
        .skip(1)
        .collect::<Vec<_>>();
    args.par_iter()
        .enumerate()
        .for_each(|(i, arg)| match fs::read_to_string(arg.clone()) {
            Err(e) => {
                panic!("ERR: {} {:?}", arg, e);
            }
            Ok(src) => {
                use std::io::BufWriter;

                let l = lexer::lex(&src, ast::Fi(i));
                let n = DashMap::new();
                let mut p = parser::P::new(&l, &n);

                let out = parser::module(&mut p);
                if p.i < p.tokens.len() {
                    p.errors.push(parser::Serror::NotAtEOF(p.span(), p.peekt()))
                }

                if true {
                    let mut buf = BufWriter::new(Vec::new());
                    out.show(0, &mut buf).unwrap();
                    let inner = String::from_utf8(
                        buf.into_inner().map_err(|x| format!("{:?}", x)).unwrap(),
                    )
                    .map_err(|x| format!("{:?}", x))
                    .unwrap();
                    println!(
                        "{} of {}\n===\n{}\n===\n{}\n===\n{}",
                        p.i,
                        p.tokens.len(),
                        p.errors
                            .iter()
                            .map(|x| { format!("{:?}\n", x) })
                            .collect::<Vec<_>>()
                            .join("\n"),
                        if env::var("HEMLIS_TOKENS").is_ok() {
                            p.tokens
                                .iter()
                                .map(|(a, s)| format!("{:?} {:?}", a, s))
                                .collect::<Vec<_>>()
                                .join("\n")
                        } else {
                            "".to_string()
                        },
                        if env::var("HEMLIS_TREE").is_ok() {
                            inner
                        } else {
                            "".to_string()
                        }
                    );
                }
            }
        });
}
