use std::{env, fs};

use ast::Ast;
use rayon::prelude::*;

mod ast;
mod grammer;
mod lexer;

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .par_iter()
        .for_each(|arg| match fs::read_to_string(&arg) {
            Err(e) => println!("ERR: {} {:?}", arg, e),
            Ok(f) => {
                let parser = grammer::ModuleParser::new();
                let out = parser.parse(
                    lexer::lex(&f)
                        .into_iter()
                        .map(|(token, span)| Ok((span.start, token?, span.end))),
                ).expect("XX");

                out.show(0, &mut std::io::stdout()).expect("YY");
            }
        })
}
