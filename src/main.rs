use std::{
    env, fs,
    time::{Duration, Instant},
};

use ast::Ast;
use rayon::prelude::*;

mod ast;
mod lexer;
mod parser;

fn main() {
    let s: Duration = env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .par_iter()
        .map(|arg| match fs::read_to_string(arg) {
            Err(e) => {
                println!("ERR: {} {:?}", arg, e);
                Instant::now().elapsed()
            }
            Ok(src) => {
                use std::io::BufWriter;

                let start = Instant::now();

                let l = lexer::lex(&src);
                let mut p = parser::P::new(0, &l);

                let out = parser::module(&mut p);
                if p.i != p.tokens.len() {
                    p.errors.push(parser::Serror::NotAtEOF)
                }
                let d = start.elapsed();
                if p.errors.len() > 0 || d > Duration::from_millis(100) {
                    println!("{} {} {}", arg, p.errors.len(), d.as_millis());
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
                        "{} of {}\n===\n{:#?}\n==={}\n===\n{}",
                        p.i,
                        p.tokens.len(),
                        l, 
                        inner,
                        p.errors
                            .iter()
                            .map(|x| format!("{:?}", x))
                            .collect::<Vec<_>>()
                            .join("\n")
                    )
                }

                d
            }
        })
        .sum();
    println!("TOTAL: {}", s.as_millis());
}
