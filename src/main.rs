use std::{env, fs, time::{Duration, Instant}};

use rayon::prelude::*;

mod ast;
mod lexer;
mod parser;

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .par_iter()
        .for_each(|arg| match fs::read_to_string(arg) {
            Err(e) => println!("ERR: {} {:?}", arg, e),
            Ok(src) => {
                use std::io::BufWriter;

                let start = Instant::now();

                let l = lexer::lex(&src);
                let mut p = parser::P::new(0, &l);

                let mut buf = BufWriter::new(Vec::new());
                parser::module(&mut p); // .show(0, &mut buf).unwrap();
                                        // let inner = buf.into_inner().map_err(|x| format!("{:?}", x)).unwrap();
                if p.i != p.tokens.len() {
                    p.errors.push(parser::Serror::NotAtEOF)
                }
                let d = start.elapsed();
                if p.errors.len() > 0 || d > Duration::from_millis(100) {
                    println!("{} {} {}", arg, p.errors.len(), d.as_millis());
                }

                /*
                println!(
                    "{} of {}\n===\n{}",
                    p.i,
                    p.tokens.len(),
                    // String::from_utf8(inner)
                    //     .map_err(|x| format!("{:?}", x))
                    //     .unwrap(),
                    p.errors
                        .iter()
                        .map(|x| format!("{:?}", x))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
                */
            }
        })
}
