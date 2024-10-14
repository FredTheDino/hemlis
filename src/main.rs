use std::{env, fs};

use rayon::prelude::*;

mod lexer;

fn main() {
    env::args()
        .skip(1)
        .collect::<Vec<_>>()
        .par_iter()
        .for_each(|arg| match fs::read_to_string(&arg) {
            Err(e) => println!("ERR: {} {:?}", arg, e),
            Ok(f) => {
                if lexer::contains_lex_errors(&f) {
                    if false {
                        println!("{}", arg);
                    } else {
                        for i in lexer::lex(&f) {
                            println!("{}:: {:?}", arg, i);
                        }
                    }
                }
            }
        })
}
