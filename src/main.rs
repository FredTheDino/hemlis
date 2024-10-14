use std::{env, fs};

mod lexer;

fn main() {
    for arg in env::args().skip(1) {
        match fs::read_to_string(&arg) {
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
        }
    }
}
