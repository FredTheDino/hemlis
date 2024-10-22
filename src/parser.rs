#[cfg(test)]
mod tests {
    use insta::assert_snapshot;

    macro_rules! gen_parser {
        ($a:ident, $p:ident) => {
            pub fn $a<'t>(src: &'t str) -> String {
                pub fn inner<'t>(src: &'t str) -> Result<String, String> {
                    use crate::{ast::Ast, grammer, lexer};
                    use std::io::BufWriter;

                    let parser = grammer::$p::new();
                    let out = parser
                        .parse(
                            lexer::lex(&src)
                                .into_iter()
                                .map(|(token, span)| Ok((span.start, token?, span.end))),
                        )
                        .map_err(|x| format!("{:?}", x))?;

                    let mut buf = BufWriter::new(Vec::new());
                    out.show(0, &mut buf).map_err(|x| format!("{:?}", x))?;
                    let inner = buf.into_inner().map_err(|x| format!("{:?}", x))?;
                    String::from_utf8(inner).map_err(|x| format!("{:?}", x))
                }
                match inner(src) {
                    Ok(s) | Err(s) => s,
                }
            }
        };
    }

    gen_parser!(p_header, HeaderParser);
    gen_parser!(p_import, ImportParser);

    #[test]
    fn empty_string() {
        assert_snapshot!(p_header(""));
    }

    #[test]
    fn normal_definition() {
        assert_snapshot!(p_header("module A (a, b, c) where"));
    }

    #[test]
    fn everything_header() {
        assert_snapshot!(p_header(r#"
module A.B.C (a, class B, C, D(..), E(F, G), 
 (+), type (+), module H) where

import A as A
import A.B.C as A.C
import A.B.C (a, class B, 
 C, D(..), E(F, G), (+), type (+))
import A.B.C hiding (foo)
        "#));
    }

    #[test]
    fn simple_import() {
        assert_snapshot!(p_import("import A as A"))
    }

    #[test]
    fn import_with_newlines() {
        assert_snapshot!(p_import("import A (foo\n , bar\n , baz)"))
    }
}
