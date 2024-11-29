# Hemlis
A faster frontend for PureScript.

Focuses:
 - Speed
 - Resiliance

Current scope and todo-list:
 1. CST parser
    - https://docs.google.com/document/d/1wXbmWPXcMeubI3Q8NaNBvxyJX-H-KZ0G7WHlmjNvEeQ/edit#heading=h.yujvlccyg2h7
 2. Formatter
 3. CTags
 4. Simple LSP with goto-definition and list-usages and the like
 5. Start looking at the typechecker
 6. Typechecker
 7. Codegen
 8. A repl?


## Evaluation of Lexers

 - Lexers are notoriously hard to write correctly and efficently by hand. Generating them is however quite easy.
 - There are a number of Rust libraries for this.
 - Alternatives
    - Logos
        - Uses Rust-Syntax
        - The most used
        - Has the most documentation
        - I know it's fast (~1GB/s on my machine)
        - I've used it before
        - Supports the linenumber and lineoffset if managed carefully
    - https://crates.io/crates/lexgen
        - Uses special syntax
        - Less used
        - Has know limitations
    - https://crates.io/crates/lrlex
        - Uses special syntax
    - Some parser library?
        - We don't nessecarily want a parser

## Current gameplan

Resources: https://github.com/Kixiron/rust-langdev?tab=readme-ov-file#lexers

Check status every week
https://logos.maciej.codes/

1. Write a lexer
2. Write a CST-parser
3. Write a formatter
4. Optimizse the formatter
5. Convert to AST (or similar format) 
6. Start doing LSP queries
