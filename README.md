# Hemlis
A faster frontend/lsp for PureScript.

## What currently exists
 - A simple parser that parses most of PureScript
 - Basic syntactical analysis (resolving names and references)
 - A simple goto-definition/list usages LSP
 - Most of it is somewhat fast

## Known limitations
 - Let-bindings (and where-bindings) in Purs are preprocessed to resolve some dependency orders. This is pretty wild and is currently not implemented - a simpler approach is used though this needs to be fixed. 
 - Only syntactical analysis - there is no type information
 - Comments are completely ignored - meaning they are not trivial to show in the LSP

## Possible extensions in no particular order (the short and incomplete list)
 - Rename symbol support
 - Supply the edtior with tokens to render things like resolved names
 - Better parser error recovery
 - Parse with correct operator precedence
 - Code actions
    - Import this symbol
    - Extract to function
    - Inline function
    - Remove unnessecary parenthesis
    - Add to exports
 - Find exports that are never imported
 - Pre-populate a match block with all constructors
 - A typechecker
 - Completions - list available names
 - Documentation on hover
 - Syntactic checks/linting (e.g. `$> pure` is probably wrong)
 - If-to-match and match-to-if
