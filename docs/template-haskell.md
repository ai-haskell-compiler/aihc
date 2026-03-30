# TemplateHaskell and TemplateHaskellQuotes

The `TemplateHaskell` and `TemplateHaskellQuotes` extensions introduce several new syntactic constructs to Haskell. Since our goal is purely parsing, the semantics and type-checking of these features are not of concern here; only the syntax matters.

## Extensions Difference
- `TemplateHaskellQuotes` enables the quotation syntax: `[| ... |]`, typed quotes `[|| ... ||]`, and name quoting `'f` / `''T`. It does *not* enable top-level splices or executing TH code.
- `TemplateHaskell` enables all of `TemplateHaskellQuotes` plus the splice syntax (`$x`, `$$x`) outside of quotations (thus allowing AST execution during compile time).

## Syntactic Constructs

### 1. Quotations (Oxford Brackets)
Quotations allow writing Haskell code and treating it as syntax tree representations.
There are several types of untyped quotations, corresponding to different parsing contexts:
- **Expression Quotes**: `[| ... |]` or `[e| ... |]` where `...` is parsed as an expression.
- **Declaration Quotes**: `[d| ... |]` where `...` is parsed as a list of top-level declarations.
- **Type Quotes**: `[t| ... |]` where `...` is parsed as a type.
- **Pattern Quotes**: `[p| ... |]` where `...` is parsed as a pattern.

### 2. Typed Quotations
Typed quotes provide a typed view of an expression.
- **Typed Expression Quotes**: `[|| ... ||]` or `[e|| ... ||]`. The inside is parsed as an expression.

### 3. Splices
Splices allow evaluating an expression (which computes an AST) and inserting it directly into the code.
- **Untyped Splices**: Written as `$x` or `$(...)` where `x` is an identifier or `...` is an expression. Note that there must be no space between the `$` and the following expression to disambiguate it from the infix application operator `$`.
- **Top-level Splices**: A splice can appear at the top-level as a declaration, e.g., `$(makeLenses ''MyType)`.

### 4. Typed Splices
Typed splices correspond to the typed quotations.
- **Typed Splices**: Written as `$$x` or `$$(...)`. Again, there must be no space after the `$$`.

### 5. Name Quoting
Identifiers and types can be quoted to extract their `Name` representations.
- **Value/Function Names**: `'name` (single quote prefix). Used for things in expression contexts (like variables or data constructors). Example: `'map`, `'True`.
- **Type/Class Names**: `''Name` (double quote prefix). Used for things in type contexts (like type constructors or classes). Example: `''Int`, `''Eq`.

Because ` ' ` is overloaded, a space may be required when the quote would otherwise be parsed as a character literal or cause a token collision. For example, `'f'7` (name ends with `'7`) does not require a space, but a space would be needed after the quote if it would otherwise form a character literal.

### 6. Quasi-Quotations
`QuasiQuotes` is a related extension but often discussed with TH. It introduces custom parsers, typically written as `[quoter| ... |]`. This syntax is structurally similar to quotations, and the parser generally just needs to identify the `quoter` and scoop the raw string until `|]`. (Note: Not strictly TH, but shares its syntax style.)

## Parser Implications
When these extensions are enabled:
1. Lexer should recognize `[|`, `[e|`, `[d|`, `[t|`, `[p|`, `|]`, `[||`, `[e||`, `||]`, `$`, `$$`, `'` and `''` as distinct tokens in relevant contexts.
2. The `$` and `$$` operators, when not followed by a space, must be parsed as a splice rather than an infix operator.
3. Nested splices inside quotations (e.g., `[| 1 + $(foo) |]`) should be properly tracked.
4. Name quotation `'id` and `''Id` requires care so they aren't parsed as unclosed character literals or promoted data constructors (`DataKinds`).