# Parser Maintainability Review

## Scope

This review covers:

- `components/haskell-parser/src/Parser.hs`
- `components/haskell-parser/src/Parser/Internal/Common.hs`
- `components/haskell-parser/src/Parser/Internal/Decl.hs`
- `components/haskell-parser/src/Parser/Internal/Expr.hs`

The focus is not feature completeness. It is robustness, maintainability, error behavior, and the kinds of structural issues that tend to accumulate technical debt in hand-written parsers.

## Executive Summary

The parser is readable and has a clear "small combinators over token stream" style, but it is already carrying a few debt patterns that will get more expensive as grammar coverage grows:

1. Public API surface and actual behavior have drifted apart.
2. Several grammar decisions are encoded with heuristic post-classification instead of explicit syntax categories.
3. Backtracking is broad and frequent, which makes ambiguity harder to reason about and error reporting weaker.
4. Core grammar fragments are duplicated across modules.
5. Span handling is manually repeated across many AST constructors.
6. Some unsupported forms are at risk of being misparsed instead of rejected clearly.

If only a few changes are made, the highest-value ones are:

- thread `ParserConfig` through for real, or remove it until it is real
- introduce explicit "unsupported declaration" guards before generic binding parsers
- factor duplicated grammar helpers into shared parsers
- replace broad `try` chains with prefix-driven dispatch
- centralize span access and token expectation/error labeling

## High-Priority Recommendations

### 1. Eliminate API drift around `ParserConfig`

`ParserConfig` is exposed as part of the public parser API, but it is currently ignored by every parse entrypoint in `Parser.hs` (`parseExprAt`, `parsePatternAt`, `parseTypeAt`, `parseModuleAt`). The only field, `allowLineComments`, is not referenced anywhere else in the codebase.

Why this matters:

- Callers can reasonably assume the config changes parser behavior, but today it does not.
- Dead configuration fields are a long-term maintenance trap because they become compatibility obligations.
- This also makes tests less trustworthy because they exercise a surface that is partly fake.

Recommendation:

- Either wire `ParserConfig` into lexing/parsing now, or remove it from the public surface until there is real behavior behind it.
- If configuration is expected to grow, thread a parser environment through the internal modules instead of continuing with ignored parameters.
- Add a test that proves the config changes behavior whenever a new config field is added.

Relevant code:

- `Parser.hs`: `defaultConfig`, `parseExprAt`, `parsePatternAt`, `parseTypeAt`, `parseModuleAt`

### 2. Reject unsupported declaration forms explicitly instead of letting them fall through

`declParser` ends in `valueDeclParser`, and the lexer only treats a small subset of words as hard keywords. That means contextual forms such as `type`, `class`, `instance`, `foreign`, `newtype`, `default`, `infix`, `infixl`, and `infixr` are mostly recognized by parser shape rather than lexical category.

The dangerous part is not "missing support". The dangerous part is "missing support that can still parse as something else".

Examples of why this is risky:

- The AST already contains `DeclFixity`, `DeclTypeSyn`, and `DeclDefault`, but `declParser` does not build them.
- Because `type` is still lexed as an identifier, an unsupported declaration such as a type synonym can potentially drift into generic binding parsing instead of failing with a clear message.
- As syntax coverage expands, these silent fallthrough cases become much harder to notice than ordinary parse failures.

Recommendation:

- Add a front-loaded "reserved declaration forms" classifier before `valueDeclParser`.
- For forms the parser does not support yet, fail explicitly with a labeled parse error such as "type synonym declarations are not implemented yet".
- Keep the unsupported guard close to `declParser`, not scattered across expression and local-declaration parsers.
- Add regression tests that ensure unsupported top-level forms fail clearly and do not produce `DeclValue`.

Relevant code:

- `Parser/Internal/Decl.hs`: `declParser`
- `Parser/Ast.hs`: `DeclFixity`, `DeclTypeSyn`, `DeclDefault`
- `Parser/Lexer.hs`: `keywordTokenKind`

### 3. Reduce broad backtracking and move to prefix-driven dispatch

The parser relies heavily on `MP.try` across top-level declaration parsing, local declaration parsing, pattern parsing, expression parsing, and several tail parsers.

Examples:

- `declParser`
- `localDeclParser`
- `exprCoreParser`
- `rhsParserWithArrow`
- tuple/list tail parsing in `Expr.hs`

Why this matters:

- Broad backtracking makes parse behavior harder to reason about when new forms are added.
- It weakens error locality because the parser often commits late.
- It increases the odds of accidental ambiguity between "not implemented yet" and "wrongly parsed as something else".

Recommendation:

- Introduce small prefix classifiers based on the next token or next few tokens.
- Left-factor grammars that share a long common prefix.
- Reserve `try` for genuinely overlapping branches, not as the default mechanism for choosing alternatives.
- For declarations, dispatch first on the leading identifier or keyword-like token, then parse only the relevant family.
- For tuple/list/paren forms, centralize the delimiter-driven branching so the fallback behavior is easier to audit.

## Medium-Priority Recommendations

### 4. Centralize duplicated grammar fragments

Several grammar fragments exist in near-duplicate form across modules:

- constraint parsing exists in both `Decl.hs` and `Expr.hs`
- typed signature item parsing is repeated for declarations, class items, instance items, and local declarations
- function-binding construction is repeated for top-level declarations, local declarations, and instance items
- plain/braced list parsing is repeated for module bodies, `where` clauses, `case` alternatives, class items, instance items, statements, and local declarations
- same-line application logic is repeated for expressions, patterns, and types

Why this matters:

- Duplicated parser fragments drift over time.
- Fixes land in one copy and not the others.
- Subtle differences become accidental rather than intentional.

Recommendation:

- Extract shared helpers into a small internal grammar utility layer.
- Create one place for:
  - constraint parsing
  - comma/semicolon separated lists with brace handling
  - same-line application continuations
  - function-bind AST construction
  - repeated signature parsers
- Where behavior is intentionally different, encode that as an explicit parameter rather than a copy-paste fork.

Relevant code:

- `Parser/Internal/Decl.hs`: `declContextParser`, `constraintParser`, class/instance item parsers, value-binding builders
- `Parser/Internal/Expr.hs`: `constraintsParser`, `constraintParser`, `plainDeclsParser`, `bracedDeclsParser`, `sameLine*Parser`

### 5. Replace heuristic name classification with explicit syntax categories

The parser currently uses casing and textual heuristics in several places:

- `moduleNameParser = identifierTextParser`
- `isTypeName`
- `isConLikeName`
- `typeIdentifierParser` deciding between `TVar` and `TCon`
- export/import parsing deciding between `ExportVar`/`ExportAbs` and `ImportItemVar`/`ImportItemAbs`

Why this matters:

- These rules encode syntax decisions as semantic guesses.
- Module names, constructor names, type constructors, variables, and operator namespaces are distinct grammar categories.
- Ad hoc classification gets harder to extend once syntax like `pattern`, richer namespaces, or operator exports/imports are added.

Recommendation:

- Introduce explicit parsers for categories such as `varid`, `conid`, module names, constructor operators, and value operators.
- Avoid interpreting the same raw identifier differently in multiple places.
- Keep post-parse semantic interpretation separate from lexical/syntactic recognition.

A concrete example: `moduleNameParser` should not just be `identifierTextParser`; it should reflect whatever module-name grammar the lexer/token stream is expected to support.

### 6. Improve parser error messages and token expectations

`tokenSatisfy` currently fails with the generic message `"token"`, and several layout-sensitive helpers fail with messages like `"line break"` or `"not a tuple section"`.

Why this matters:

- Hand-written parsers live or die on diagnostics.
- Broad `try` plus generic failure text makes real parse bugs harder to understand.
- Once the parser is used for corpus-scale progress tracking, bad diagnostics slow down triage significantly.

Recommendation:

- Replace the generic `tokenSatisfy` failure path with labeled expectations.
- Prefer helpers like "expected identifier", "expected constructor name", "expected `)`", or "expected declaration after `where`".
- Use Megaparsec labels consistently on the entrypoints for major grammar families.
- Consider custom parse errors for "recognized but unsupported syntax" so unsupported forms are clearly distinguished from malformed input.

Relevant code:

- `Parser/Internal/Common.hs`: `tokenSatisfy`
- `Parser/Internal/Expr.hs`: `sameLineAtomExprParser`, `sameLinePatternAtomParser`, `sameLineTypeAtomParser`, `parseTupleSection`

### 7. Consolidate span handling behind a shared abstraction

Span plumbing is currently spread across:

- `withSpan`
- `exprSourceSpan`
- `typeSourceSpan`
- `patternSourceSpan`
- `declSourceSpan`
- `mergeSourceSpans`

Why this matters:

- Every new AST constructor requires updates in multiple functions.
- Span logic is now split across modules instead of being attached to the AST types themselves.
- Helpers like `sameLine*Parser` also carry `NoSourceSpan -> 1` fallback logic repeatedly.

Recommendation:

- Introduce a `HasSourceSpan` typeclass or equivalent shared abstraction.
- Put all span extraction in one place.
- Keep merging and "end span of list" helpers with the span abstraction rather than in grammar modules.
- Turn on compiler warnings for incomplete pattern matches if they are not already enforced in CI.

This will not change parser behavior directly, but it will reduce maintenance cost every time the AST evolves.

### 8. Separate syntax recognition from semantic validation

Some parser branches mix grammar recognition with semantic or policy checks:

- `localTypeSigDeclParser` accepts only local type signatures whose parsed type contains an explicit `forall`
- export/import classification uses capitalization to decide AST constructor shape

Why this matters:

- These choices are harder to change later because they are baked into parse control flow.
- Grammar acceptance and language-policy validation become coupled.
- Backtracking becomes harder to reason about when a parser first succeeds syntactically and then rejects semantically.

Recommendation:

- Parse the syntactic form first.
- Validate policy constraints in a separate pass, or at least in an explicit post-parse validation step.
- Keep parser failure reserved for "this token stream is not this grammar form", not "this parsed form is not allowed under current language rules".

## Lower-Priority but Worth Doing

### 9. Normalize brace and semicolon behavior across grammar families

Brace-delimited parsers are currently inconsistent:

- some allow empty bodies
- some allow leading/trailing semicolons
- some require at least one item

Examples include:

- `moduleBodyParser`
- `bracedStmtListParser`
- `classItemsBracedParser`
- `instanceItemsBracedParser`
- `bracedDeclsParser`
- lambda-case braced alternatives

Some of this may be intentional, but right now the behavior is encoded piecemeal rather than by policy.

Recommendation:

- Define a small family of reusable helpers for:
  - empty vs non-empty bodies
  - leading semicolon allowance
  - trailing semicolon allowance
- Use those helpers consistently so differences are explicit.

### 10. Reduce repeated literal parser boilerplate

Expressions and patterns both repeat similar literal parsing logic, and the parser already depends on token spans and token text to preserve representation.

Recommendation:

- Extract token-to-literal helpers and token-to-expression helpers where practical.
- This is not the highest-risk area, but it is an easy way to lower future edit cost.

## Testing Recommendations

To keep the parser from accruing silent debt, I would add targeted regression tests in addition to the existing round-trip/property coverage:

1. Unsupported declaration forms fail clearly and do not parse as `DeclValue`.
2. Declaration-prefix ambiguities around `type`, `foreign`, `class`, `instance`, `default`, `infix`, `infixl`, and `infixr`.
3. Newline-sensitive application for expressions, patterns, and types.
4. Braced vs layout-sensitive forms for `where`, `let`, `do`, `case`, class bodies, and instance bodies.
5. Error-message snapshots for a representative set of malformed inputs.
6. Shared grammar invariants, such as constraint parsing, enforced through one common test matrix if the duplicated parsers are merged.

## Suggested Implementation Order

If this were being cleaned up incrementally, I would do it in this order:

1. Make unsupported declaration forms fail explicitly.
2. Fix the `ParserConfig` API drift.
3. Introduce shared helpers for duplicated grammar fragments.
4. Improve token labeling and parser diagnostics.
5. Refactor broad `try` chains into prefix-driven dispatch.
6. Consolidate span access behind a shared abstraction.
7. Tighten syntactic categories for names and namespaces.

That sequence reduces the highest-risk ambiguity first, then pays down the structural debt that would otherwise make future grammar work slower and more fragile.
