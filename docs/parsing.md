# Parsing Guide

This document is for coding agents working on `aihc-parser`. Its purpose is to
make parser changes simpler, safer, and easier to review.

The priorities are:

1. Correctness
2. Readable code
3. Performance

Prefer simple, obviously correct parsers unless there is a demonstrated
pathological case. When performance work is necessary, optimize by reducing
ambiguity and backtracking, not by adding clever stateful shortcuts.

Applied to parser work, that means:

- Preserve semantics before chasing speed.
- Prefer local, explicit dispatch over clever global machinery.
- Use `checkPattern` and shared helpers to delete duplicated grammar.
- Only optimize performance after identifying a real ambiguity or benchmarked pathological case.
- When performance matters, reduce ambiguity and backtracking first.

Do not optimize by:

- hiding ambiguity behind more parser state
- adding ad hoc flags that only make sense for one call site
- keeping two large grammars in sync when one parse-plus-reclassify path would do

## Architecture Overview

At a high level, parsing is:

1. Lex source text into `LexToken`s in [Aihc.Parser.Lex](components/aihc-parser/src/Aihc/Parser/Lex.hs).
2. Wrap those tokens in `TokStream` in [Aihc.Parser.Types](components/aihc-parser/src/Aihc/Parser/Types.hs).
3. Run Megaparsec parsers over `TokStream` from [Aihc.Parser](components/aihc-parser/src/Aihc/Parser.hs).
4. Build syntax trees from the internal parser modules.

Important consequences:

- Lexing is lazy and shared. Backtracking does not rescan characters from the source file.
- Parsing work is still repeated when `MP.try` rewinds after consuming a long token prefix.
- Layout is part of the token stream machinery, so parser structure affects both correctness and diagnostics.

### `TokStream`

`TokStream` is the key performance boundary.

- `tokStreamRawTokens` is a shared lazy list of already-scanned raw tokens.
- `tokStreamLayoutState` overlays virtual layout tokens (`{`, `;`, `}`).
- `tokStreamPendingPragmas` keeps hidden pragmas out of the ordinary token stream.

This means backtracking is cheaper than rescanning source text, but not free:
the parser still reconsumes the same token prefix and rebuilds the same partial
results.

### `Common.hs`

[Common.hs](components/aihc-parser/src/Aihc/Parser/Internal/Common.hs) is the shared parser toolbox. It provides:

- `TokParser`
- token primitives like `expectedTok` and `tokenSatisfy`
- source span helpers like `withSpan` and `withSpanAnn`
- diagnostic helpers like `label` and `region`
- small lookahead probes like `startsWithTypeSig`, `startsWithAsPattern`, and `startsWithContextType`
- shared combinators like `functionHeadParserWith`, `layoutSepBy1`, and `foldInfixR`

If a simplification can be expressed as a reusable parser helper, it usually
belongs here.

### `Expr.hs`

[Expr.hs](components/aihc-parser/src/Aihc/Parser/Internal/Expr.hs) is the largest grammar surface.

- `exprParser` is the public expression entry point.
- It dispatches early on obvious leading tokens such as `do`, `if`, `let`, `proc`, and `\`.
- It handles application, infix chaining, record suffixes, parens, tuple syntax, and many extension forms.
- Several statement-level parsers already use a good pattern: parse once, then decide whether the result is an expression or a pattern bind.

Notable example:

- `exprOrPatternBindParser` parses an expression once, then checks for `<-`, and only then converts with `checkPattern`.

That pattern is usually preferable to `MP.try patternParser <|> exprParser`.

### `Pattern.hs`

[Pattern.hs](components/aihc-parser/src/Aihc/Parser/Internal/Pattern.hs) handles:

- atomic patterns
- constructor application
- infix patterns
- tuples, lists, records, and view patterns
- pattern-only syntax such as `~`, `!`, `@`, wildcard patterns, and type binders

This file is a common source of ambiguity because patterns and expressions share
so much surface syntax.

### `Decl.hs`

[Decl.hs](components/aihc-parser/src/Aihc/Parser/Internal/Decl.hs) performs early dispatch for declarations.

Good current behavior:

- It uses `lookAhead` on one or two tokens to separate obvious declaration forms.
- It recognizes when a leading `TkVarId` or `TkKeywordType` needs more specific dispatch.

Risk area:

- fallback chains such as pattern bind vs value declaration vs expression splice still rely on `MP.try` around large alternatives.

### `CheckPattern.hs`

[CheckPattern.hs](components/aihc-parser/src/Aihc/Parser/Internal/CheckPattern.hs) is the most important simplification tool in the parser.

It implements "parse as expression, then reclassify as pattern":

- shared syntax is parsed once by the expression parser
- `checkPattern` converts valid `Expr` trees into `Pattern` trees
- expression-only constructs are rejected with explicit messages

This is generally the right way to remove pattern/expression ambiguity. Prefer
extending this approach over reintroducing parallel expression and pattern
grammars for shared syntax.

## How The Parts Fit Together

If you are changing ambiguous syntax, look for a way to:

- dispatch on one token kind early
- parse the shared prefix once
- branch only after the disambiguating token appears
- reuse `checkPattern` instead of reparsing the same prefix as both pattern and expression

## `MP.try` And O(n^2) Behavior

`MP.try` is necessary sometimes, but broad `try` around long alternatives is one
of the main ways this parser becomes slow and hard to reason about.

Because `TokStream` shares raw tokens, `MP.try` does not cause O(n^2) lexing.
The cost comes from reparsing: the parser walks the same token prefix again and
rebuilds the same intermediate structures. Nested or chained alternatives
multiply that repeated work.

So the cost model is:

- one failed `MP.try` after consuming `k` tokens costs O(k)
- doing that repeatedly at many positions costs O(n^2)

### A concrete shape

This is the classic pattern:

```haskell
parseThing =
  MP.try parsePatternLikeThing
    <|> parseExprLikeThing
```

Now imagine input where each level looks pattern-like until the last token:

```haskell
f (((((((x))))))) = rhs
```

or:

```haskell
x1 x2 x3 x4 x5 ... xn
```

If the first branch keeps parsing deeper before eventually failing, and the
second branch then reparses the same prefix, each level adds another large
prefix replay. That is how you get O(n^2) behavior from a parser that "only"
backtracks.

### Where this repo is vulnerable

The current hotspots are exactly the places where alternatives share a long
prefix:

- declaration fallback chains in [Decl.hs](components/aihc-parser/src/Aihc/Parser/Internal/Decl.hs)
- expression vs pattern bind decisions in [Expr.hs](components/aihc-parser/src/Aihc/Parser/Internal/Expr.hs)
- parenthesized and tuple-like pattern parsing in [Pattern.hs](components/aihc-parser/src/Aihc/Parser/Internal/Pattern.hs)
- context/type head ambiguity helpers in [Common.hs](components/aihc-parser/src/Aihc/Parser/Internal/Common.hs)

Examples of better current practice already in the tree:

- `startsWithTypeSig` in `Common.hs`: O(1)-ish token probe instead of trying to parse a full signature and rewinding.
- `exprOrPatternBindParser` in `Expr.hs`: parse once, then branch on `<-`.
- targeted `lookAhead` dispatch in `Decl.hs`: inspect a small fixed prefix before committing.

### How to fix it

When you see a broad `MP.try`, ask:

1. Can the branches be separated by the next token kind?
2. Can I parse the shared prefix once, then inspect one delimiter?
3. Can a focused `lookAhead` answer the question without building AST?
4. Can `checkPattern` remove the need for a second parser entirely?

The preferred fix is almost always shared-prefix parse first, branch second —
not: parse everything one way, fail late, rewind, parse everything another way.

### How much lookahead is acceptable

Preferred:

- O(1) token-kind lookahead
- small fixed-width lookahead such as one or two tokens

Acceptable when necessary:

- linear scanning lookahead that only inspects token kinds and nesting depth

Avoid unless there is a demonstrated need:

- reparsing a full subtree under `MP.try`
- stateful shortcuts that guess and later repair parser state

`startsWithContextType` is a good example of the acceptable middle ground: it
scans token kinds at top-level nesting depth without building types or rewinding
large parser branches.

## Error Messages

Descriptive errors are a correctness feature, not a polish pass.

The current parser already has the right building blocks:

- `label` rewrites failures into `UnexpectedTokenExpecting`
- `region` adds context lines such as `while parsing then branch`
- `MP.customFailure` lets a parser report a precise expected form at the exact token
- `mkFoundToken` preserves the offending token text, kind, origin, and span

### Rules for good parser diagnostics

Point at the token that actually caused the failure.

- Prefer `lookAhead anySingle` plus `mkFoundToken` when constructing a custom error.
- Do not consume extra tokens and then fail later with a vague message.
- If the parser can tell the user what delimiter or keyword was expected, say that directly.

Add local context without burying the root cause.

- Wrap subparsers in `region` when the parent construct matters.
- Use short context strings like `while parsing case alternative`.
- Keep the final "expecting ..." line specific.

Use `label` for user-facing grammar names.

- Good: `label "pattern"`, `label "expression"`, `label "right-hand side"`
- Bad: exposing internal helper names or generic parser-combinator failures

Use `MP.customFailure` when the parser knows more than Megaparsec does.

Examples already in the tree:

- declaration head errors in `Decl.hs`
- operator-token validation in `Pattern.hs`

### Practical guidance

If you are splitting a parser into:

```haskell
shared <- parseSharedPrefix
case next token of
  ...
```

that often improves diagnostics as well as performance, because the parser
fails exactly at the disambiguating token instead of after speculative work.

If a construct is rejected by `checkPattern`, make the rejection message name
the illegal expression form:

- `unexpected if-then-else in pattern`
- `unexpected do expression in pattern`
- `unexpected variable operator '+' in pattern`

Those messages are much better than a generic parse failure after a backtrack.

### How error messages are tested

Error message golden tests live under:

- [error-messages fixtures](components/aihc-parser/test/Test/Fixtures/error-messages)

The harness is:

- [ParserErrorGolden.hs](components/aihc-parser/common/ParserErrorGolden.hs)
- [Test.ErrorMessages.Suite](components/aihc-parser/test/Test/ErrorMessages/Suite.hs)

Each fixture records:

- `src`
- expected `ghc` message
- expected `aihc` message

If you intentionally improve diagnostics, update the relevant fixtures.

## Testing

Parser simplifications are not done until they are covered by the right test
layer. A change that affects both the accepted syntax surface and a specific AST
shape may need both oracle and golden coverage. A change that touches a whole
class of syntax should also have a QuickCheck property.

### Golden tests

Golden parser fixtures live under:

- [golden fixtures](components/aihc-parser/test/Test/Fixtures/golden)

The harness is:

- [ParserGolden.hs](components/aihc-parser/common/ParserGolden.hs)
- [Test.Parser.Suite](components/aihc-parser/test/Test/Parser/Suite.hs)

Golden tests are for direct parser behavior:

- parse success/failure
- exact shorthand AST on success
- expected `pass`, `fail`, or `xfail`

Rules:

- `pass` requires an `ast`
- `fail` expects rejection
- `xfail` marks a known bug and requires a reason
- `xpass` is not allowed in fixtures; an `xfail` that now passes is reported by the harness

Use golden tests when:

- you are changing the concrete parser behavior directly
- you want a small fixed regression case
- you are pinning a specific AST shape

### Oracle tests

Oracle fixtures live under:

- [oracle fixtures](components/aihc-parser/test/Test/Fixtures/oracle)

The harness is:

- [ExtensionSupport.hs](components/aihc-parser/common/ExtensionSupport.hs)
- [GhcOracle.hs](components/aihc-parser/common/GhcOracle.hs)
- [Test.Oracle.Suite](components/aihc-parser/test/Test/Oracle/Suite.hs)

Oracle fixtures are module `.hs` files with an `ORACLE_TEST` block:

- `pass`
- `xfail <reason>`

Important: oracle `pass` means more than "GHC accepts the file".

For a passing oracle case:

- GHC must accept the module
- `aihc-parser` must also validate against the oracle-based roundtrip/fingerprint check

For `xfail`:

- the known discrepancy must still be present
- if it disappears, the suite reports an unexpected pass

Use oracle tests when:

- the question is "should we match GHC here?"
- ambiguity crosses multiple parser subsystems
- a change could alter accepted syntax without obviously changing a small golden AST

### QuickCheck tests

QuickCheck property tests are assembled in:

- [Spec.hs](components/aihc-parser/test/Spec.hs)

Representative properties live in:

- [ExprRoundTrip.hs](components/aihc-parser/test/Test/Properties/ExprRoundTrip.hs)
- [PatternRoundTrip.hs](components/aihc-parser/test/Test/Properties/PatternRoundTrip.hs)

These mainly verify pretty-print/parse roundtrips over generated ASTs.

Use QuickCheck when:

- a simplification affects a whole class of syntax, not one fixture
- the risk is "this refactor breaks an invariant in a broad way"
- you want coverage for nested or generated shapes that are tedious to enumerate manually

Useful commands:

- `cabal test -v0 aihc-parser:spec --test-options="--pattern properties"`
- `cabal test aihc-parser:spec -v0 --test-options="--pattern properties --quickcheck-tests 10000"`
- `just qc`
- `just replay "<seed>"`

### Performance tests

The dedicated performance harness is:

- [Test.Performance.Suite](components/aihc-parser/test/Test/Performance/Suite.hs)

Performance fixtures live under:

- [performance fixtures](components/aihc-parser/test/Test/Fixtures/performance/module)

This suite exists for demonstrated pathological cases. It currently enforces
"parse under 1s" for fixture and generated stress cases.

Use a performance regression test when:

- you have evidence of pathological backtracking
- the change is specifically about asymptotic behavior
- a normal golden/oracle test would not catch the performance bug

### What to run

Normal local validation:

- `just fmt` — runs `alejandra` on Nix files and `ormolu --mode inplace` on Haskell sources (excluding fixtures and `dist-newstyle`)
- `just check` — runs `ormolu --mode check` and `hlint` on the same file set, then `cabal test all --ghc-options=-Werror`

Focused parser work:

- `cabal test -v0 aihc-parser:spec --test-options="--pattern parser-golden"`
- `cabal test -v0 aihc-parser:spec --test-options="--pattern oracle"`
- `cabal test -v0 aihc-parser:spec --test-options="--pattern error-messages"`
- `cabal test -v0 aihc-parser:spec --test-options="--pattern performance"`

For commits, always run `just fmt` then `just check`.

## Good Patterns

These are the patterns to prefer when simplifying parser code.

### 1. Generalize, then extract common code

If two parsers share real structure, do not keep two near-copies alive.

Prefer:

- one helper with the varying parser passed in
- one parse path plus a reclassification pass

Examples already in the tree:

- `patternParserWithTypeSigParser`
- `exprParserWithTypeSigParser`
- `exprOrPatternBindParser`

### 2. Replace broad `MP.try` with focused lookahead

Prefer:

- token-kind dispatch with `lookAhead anySingle`
- fixed-width probes on one or two tokens
- small scanning lookahead that only tracks nesting depth

Examples:

- `startsWithTypeSig`
- `startsWithAsPattern`
- `startsWithContextType`
- `Decl.ordinaryDeclParser` dispatch on the first two tokens

The principle is:

- ask the smallest question that decides the branch
- do not speculate with a full parse if a token probe can answer it

### 3. Parse the shared prefix before branching

This is the main anti-backtracking technique.

Prefer:

```haskell
shared <- parseShared
next <- MP.optional (expectedTok delimiter)
case next of
  ...
```

over:

```haskell
MP.try parseWholeBranchA <|> parseWholeBranchB
```

Use this when two alternatives differ only at:

- `::`
- `=`
- `<-`
- `->`
- a closing delimiter

### 4. Let `checkPattern` do the pattern/expression disambiguation

If both grammars accept the same prefix, parse once as expression and convert.

This is usually better than:

- a parallel pattern parser
- a parallel expression parser
- a large speculative `try`

### 5. Keep diagnostics attached to the disambiguating token

When refactoring for simplicity or performance:

- preserve `label`
- preserve `region`
- prefer explicit `customFailure` when the grammar knows what token was wrong

A performance fix that makes diagnostics vague is usually not a good fix.

## Red Flags

Stop and reconsider if a change introduces:

- a new broad `MP.try` around a parser that can consume an arbitrarily long prefix
- duplicated expression and pattern grammar for the same surface syntax
- parser state flags used only to rescue one ambiguous construct
- a performance "fix" that depends on subtle invariants and is hard to explain
- worse source locations or less specific errors

## Preferred Review Standard

The standard for parser changes is not "tests are green".

A good parser simplification should be:

- obviously correct from the code structure
- smaller or clearer than what it replaces
- backed by the right regression tests
- explicit about ambiguity boundaries
- precise about diagnostics

If a change is faster but harder to reason about, it is probably not done yet.
If a change is simpler but introduces a known pathological backtracking case, it
is also not done yet. The target is parser code that is easy to trust.

## Related Documents

- [docs/unified-expr-pattern-parsing.md](docs/unified-expr-pattern-parsing.md) for the deeper design rationale behind expression-pattern unification
- [AGENTS.md](AGENTS.md) for repository workflow and required validation commands
