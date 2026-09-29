# AIHC grammars

This directory has the TextMate grammars of the AIHC intermediate languages.
The VS Code extension in `editors/vscode-lir` and the AIHC Manual use them.

| Language | Grammar | Scope name | Reference |
| --- | --- | --- | --- |
| System FC | `syntaxes/fc.tmLanguage.json` | `source.aihc-fc` | `bin/aihc/compiler/fc/src/Aihc/Fc/Pretty.hs` |
| GRIN | `syntaxes/grin.tmLanguage.json` | `source.aihc-grin` | `bin/aihc/compiler/grin/src/Aihc/Grin/Pretty.hs` |
| LIR | `syntaxes/lir.tmLanguage.json` | `source.lir` | `docs/lir.md` and `bin/aihc/compiler/lir/src/Aihc/Lir/Parser.hs` |

The grammars use standard TextMate scopes, so existing themes can select colors.
They do not check types or report compiler errors.
The last rule of each grammar gives the scope `invalid.illegal.unrecognized` to each character that no other rule recognizes.

## Tests

`npm test` runs two checks.

- `test/grammar.mjs` checks the scopes of selected tokens.
  Each fixture in `test/fixtures/<language>` has a source file and a `.json` file with expected scopes.
  Line numbers start at one.
  Each assertion gives the source text and its most specific TextMate scope.
  An optional `parentScope` checks an outer scope, for example the string scope of a quoted name.
  For repeated text, set `occurrence` to select the required occurrence.
  Each file starts with a fresh tokenizer state.
- `test/corpus.mjs` tokenizes the compiler output in the repository.
  This output is the expected programs of the System FC and GRIN golden tests, and the Lir test and runtime sources.
  The check fails when a grammar does not recognize a character.
  Thus, a change to a printer must also change the grammar.

From the repository root, install the dependencies and run the tests:

```sh
nix shell --inputs-from . nixpkgs#nodejs --command npm --prefix editors/grammars ci
nix shell --inputs-from . nixpkgs#nodejs --command npm --prefix editors/grammars test
```

`nix build .#aihc-grammars`, `just check`, and `nix flake check` also run these tests.

## Highlighter

`highlight.mjs LANGUAGE` reads one program from standard input.
It writes a code block with the classes of the Pygments highlighter.
It fails when the grammar does not recognize a character.
The package `aihc-grammars` installs it as `aihc-highlight`.

The manual highlights each fence with the language `aihc-fc`, `aihc-grin`, or `aihc-lir` with this command.
See `docs/aihc-manual/hooks.py`.
