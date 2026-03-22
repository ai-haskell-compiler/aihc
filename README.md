# AI-written Haskell Compiler (aihc)

## Docs

- [Haskell 2010 Language Report (Markdown)](docs/haskell2010-language-report.md)
- [Layout-sensitive parsing](docs/hse-indentation-layout.md)

## Component Progress

| Name | Progress |
| --- | --- |
| Parser Tests | <!-- AUTO-GENERATED: START parser-progress --> `325/451` (`72.06%`) <!-- AUTO-GENERATED: END parser-progress --> |
| Lexer Tests | <!-- AUTO-GENERATED: START lexer-progress --> `12/15` (`80.00%`) <!-- AUTO-GENERATED: END lexer-progress --> |
| Parser Stackage | <!-- AUTO-GENERATED: START parser-stackage-progress --> `369/3390` (`10.88%`) <!-- AUTO-GENERATED: END parser-stackage-progress --> |
| CPP preprocessor | <!-- AUTO-GENERATED: START cpp-progress --> `37/37` (`100.00%`) <!-- AUTO-GENERATED: END cpp-progress --> |
| Name resolution | <!-- AUTO-GENERATED: START name-resolution-progress --> `10/12` (`83.33%`) <!-- AUTO-GENERATED: END name-resolution-progress --> |

## Ways to contribute

- PRs accepted and bugs are welcomed.
- If you have spare inference, run this command to generate a prompt: `nix run .#prompt`
- If you have spare compute, run this command to fuzz test aihc: `nix run .#parser-fuzz`


## Nix Commands

Run the full test suite:

```bash
nix flake check
```

### Apps

| Command | Description |
| --- | --- |
| `nix run .#parser-test` | Run parser test suite |
| `nix run .#cpp-test` | Run CPP preprocessor test suite |
| `nix run .#name-resolution-test` | Run name resolution test suite |
| `nix run .#parser-progress` | Show parser oracle test progress |
| `nix run .#lexer-progress` | Show lexer oracle test progress |
| `nix run .#parser-extension-progress` | Show parser extension test progress |
| `nix run .#cpp-progress` | Show CPP preprocessor test progress |
| `nix run .#name-resolution-progress` | Show name resolution test progress |
| `nix run .#stackage-progress` | Show parser progress on Stackage packages |
| `nix run .#prompt` | Generate a prompt for contributing |
| `nix run .#hackage-tester` | Test parser against Hackage packages |
| `nix run .#parser-fuzz` | Fuzz test the parser |
| `nix run .#parser-quickcheck-batch` | Run QuickCheck property tests |
| `nix run .#parser-quickcheck-soak` | Run QuickCheck soak tests |
| `nix run .#line-counts` | Show line counts per component |
| `nix run .#generate-reports` | Update generated README content |
| `nix run .#check-reports` | Check if generated content is up-to-date |

Strict variants (fail on unexpected results): `parser-progress-strict`, `lexer-progress-strict`, `parser-extension-progress-strict`, `cpp-progress-strict`, `name-resolution-progress-strict`.

