# Test runner for aihc project
# See https://just.systems for Just documentation

# Run all tests with hidden successes (1000 Hedgehog tests per property)
test:
  TASTY_HEDGEHOG_TESTS=1000 TASTY_HEDGEHOG_SHRINKS=10000 cabal test -v0 all --test-options='--hide-successes'

# Replay a specific Hedgehog test case
# Usage: just replay "<replay-string>"
replay ARGUMENT:
  cabal test all --jobs=1 -v0 --test-options='--pattern properties --hedgehog-replay="{{ARGUMENT}}" --hide-successes'

# Run Hedgehog with 10000 tests in a loop until failure
hedgehog:
  while true; do just hedgehog1 || break; done

hedgehog1:
  TASTY_HEDGEHOG_TESTS=10000 TASTY_HEDGEHOG_SHRINKS=1000000 cabal test all -v0 --jobs=1 --test-options="--hide-successes"

# Build the web kit for the playgrounds of The Lazy Reader and serve the guide at http://127.0.0.1:8000
guide:
  #!/usr/bin/env bash
  set -euo pipefail
  nix develop --quiet --command bash -c 'cabal build -v0 exe:aihc exe:aihc-dev && scripts/build-web-kit.sh --output docs/aihc-guide/src/play/kit --store .aihc-target/lazy-reader-store && cd docs/aihc-guide && mkdocs serve'

# Auto-format Nix, Cabal, and C files (Nix and Cabal files come from scripts/repository-files.sh, so other worktrees and the benchmarks submodule are excluded)
fmt:
  nix develop --quiet --command bash -c 'set -o pipefail; scripts/repository-files.sh "*.nix" | xargs -0 -r alejandra || exit 1; scripts/repository-files.sh "*.cabal" | xargs -0 -r -n 1 cabal-gild --mode format --io || exit 1; find components tooling bin core-libs scripts test -type f \( -name "*.c" -o -name "*.h" \) -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*" -not -path "*/generated/*" -print0 | xargs -0 -r clang-format -i'

# Run full CI check: format, C lint, then tests (warnings are errors only here, not in plain `cabal` / `just test`)
# The manual is not part of this check, because it builds the compiler a second time in Nix. CI builds it as the `docs` check.
check:
  nix build .#aihc-grammars --no-link
  nix build .#vscode-lir --no-link
  nix develop --quiet --command bash -c 'set -o pipefail; scripts/repository-files.sh "*.cabal" | xargs -0 -r -n 1 cabal-gild --mode check --input'
  nix develop --quiet --command bash -c 'find components tooling bin core-libs scripts test -type f \( -name "*.c" -o -name "*.h" \) -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*" -not -path "*/generated/*" -print0 | xargs -0 -r clang-format --dry-run --Werror'
  nix develop --quiet --command scripts/clang-tidy-all.sh
  nix develop --quiet --command scripts/update-wit-bindings.sh --check
  TASTY_HEDGEHOG_TESTS=1000 cabal test -v0 all --ghc-options=-Werror --test-options='--hide-successes'

# Preview the manual at http://127.0.0.1:8000/.
docs: docs-examples
  AIHC_HIGHLIGHT="$(nix build --no-link --print-out-paths .#aihc-grammars)/bin/aihc-highlight" nix develop --quiet --command bash -c 'cd docs/aihc-manual && mkdocs serve'

# Compile the pipeline examples of the manual with the local compiler.
docs-examples:
  cabal run -v0 aihc-dev -- pipeline-examples docs/aihc-manual/pipeline-examples --output docs/aihc-manual/generated

# Regenerate the committed Unicode tables from pinned UCD inputs.
generate-unicode:
  nix run .#generate-unicode

# Verify that the committed Unicode tables match the generator and pinned UCD.
check-unicode:
  nix run .#check-unicode
