# Test runner for aihc project
# See https://just.systems for Just documentation

# Run all tests with hidden successes (1000 QuickCheck tests per property)
test:
  cabal test -v0 all --test-options='--hide-successes --quickcheck-tests 1000'

# Replay a specific QuickCheck test case
# Usage: just replay "<replay-string>"
replay ARGUMENT:
  cabal test aihc-parser:spec -v0 --test-options='--pattern properties --quickcheck-replay="{{ARGUMENT}}" --hide-successes'

# Run QuickCheck with 10000 tests in a loop until failure
qc:
  while true; do cabal test aihc-parser:spec -v0 --test-options="--pattern properties --quickcheck-tests 10000" || break; done

# Auto-format Nix files and Haskell files (excludes dist-newstyle, result, .git; Haskell excludes test fixtures)
fmt:
  nix develop --quiet --command bash -c 'find . -name "*.nix" -not -path "*/.git/*" -not -path "*/dist-newstyle/*" -not -path "*/result/*" -print0 | xargs -0 -r alejandra; ormolu --mode inplace $(find components -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'

# Apply HLint hints in place via apply-refact (HLint --refactor accepts one file at a time; same file set as fmt/check)
hlint-refactor:
  #!/usr/bin/env bash
  set -euo pipefail
  exec nix develop --quiet --command bash -c \
    'set -euo pipefail
     while IFS= read -r -d "" f; do
       hlint --refactor --refactor-options="--inplace" "$f"
     done < <(find components -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*" -print0)'

# Run full CI check: format, lint, then tests (warnings are errors only here, not in plain `cabal` / `just test`)
check:
  nix develop --quiet --command bash -c 'ormolu --mode check $(find components -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'
  nix develop --quiet --command bash -c 'hlint $(find components -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'
  cabal test -v0 all --ghc-options=-Werror --test-options='--hide-successes --quickcheck-tests 1000'
