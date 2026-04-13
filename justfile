# Test runner for aihc project
# See https://just.systems for Just documentation

# Run all tests with hidden successes (1000 QuickCheck tests per property)
test:
  cabal test -v0 all --test-options='--hide-successes --quickcheck-tests 1000' --test-show-details=failures

# Replay a specific QuickCheck test case
# Usage: just replay "<replay-string>"
replay ARGUMENT:
  cabal test aihc-parser:spec -v0 --test-options='--pattern properties --quickcheck-replay="{{ARGUMENT}}" --hide-successes'

# Run QuickCheck with 10000 tests in a loop until failure
qc:
  while true; do cabal test aihc-parser:spec -v0 --test-options="--pattern properties --quickcheck-tests 10000" || break; done

# Auto-format all Haskell files (excludes dist-newstyle and test fixtures)
fmt:
  nix develop --quiet --command bash -c 'ormolu --mode inplace $(find components -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'

# Run full CI check: format, lint, then tests
check:
  nix develop --quiet --command bash -c 'ormolu --mode check $(find components -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'
  nix develop --quiet --command bash -c 'hlint $(find components -name "*.hs" -not -path "*/dist-newstyle/*" -not -path "*/test/Test/Fixtures/*")'
  just test
