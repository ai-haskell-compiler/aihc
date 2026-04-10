# Test runner for aihc project
# See https://just.systems for Just documentation

# Run all tests with hidden successes
test:
  cabal test -v0 all --test-options='--hide-successes' --test-show-details=failures

# Replay a specific QuickCheck test case
# Usage: just replay "<replay-string>"
replay ARGUMENT:
  cabal test aihc-parser:spec -v0 --test-options='--pattern properties --quickcheck-replay="{{ARGUMENT}}" --hide-successes'

# Run QuickCheck with 10000 tests in a loop until failure
qc:
  while true; do cabal test aihc-parser:spec -v0 --test-options="--pattern properties --quickcheck-tests 10000"; done

# Run full CI check: format, lint, then tests
check:
  nix develop --command bash -c 'ormolu --mode check $(find components -name "*.hs" -not -path "*/test/Test/Fixtures/*")'
  nix develop --command bash -c 'hlint $(find components -name "*.hs" -not -path "*/test/Test/Fixtures/*")'
  cabal test -v0 all --test-options='--hide-successes'
