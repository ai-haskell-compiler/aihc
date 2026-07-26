#!/usr/bin/env bash

set -euo pipefail

repository_root=$(cd "$(dirname "$0")/.." && pwd)
iterations=${1:-5}
benchmark_root=$(mktemp -d "${TMPDIR:-/tmp}/aihc-integer-bench.XXXXXX")
trap 'rm -rf -- "$benchmark_root"' EXIT

for benchmark in integer-factorial integer-fibonacci; do
  source_file="$repository_root/benchmarks/$benchmark/Main.hs"
  ghc_output="$benchmark_root/$benchmark-ghc"
  ghc_build="$benchmark_root/$benchmark-ghc-build"
  aihc_output="$benchmark_root/$benchmark-aihc"

  mkdir -p "$ghc_build"
  ghc -O2 -fforce-recomp -outputdir "$ghc_build" "$source_file" -o "$ghc_output"
  cabal run -v0 exe:aihc -- compile "$source_file" -o "$aihc_output"

  echo "$benchmark"
  for compiler in ghc aihc; do
    executable="$benchmark_root/$benchmark-$compiler"
    echo "  $compiler"
    run=1
    while [[ "$run" -le "$iterations" ]]; do
      /usr/bin/time -p "$executable" >/dev/null
      run=$((run + 1))
    done
  done
done
