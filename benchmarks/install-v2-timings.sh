#!/usr/bin/env bash

set -euo pipefail

repository_root=$(cd "$(dirname "$0")/.." && pwd)
store_root=$(mktemp -d "${TMPDIR:-/tmp}/aihc-install-v2-bench.XXXXXX")
trap 'rm -rf -- "$store_root"' EXIT

target=${1:-apple-arm64}

print_package_timings() {
  local name=$1
  local directory=$2
  echo "$name"
  cabal run -v0 exe:aihc -- install-v2 "$directory" --store "$store_root" --print-timings --target "$target" --reinstall
}

cd "$repository_root"
print_package_timings keep-grin bin/aihc/test/Test/Fixtures/install-v2/keep-grin
print_package_timings aihc-prim core-libs/aihc-prim
print_package_timings aihc-base core-libs/aihc-base
