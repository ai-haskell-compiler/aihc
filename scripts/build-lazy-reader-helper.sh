#!/usr/bin/env bash
# Build the playground helper of The Lazy Reader for the browser.
#
# The helper is bin/aihc-play, compiled by aihc to wasm32-wasip3. jco turns
# the component into a JavaScript module that imports the WASI interfaces
# from the browser host of the guide, docs/aihc-guide/src/play/host.js.
#
# Usage: scripts/build-lazy-reader-helper.sh --output DIR [--store DIR]
#        [--optimization LEVEL] [--jco PATH]
#
# AIHC names the compiler. The default is the cabal build of exe:aihc.
# The output directory gets aihc-play.js and aihc-play.core.wasm.
set -euo pipefail

output=""
store=""
optimization="s"
jco="${JCO:-}"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) output=$2; shift 2 ;;
    --store) store=$2; shift 2 ;;
    --optimization) optimization=$2; shift 2 ;;
    --jco) jco=$2; shift 2 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done
if [[ -z "$output" ]]; then
  echo "usage: $0 --output DIR [--store DIR] [--optimization LEVEL] [--jco PATH]" >&2
  exit 2
fi

root=$(cd "$(dirname "$0")/.." && pwd)
aihc="${AIHC:-$(cd "$root" && cabal list-bin aihc:exe:aihc)}"
# The wasm link reads the WIT world and the runtime sources from the data
# directory of the compiler. A cabal build has them in the repository.
export aihc_datadir="${aihc_datadir:-$root/bin/aihc}"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
if [[ -z "$store" ]]; then
  store="$work/store"
fi

# The workspace gives the local components to the dependency solver.
mkdir -p "$work/workspace"
ln -s "$root/components/aihc-tc" "$work/workspace/aihc-tc"
ln -s "$root/components/aihc-resolve" "$work/workspace/aihc-resolve"

echo "=== Compiling aihc-play to wasm32-wasip3 at -O$optimization ==="
# aihc finds the core libraries from the repository root, and it writes
# the lock of its plan there too. The lock of the repository is the plan of
# aihc itself, so the script puts it back afterwards.
cd "$root"
if [[ -f aihc.lock ]]; then
  cp aihc.lock "$work/aihc.lock.before"
  trap 'cp "$work/aihc.lock.before" "$root/aihc.lock"; rm -rf "$work"' EXIT
fi
"$aihc" build "$root/bin/aihc-play/app/Main.hs" \
  -i "$root/bin/aihc-play/app" \
  -i "$root/bin/aihc-play/src" \
  -i "$root/bin/aihc/prim-wiring" \
  -p aihc-parser -p aihc-resolve -p aihc-tc \
  -p base -p bytestring -p containers -p filepath -p text \
  --workspace "$work/workspace" \
  --target wasm32-wasip3 \
  --store "$store" \
  --build-root "$work/build" \
  -O "$optimization" \
  --output "$work/aihc-play.wasm"

if [[ -z "$jco" ]]; then
  # The pinned jco release. npx downloads it when it is not installed.
  jco="npx --yes @bytecodealliance/jco@1.38.0"
fi

echo "=== Transpiling the component with jco ==="
mkdir -p "$output"
# Each WASI interface maps to one export of the browser host.
$jco transpile "$work/aihc-play.wasm" \
  --out-dir "$work/out" \
  --name aihc-play \
  --async-mode jspi \
  --no-nodejs-compat \
  --quiet \
  -M 'wasi:cli/environment@0.3.0=../host.js#environment' \
  -M 'wasi:cli/types@0.3.0=../host.js#types' \
  -M 'wasi:cli/stdin@0.3.0=../host.js#stdin' \
  -M 'wasi:cli/stdout@0.3.0=../host.js#stdout' \
  -M 'wasi:cli/stderr@0.3.0=../host.js#stderr' \
  -M 'wasi:cli/terminal-input@0.3.0=../host.js#terminalInput' \
  -M 'wasi:cli/terminal-output@0.3.0=../host.js#terminalOutput' \
  -M 'wasi:cli/terminal-stdin@0.3.0=../host.js#terminalStdin' \
  -M 'wasi:cli/terminal-stdout@0.3.0=../host.js#terminalStdout' \
  -M 'wasi:cli/terminal-stderr@0.3.0=../host.js#terminalStderr' \
  -M 'wasi:clocks/types@0.3.0=../host.js#clocksTypes' \
  -M 'wasi:clocks/monotonic-clock@0.3.0=../host.js#monotonicClock' \
  -M 'wasi:filesystem/types@0.3.0=../host.js#fsTypes' \
  -M 'wasi:filesystem/preopens@0.3.0=../host.js#preopens' \
  -M 'wasi:http/types@0.3.0=../host.js#httpTypes' \
  -M 'wasi:http/client@0.3.0=../host.js#client'
# wasm-opt makes the core module a third of its size. The build goes on
# without it when it is not installed.
if command -v wasm-opt >/dev/null; then
  echo "=== Shrinking the core module with wasm-opt ==="
  wasm-opt -Oz --all-features -o "$work/out/aihc-play.core.opt.wasm" "$work/out/aihc-play.core.wasm"
  mv "$work/out/aihc-play.core.opt.wasm" "$work/out/aihc-play.core.wasm"
else
  echo "wasm-opt is not on the PATH; the core module stays unoptimized" >&2
fi
cp "$work/out/aihc-play.js" "$work/out/aihc-play.core.wasm" "$output/"
ls -la "$output"
# The component itself goes beside the output directory, for a check under
# wasmtime: wasmtime run -S cli -S http aihc-play.wasm < requests. It is
# not part of the site: Workers rejects an asset above 25 MiB.
cp "$work/aihc-play.wasm" "$(dirname "$output")/aihc-play.wasm"
