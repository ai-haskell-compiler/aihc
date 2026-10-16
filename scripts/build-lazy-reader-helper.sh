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
# AIHC names the compiler and AIHC_DEV the developer tool. The defaults
# are the cabal builds of exe:aihc and exe:aihc-dev. The output directory
# gets aihc-play.js, aihc-play-component.core.wasm, and core.bundle.gz: the naming
# and type facts of the core libraries, which the helper loads instead of
# compiling them. The bundle comes from the wasm32-wasip3 entries of the
# store, which the helper build installs anyway.
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
aihc_dev="${AIHC_DEV:-$(cd "$root" && cabal list-bin aihc:exe:aihc-dev)}"
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
# These source modules use the GHC2021 edition and OverloadedStrings in Cabal.
node --input-type=module - "$root" "$work/source" <<'JS'
import { readdir, readFile, writeFile, mkdir } from "node:fs/promises";
import { join, dirname } from "node:path";
const [root, output] = process.argv.slice(2);
async function copy(directory, relative = "") {
  for (const entry of await readdir(join(directory, relative), { withFileTypes: true })) {
    const name = join(relative, entry.name);
    if (entry.isDirectory()) await copy(directory, name);
    else if (name.endsWith(".hs")) {
      const target = join(output, name);
      await mkdir(dirname(target), { recursive: true });
      const source = await readFile(join(directory, name), "utf8");
      await writeFile(target, "{-# LANGUAGE GHC2021, OverloadedStrings #-}\n" + source);
    }
  }
}
for (const directory of ["bin/aihc-play/app", "bin/aihc-play/src", "bin/aihc/src", "bin/aihc/prim-wiring", ...["fc", "grin", "lir", "native", "wasm"].map((name) => "bin/aihc/compiler/" + name + "/src")]) {
  await copy(join(root, directory));
}
JS
"$aihc" build "$work/source/Main.hs" \
  -i "$work/source" \
  -p aihc-parser -p aihc-resolve -p aihc-tc \
  -p array -p base -p binary -p bytestring -p containers -p directory -p filepath -p text -p transformers \
  -p megaparsec -p prettyprinter -p primitive -p process -p vector \
  --workspace "$work/workspace" \
  --target wasm32-wasip3 \
  --store "$store" \
  --build-root "$work/build" \
  -O "$optimization" \
  --output "$work/aihc-play.wasm"

echo "=== Writing the bundle of the core libraries ==="
mkdir -p "$output"
"$aihc_dev" play-bundle --store "$store/wasm32-wasip3" --output "$work/core.bundle"
gzip -9 -n -c "$work/core.bundle" > "$output/core.bundle.gz"

echo "=== Writing the browser linker inputs ==="
"$aihc_dev" play-runtime --store "$store/wasm32-wasip3" --output "$work/runtime"
echo '(module)' > "$work/empty.wat"
wasm-tools component embed "$root/bin/aihc/compiler/wasm/runtime/wit" "$work/empty.wat" -o "$work/runtime/world.wasm"
npm ci --prefix "$root/docs/aihc-guide"
node "$root/docs/aihc-guide/build-play.mjs" "$output" "$work/runtime"

if [[ -z "$jco" ]]; then
  # The pinned jco release. npx downloads it when it is not installed.
  jco="npx --yes @bytecodealliance/jco@1.38.0"
fi

echo "=== Transpiling the component with jco ==="
mkdir -p "$output"
$jco transpile "$work/aihc-play.wasm" \
  --out-dir "$work/out" \
  --name aihc-play-component \
  --instantiation async \
  --async-mode jspi \
  --no-nodejs-compat \
  --quiet
# wasm-opt makes the core module a third of its size. The build goes on
# without it when it is not installed.
if command -v wasm-opt >/dev/null; then
  echo "=== Shrinking the core module with wasm-opt ==="
  wasm-opt -Oz --all-features -o "$work/out/aihc-play-component.core.opt.wasm" "$work/out/aihc-play-component.core.wasm"
  mv "$work/out/aihc-play-component.core.opt.wasm" "$work/out/aihc-play-component.core.wasm"
else
  echo "wasm-opt is not on the PATH; the core module stays unoptimized" >&2
fi
cp "$work/out/aihc-play-component.js" "$output/"
for core in "$work/out/"*.wasm; do
  gzip -9 -n -c "$core" > "$output/$(basename "$core").gz"
done
ls -la "$output"
# The component itself goes beside the output directory, for a check under
# wasmtime: wasmtime run -S cli -S http aihc-play.wasm < requests. It is
# not part of the site: Workers rejects an asset above 25 MiB.
cp "$work/aihc-play.wasm" "$(dirname "$output")/aihc-play.wasm"
