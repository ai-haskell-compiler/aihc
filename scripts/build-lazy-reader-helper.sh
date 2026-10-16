#!/usr/bin/env bash
# Build the playground helper of The Lazy Reader for the browser.
#
# The helper is bin/aihc-play, compiled by aihc to wasm32-wasip3. jco turns
# the component into a JavaScript module that imports the WASI interfaces
# from the browser host of the guide, docs/aihc-guide/src/play/host.js.
#
# Usage: scripts/build-lazy-reader-helper.sh --output DIR [--store DIR]
#        [--optimization LEVEL]
#
# AIHC names the compiler and AIHC_DEV the developer tool. The defaults
# are the cabal builds of exe:aihc and exe:aihc-dev. The output directory
# gets the helper, browser tools, and compressed core library assets.
# The helper reads core.bundle.gz for name resolution and type checks.
# The browser linker reads the runtime objects and core archives.
set -euo pipefail

output=""
store=""
optimization="s"
tools="${AIHC_PLAY_TOOLS:-}"
if [[ -z "$tools" ]]; then
  tools=$(nix build .#aihc-play-tools --no-link --print-out-paths)
fi
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) output=$2; shift 2 ;;
    --store) store=$2; shift 2 ;;
    --optimization) optimization=$2; shift 2 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done
if [[ -z "$output" ]]; then
  echo "usage: $0 --output DIR [--store DIR] [--optimization LEVEL]" >&2
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

echo "=== Compiling the core libraries for the browser linker ==="
# The browser linker needs native archives. LTO entries contain System FC.
"$aihc" install "$root/core-libs/aihc-base" \
  --immutable --target wasm32-wasip3 --store "$store" \
  --build-root "$work/runtime-build" -O 1

echo "=== Writing the bundle of the core libraries ==="
mkdir -p "$output"
"$aihc_dev" play-bundle --store "$store/wasm32-wasip3" --output "$work/core.bundle"
gzip -9 -n -c "$work/core.bundle" > "$output/core.bundle.gz"

echo "=== Writing the browser linker inputs ==="
"$aihc_dev" play-runtime --store "$store/wasm32-wasip3" --output "$work/runtime"
echo '(module)' > "$work/empty.wat"
wasm-tools component embed "$root/bin/aihc/compiler/wasm/runtime/wit" "$work/empty.wat" -o "$work/runtime/world.wasm"
node "$root/scripts/package-play-runtime.mjs" "$output" "$work/runtime"

# Local builds use the same tools without an R2 account.
version=$(cat "$tools/version")
if [[ -n "${AIHC_PLAY_TOOLS_URL:-}" ]]; then
  tools_url="${AIHC_PLAY_TOOLS_URL%/}/$version/tools.js"
else
  mkdir -p "$output/tools/$version"
  cp -R "$tools/assets/." "$output/tools/$version/"
  tools_url="./tools/$version/tools.js"
fi
node --input-type=module - "$output/tools.json" "$tools_url" <<'JS'
import { writeFile } from "node:fs/promises";
await writeFile(process.argv[2], JSON.stringify({ url: process.argv[3] }));
JS
cp "$root/docs/aihc-guide/src/play/run.js" "$output/run.js"
cp "$root/docs/aihc-guide/src/play/host.js" "$output/host.js"

echo "=== Transpile the component with jco ==="
"$tools/bin/aihc-play-transpile" "$work/aihc-play.wasm" "$work/out"
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
for asset in "$output/"*; do
  [[ -f "$asset" ]] || continue
  if [[ $(wc -c < "$asset") -gt 26214400 ]]; then
    echo "The asset exceeds the site limit: $asset" >&2
    exit 1
  fi
done
ls -la "$output"
# The component itself goes beside the output directory, for a check under
# wasmtime: wasmtime run -S cli -S http aihc-play.wasm < requests. It is
# not part of the site: Workers rejects an asset above 25 MiB.
cp "$work/aihc-play.wasm" "$(dirname "$output")/aihc-play.wasm"
