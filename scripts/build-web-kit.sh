#!/usr/bin/env bash
# Build the web kit: aihc for the browser.
#
# The kit has the playground helper, the browser tools, the inputs of the
# browser linker, the JavaScript glue of web-kit/src, and manifest.json.
# web-kit/README.md gives the layout. A release ships the kit, and The Lazy
# Reader uses it for its playgrounds.
#
# The helper is bin/aihc-play, compiled by aihc to wasm32-wasip3. jco turns
# the component into a JavaScript module that imports the WASI interfaces
# from web-kit/src/host.js.
#
# Usage: scripts/build-web-kit.sh --output DIR [--version VERSION]
#        [--archive FILE] [--component FILE] [--store DIR]
#        [--optimization LEVEL]
#
# --output DIR     Write the kit to DIR. DIR must be empty or an earlier kit.
# --version V      The version in manifest.json. The default is the version
#                  in bin/aihc/aihc.cabal with the suffix -dev.
# --archive FILE   Also pack DIR as FILE, a .tar.gz archive.
# --component FILE Also write the helper component to FILE, for a check
#                  under wasmtime: wasmtime run -S cli -S http FILE.
#
# Without the variables below, the kit is complete: each file is in DIR.
# The variables are for The Lazy Reader, which keeps large files on R2.
# With AIHC_PLAY_TOOLS_URL, the browser tools stay at
# AIHC_PLAY_TOOLS_URL/TOOLS_VERSION/ and manifest.json gives their URL.
# With AIHC_PLAY_HELPER_URL, manifest.json puts the compressed files at
# AIHC_PLAY_HELPER_URL/BUILD/. Then scripts/publish-play-helper.sh uploads
# them there and removes them from DIR. The preview workflow uses this.
#
# AIHC names the compiler and AIHC_DEV the developer tool. The defaults
# are the cabal builds of exe:aihc and exe:aihc-dev.
# The helper reads core.bundle.gz for name resolution and type checks.
# The browser linker reads the runtime objects and core archives.
# th-image.wasm.gz is the image that runs Template Haskell splices.
set -euo pipefail

output=""
store=""
optimization="s"
version=""
archive=""
component=""
tools="${AIHC_PLAY_TOOLS:-}"
if [[ -z "$tools" ]]; then
  tools=$(nix build .#aihc-play-tools --no-link --print-out-paths)
fi
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) output=$2; shift 2 ;;
    --store) store=$2; shift 2 ;;
    --optimization) optimization=$2; shift 2 ;;
    --version) version=$2; shift 2 ;;
    --archive) archive=$2; shift 2 ;;
    --component) component=$2; shift 2 ;;
    *) echo "unknown option: $1" >&2; exit 2 ;;
  esac
done
if [[ -z "$output" ]]; then
  echo "usage: $0 --output DIR [--version VERSION] [--archive FILE] [--component FILE] [--store DIR] [--optimization LEVEL]" >&2
  exit 2
fi
# manifest.json lists each file of DIR. Thus DIR must not have other files.
if [[ -d "$output" && -n "$(ls -A "$output")" ]]; then
  if [[ ! -f "$output/manifest.json" ]]; then
    echo "The output directory is not empty and is not a kit: $output" >&2
    exit 1
  fi
  rm -rf "$output"
fi
mkdir -p "$output"
output=$(cd "$output" && pwd)
# The script changes the directory. Make each path absolute first.
absolute() { case "$1" in /*) echo "$1" ;; *) echo "$PWD/$1" ;; esac; }
[[ -z "$archive" ]] || archive=$(absolute "$archive")
[[ -z "$component" ]] || component=$(absolute "$component")
[[ -z "$store" ]] || store=$(absolute "$store")

root=$(cd "$(dirname "$0")/.." && pwd)
if [[ -z "$version" ]]; then
  version="$(sed -n 's/^version:[[:space:]]*//p' "$root/bin/aihc/aihc.cabal")-dev"
fi
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
for (const directory of ["bin/aihc-play/app", "bin/aihc-play/src", "bin/aihc/src", "bin/aihc/prim-wiring", ...["fc", "grin", "lir", "native", "th", "wasm"].map((name) => "bin/aihc/compiler/" + name + "/src")]) {
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

echo "=== Compiling the Template Haskell library ==="
# The bundle, the browser linker, and the splice image use the same store
# entries. Thus the facts of the bundle agree with the code of the image.
"$aihc" install "$root/core-libs/aihc-template-haskell" \
  --immutable --target wasm32-wasip3 --store "$store" \
  --build-root "$work/th-build" -O 1

echo "=== Linking the Template Haskell image ==="
# The image runs the splices of a module in the browser. It is the guest
# program of Aihc.Wasm.Engine with the core libraries and the Template
# Haskell library. A splice module loads into the image as a shared module.
"$aihc" build "$root/bin/aihc/compiler/wasm/runtime/Guest.hs" \
  -p aihc-template-haskell \
  --wasm-image --target wasm32-wasip3 --store "$store" \
  --build-root "$work/th-image-build" -O 1 \
  --output "$work/th-image.wasm"
gzip -9 -n -c "$work/th-image.wasm" > "$output/th-image.wasm.gz"

echo "=== Writing the bundle of the core libraries ==="
"$aihc_dev" play-bundle --store "$store/wasm32-wasip3" --output "$work/core.bundle"
gzip -9 -n -c "$work/core.bundle" > "$output/core.bundle.gz"

echo "=== Writing the browser linker inputs ==="
"$aihc_dev" play-runtime --store "$store/wasm32-wasip3" --output "$work/runtime"
echo '(module)' > "$work/empty.wat"
wasm-tools component embed "$root/bin/aihc/compiler/wasm/runtime/wit" "$work/empty.wat" -o "$work/runtime/world.wasm"
node "$root/scripts/pack-web-kit-runtime.mjs" "$output" "$work/runtime"

# Without AIHC_PLAY_TOOLS_URL, the kit has its own copy of the browser tools.
manifest_options=()
if [[ -n "${AIHC_PLAY_TOOLS_URL:-}" ]]; then
  manifest_options+=(--tools-url "${AIHC_PLAY_TOOLS_URL%/}/$(cat "$tools/version")")
else
  mkdir -p "$output/tools"
  cp -R "$tools/assets/." "$output/tools/"
  chmod -R u+w "$output/tools"
fi
if [[ -n "${AIHC_PLAY_HELPER_URL:-}" ]]; then
  manifest_options+=(--data-url "$AIHC_PLAY_HELPER_URL")
fi
cp "$root/web-kit/src/"*.js "$output/"
# The engine core of Aihc.Wasm.Engine runs the splices in the browser.
cp "$root/bin/aihc/compiler/wasm/runtime/engine-core.mjs" "$output/engine-core.js"
cp "$root/web-kit/README.md" "$root/LICENSE" "$output/"

echo "=== Transpile the component with jco ==="
"$tools/bin/aihc-play-transpile" "$work/aihc-play.wasm" "$work/out"
# wasm-opt makes the core module a third of its size. The build goes on
# without it when it is not installed.
# Do not use --all-features. With it, binaryen 132 writes compact imports
# and exact reference types, and browsers do not accept them. Give only the
# features that the compiler output uses, and gc. The output does not use
# gc, but without it the optimized code section is 38% larger.
wasm_features=(
  --mvp-features
  --enable-sign-ext
  --enable-mutable-globals
  --enable-nontrapping-float-to-int
  --enable-bulk-memory
  --enable-bulk-memory-opt
  --enable-multivalue
  --enable-reference-types
  --enable-tail-call
  --enable-threads
  --enable-gc
)
if command -v wasm-opt >/dev/null; then
  echo "=== Shrinking the core module with wasm-opt ==="
  wasm-opt -Oz "${wasm_features[@]}" -o "$work/out/aihc-play-component.core.opt.wasm" "$work/out/aihc-play-component.core.wasm"
  mv "$work/out/aihc-play-component.core.opt.wasm" "$work/out/aihc-play-component.core.wasm"
else
  echo "wasm-opt is not on the PATH; the core module stays unoptimized" >&2
fi
cp "$work/out/aihc-play-component.js" "$output/"
for core in "$work/out/"*.wasm; do
  gzip -9 -n -c "$core" > "$output/$(basename "$core").gz"
done
# manifest.json lists each file of the kit. Write it after all the files
# are in the output directory.
node "$root/scripts/write-web-kit-manifest.mjs" "$output" "$version" "$tools/assets" ${manifest_options[@]+"${manifest_options[@]}"}
# Workers rejects an asset above 25 MiB. R2 has no such limit, but a site
# can also serve the kit as static assets of a Worker, as The Lazy Reader
# does. Thus each file of the kit stays below the limit.
while IFS= read -r -d "" asset; do
  if [[ $(wc -c < "$asset") -gt 26214400 ]]; then
    echo "The asset exceeds the limit of 25 MiB: $asset" >&2
    exit 1
  fi
done < <(find "$output" -type f -print0)
find "$output" -type f -exec ls -l {} +
echo "Kit $version, build $(sed -n 's/^ "build": "\(.*\)",$/\1/p' "$output/manifest.json"): $(du -sk "$output" | cut -f1) KiB"
if [[ -n "$component" ]]; then
  cp "$work/aihc-play.wasm" "$component"
fi
if [[ -n "$archive" ]]; then
  # The archive has one directory, named after the archive.
  name=$(basename "$archive" .tar.gz)
  mkdir -p "$work/archive"
  cp -R "$output" "$work/archive/$name"
  tar -C "$work/archive" -czf "$archive" "$name"
  echo "Wrote $archive ($(wc -c < "$archive" | tr -d ' ') bytes)"
fi
