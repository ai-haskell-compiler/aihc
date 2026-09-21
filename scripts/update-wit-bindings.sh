#!/usr/bin/env bash
# Regenerate the WASI 0.3 C bindings of the aihc-rts package from the
# component world under bin/aihc/compiler/wasm/runtime/wit, or check that
# the committed bindings are what wit-bindgen writes today.
#
# The bindings are committed so that a compiler needs no wit-bindgen to
# build a wasm32-wasip3 program. The component type that wit-bindgen would
# put in an object file is embedded at link time instead, with
# `wasm-tools component embed`, from the same world.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-wit-bindings.sh [--update|--check]

  --update  Rewrite core-libs/aihc-rts/wasm/generated in place
  --check   Exit non-zero if the committed bindings are out of date
USAGE
}

if [ "$#" -ne 1 ]; then
	usage >&2
	exit 2
fi

mode="$1"
case "$mode" in
--update | --check) ;;
*)
	usage >&2
	exit 2
	;;
esac

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
world="$repo_root/bin/aihc/compiler/wasm/runtime/wit"
committed="$repo_root/core-libs/aihc-rts/wasm/generated"

generated="$(mktemp -d)"
trap 'rm -rf "$generated"' EXIT

wit-bindgen c --world command --no-object-file --out-dir "$generated" "$world"

# Keep canonical ABI buffers in the explicit host scope until the call ends.
python3 - "$generated/command.c" <<'PY'
import pathlib
import sys

path = pathlib.Path(sys.argv[1])
source = path.read_text()
allocator = """  (void) old_size;
  if (new_size == 0) return (void*) align;
  void *ret = realloc(ptr, new_size);
  if (!ret) abort();
  return ret;"""
replacement = """  if (new_size == 0) return (void*) align;
  if (align > _Alignof(max_align_t)) aihc_fail("unsupported canonical ABI alignment");
  void *ret = aihc_wasi_allocate(new_size);
  if (old_size != 0) {
    memcpy(ret, ptr, old_size < new_size ? old_size : new_size);
  }
  return ret;"""
if source.count(allocator) != 1:
    sys.exit("The canonical ABI allocator changed. Update its GC adapter.")
source = source.replace('#include "command.h"', '#include "command.h"\n#include "aihc_runtime_internal.h"')
source = source.replace(allocator, replacement)
source = source.replace("    free(", "    (void)(")
path.write_text(source)
PY

case "$mode" in
--update)
	mkdir -p "$committed"
	cp "$generated"/command.c "$generated"/command.h "$committed/"
	echo "Wrote $committed from $world."
	;;
--check)
	if ! diff --unified --recursive "$committed" "$generated"; then
		echo "The committed WASI bindings differ from what wit-bindgen writes; run scripts/update-wit-bindings.sh --update." >&2
		exit 1
	fi
	;;
esac
