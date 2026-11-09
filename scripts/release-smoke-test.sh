#!/usr/bin/env bash
# Build and run one example with a release executable of aihc. The test
# runs outside the repository checkout, with no AIHC_CORE_LIBS_ROOT, so the
# executable must use the core-libs sources that it carries.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/release-smoke-test.sh --aihc FILE --target TARGET [--example NAME]

  --aihc FILE      The release executable
  --target TARGET  Compile for TARGET, which must be the host
  --example NAME   The example under examples/ (default: arithmetic)
  --help           Show this message
USAGE
}

aihc=""
target=""
example="arithmetic"

while [ "$#" -gt 0 ]; do
	case "$1" in
	--aihc)
		aihc="${2:?--aihc needs a file}"
		shift 2
		;;
	--target)
		target="${2:?--target needs a target}"
		shift 2
		;;
	--example)
		example="${2:?--example needs a name}"
		shift 2
		;;
	--help)
		usage
		exit 0
		;;
	*)
		usage >&2
		exit 2
		;;
	esac
done

if [ -z "$aihc" ] || [ -z "$target" ]; then
	usage >&2
	exit 2
fi

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
case "$aihc" in
/*) ;;
*) aihc="$PWD/$aihc" ;;
esac

source="$repo_root/examples/$example/Main.hs"
expected="$repo_root/examples/$example/stdout"
if [ ! -f "$source" ] || [ ! -f "$expected" ]; then
	echo "The example $example has no Main.hs or no stdout." >&2
	exit 1
fi

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# A release must not read the checkout.
unset AIHC_CORE_LIBS_ROOT
unset aihc_datadir
cp "$source" "$work/Main.hs"
cd "$work"

echo "Building examples/$example with $aihc for $target"
"$aihc" build Main.hs \
	--target "$target" \
	--store "$work/store" \
	--build-root "$work/build" \
	--output "$work/$example"

echo "Running the example"
"$work/$example" >"$work/stdout"
if ! cmp -s "$work/stdout" "$expected"; then
	echo "The output of $example is not the expected output." >&2
	diff "$expected" "$work/stdout" >&2 || true
	exit 1
fi
echo "The example $example gives the expected output."
