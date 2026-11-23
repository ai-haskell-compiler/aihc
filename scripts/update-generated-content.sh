#!/usr/bin/env bash
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-generated-content.sh [--update|--check]

  --update  Rewrite generated files in place
  --check   Exit non-zero if generated files are out of date
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
cd "$repo_root"

if [ ! -f flake.nix ]; then
	echo "Run this script from inside the repository." >&2
	exit 1
fi

run_cmd() {
	local cmd="$1"
	bash -c "$cmd"
}

resolve_extension_markdown_cmd="${RESOLVE_EXTENSION_PROGRESS_CMD:-nix run .#resolve-extension-progress -- --markdown}"
fixture_extension_markdown_cmd="${FIXTURE_EXTENSION_PROGRESS_CMD:-nix run .#fixture-extension-progress}"
# The self-hosting command gets its output file as the last argument.
self_hosting_packages_cmd="${SELF_HOSTING_PACKAGES_CMD:-nix run .#self-hosting-packages -- --output}"

tmpdir="$(mktemp -d)"
cleanup() {
	rm -rf "$tmpdir"
}
trap cleanup EXIT

resolve_extension_out="$tmpdir/resolve-extension-progress.md"
fixture_extension_out="$tmpdir/fixture-extension-progress.md"
self_hosting_packages_out="$tmpdir/self-hosting-packages.md"

run_cmd "$resolve_extension_markdown_cmd" | sed -n '/^# Name Resolver Extension Support Status/,$p' >"$resolve_extension_out"
run_cmd "$fixture_extension_markdown_cmd" | sed -n '/^# Test Fixture Extension Support Status/,$p' >"$fixture_extension_out"
run_cmd "$self_hosting_packages_cmd $(printf '%q' "$self_hosting_packages_out")"

stale=0

if [ "$mode" = "--update" ]; then
	cp "$resolve_extension_out" docs/aihc-resolve-supported-extensions.md
else
	if ! cmp -s docs/aihc-resolve-supported-extensions.md "$resolve_extension_out"; then
		echo "Generated file out of date: docs/aihc-resolve-supported-extensions.md" >&2
		stale=1
	fi
fi

if [ "$mode" = "--update" ]; then
	cp "$fixture_extension_out" docs/aihc-fixture-supported-extensions.md
else
	if ! cmp -s docs/aihc-fixture-supported-extensions.md "$fixture_extension_out"; then
		echo "Generated file out of date: docs/aihc-fixture-supported-extensions.md" >&2
		stale=1
	fi
fi

if [ "$mode" = "--update" ]; then
	cp "$self_hosting_packages_out" docs/self-hosting-packages.md
else
	if ! cmp -s docs/self-hosting-packages.md "$self_hosting_packages_out"; then
		echo "Generated file out of date: docs/self-hosting-packages.md" >&2
		stale=1
	fi
fi

if [ "$mode" = "--check" ] && [ "$stale" -ne 0 ]; then
	exit 1
fi
