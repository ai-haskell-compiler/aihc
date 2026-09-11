#!/usr/bin/env bash
# Install the pinned Hackage packages of docs/hackage-install-packages.md with
# `aihc install --lint`, and report which of them failed.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/install-hackage-packages.sh [OPTION]...

  --list FILE     Read the package table from FILE
                  (default: docs/hackage-install-packages.md)
  --target TARGET Install for TARGET (default: llvm)
  --store DIR     Use DIR as the package store (default: a temporary directory)
  --report-dir DIR
                  Write a Markdown report per failed package to DIR, plus a
                  failed.txt naming them
  --help          Show this message

The aihc executable is taken from $AIHC, and defaults to `aihc` on PATH.
Exits non-zero when any package fails to install.
USAGE
}

list_file=""
target="llvm"
store=""
report_dir=""

while [ "$#" -gt 0 ]; do
	case "$1" in
	--list)
		list_file="${2:?--list needs a file}"
		shift 2
		;;
	--target)
		target="${2:?--target needs a target}"
		shift 2
		;;
	--store)
		store="${2:?--store needs a directory}"
		shift 2
		;;
	--report-dir)
		report_dir="${2:?--report-dir needs a directory}"
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

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if [ ! -f flake.nix ]; then
	echo "Run this script from inside the repository." >&2
	exit 1
fi

aihc="${AIHC:-aihc}"
list_file="${list_file:-docs/hackage-install-packages.md}"

if [ ! -f "$list_file" ]; then
	echo "No package list at $list_file." >&2
	exit 1
fi

# The rows of the one Markdown table in the list file, as "name version" lines.
# The header and its separator carry no version number, so they drop out.
packages="$(
	awk -F'|' '
		/^\|/ {
			name = $2
			version = $3
			gsub(/^[ \t]+|[ \t]+$/, "", name)
			gsub(/^[ \t]+|[ \t]+$/, "", version)
			if (version ~ /^[0-9]+(\.[0-9]+)*$/) {
				print name, version
			}
		}
	' "$list_file"
)"

if [ -z "$packages" ]; then
	echo "No packages found in $list_file." >&2
	exit 1
fi

work_directory="$(mktemp -d)"
trap 'rm -rf "$work_directory"' EXIT

workspace="$work_directory/workspace"
logs="$work_directory/logs"
mkdir -p "$workspace" "$logs"

if [ -z "$store" ]; then
	store="$work_directory/store"
fi
mkdir -p "$store"

failed=""

# Unpack each release next to the others. `aihc install` prefers the siblings of
# the package it installs over Hackage, so every dependency is the pinned
# version of this list rather than whatever Hackage prefers today.
while read -r name version; do
	echo "Fetching $name-$version"
	archive="$work_directory/$name-$version.tar.gz"
	if ! curl --fail --silent --show-error --location \
		--output "$archive" \
		"https://hackage.haskell.org/package/$name-$version/$name-$version.tar.gz" \
		2>"$logs/$name.log"; then
		# A version that is not on Hackage is a failure of the list, and is
		# reported like a failed install rather than stopping the run.
		failed="$failed $name-$version"
		echo "  failed to download:"
		sed 's/^/  /' "$logs/$name.log"
		continue
	fi
	tar -xzf "$archive" -C "$work_directory"
	rm -f "$archive"
	mv "$work_directory/$name-$version" "$workspace/$name"
done <<<"$packages"

echo "Preparing the $target toolchain in $store"
"$aihc" prepare-runtime --target "$target" --store "$store"
"$aihc" install core-libs/aihc-base --store "$store" --immutable --lint --target "$target"

while read -r name version; do
	case " $failed " in
	*" $name-$version "*)
		echo "Skipping $name-$version, which did not download."
		continue
		;;
	esac
	echo "Installing $name-$version"
	log="$logs/$name.log"
	if "$aihc" install "$workspace/$name" \
		--store "$store" --immutable --lint --target "$target" >"$log" 2>&1; then
		echo "  ok"
	else
		failed="$failed $name-$version"
		echo "  failed:"
		sed 's/^/  /' "$log"
	fi
done <<<"$packages"

if [ -n "$report_dir" ]; then
	mkdir -p "$report_dir"
	: >"$report_dir/failed.txt"
	while read -r name version; do
		case " $failed " in
		*" $name-$version "*) ;;
		*) continue ;;
		esac
		echo "$name-$version" >>"$report_dir/failed.txt"
		{
			echo "\`aihc install $name-$version --lint\` failed for target \`$target\`."
			echo
			echo "Reproduce it with:"
			echo
			echo '```console'
			echo "\$ nix run .#install-hackage-packages"
			echo '```'
			echo
			echo "The package list is in \`$list_file\`."
			echo
			echo "Last lines of the log; the workflow run holds all of it."
			echo
			echo '```'
			tail -n 60 "$logs/$name.log"
			echo '```'
		} >"$report_dir/$name-$version.md"
	done <<<"$packages"
fi

if [ -n "$failed" ]; then
	echo "Failed to install:$failed" >&2
	exit 1
fi

echo "Installed every package of $list_file."
