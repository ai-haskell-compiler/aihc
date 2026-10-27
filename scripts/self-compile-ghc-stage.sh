#!/usr/bin/env bash
# Build stage 0 of the aihc self-compile: compile the `aihc` executable with
# GHC and cabal, from an empty package store. Run this script in the
# development shell of the flake, which gives GHC and cabal.
#
# The packages that are not in the global package database of GHC get the
# versions of docs/self-hosting-packages.md. Thus, GHC compiles the same
# packages that the later stages compile with aihc.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/self-compile-ghc-stage.sh --work-dir DIR --output FILE [OPTION]...

  --work-dir DIR  Build in DIR. DIR must not exist or must be empty.
  --output FILE   Copy the compiled aihc executable to FILE
  --list FILE     Read the package table from FILE
                  (default: docs/self-hosting-packages.md)
  -O LEVEL        Compile each package at GHC optimization level LEVEL:
                  0, 1 or 2 (default: 2)
  --time-file FILE
                  Write the build time in seconds to FILE. The time does
                  not include the package index update and the source
                  downloads.
  --help          Show this message

The package store is DIR/store. Thus, cabal compiles each dependency.
Cabal keeps the source downloads in $CABAL_DIR/packages, so a cache of that
directory removes the downloads. The build disables the `hackage` and
`pretty-ui` flags of aihc, so the executable has the minimal configuration.
The logs are in DIR/logs.
USAGE
}

work_directory=""
output=""
list_file=""
level="2"
time_file=""

while [ "$#" -gt 0 ]; do
	case "$1" in
	--work-dir)
		work_directory="${2:?--work-dir needs a directory}"
		shift 2
		;;
	--output)
		output="${2:?--output needs a file}"
		shift 2
		;;
	--list)
		list_file="${2:?--list needs a file}"
		shift 2
		;;
	-O)
		level="${2:?-O needs a level}"
		shift 2
		;;
	-O?*)
		level="${1#-O}"
		shift
		;;
	--time-file)
		time_file="${2:?--time-file needs a file}"
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

if [ -z "$work_directory" ] || [ -z "$output" ]; then
	usage >&2
	exit 2
fi

case "$level" in
0 | 1 | 2) ;;
*)
	echo "GHC has no optimization level '$level'." >&2
	exit 2
	;;
esac

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if [ ! -f flake.nix ]; then
	echo "Run this script from inside the repository." >&2
	exit 1
fi

list_file="${list_file:-docs/self-hosting-packages.md}"

if [ ! -f "$list_file" ]; then
	echo "No package list at $list_file." >&2
	exit 1
fi

if [ -e "$work_directory" ] && [ -n "$(ls -A "$work_directory")" ]; then
	echo "The work directory $work_directory is not empty." >&2
	exit 1
fi
mkdir -p "$work_directory"
work_directory="$(cd "$work_directory" && pwd)"
case "$output" in
/*) ;;
*) output="$repo_root/$output" ;;
esac
case "$time_file" in
"" | /*) ;;
*) time_file="$repo_root/$time_file" ;;
esac

log_dir="$work_directory/logs"
mkdir -p "$log_dir"

# The rows of the package table, as "name<TAB>version<TAB>source" lines. The
# header and its separator carry no version number, so they drop out.
packages="$(
	awk -F'|' '
		function trim(s) {
			gsub(/^[ \t]+|[ \t]+$/, "", s)
			return s
		}
		/^\|/ {
			name = trim($2)
			version = trim($3)
			source = trim($4)
			if (version ~ /^[0-9]+(\.[0-9]+)*$/) {
				print name "\t" version "\t" source
			}
		}
	' "$list_file"
)"

if [ -z "$packages" ]; then
	echo "No packages found in $list_file." >&2
	exit 1
fi

# GHC comes with compiled versions of its boot packages. Cabal uses them, and
# a version constraint on them could make cabal compile them again.
global_packages="$(ghc-pkg list --global --simple-output --names-only | tr ' ' '\n')"

# The project is the work directory, so dist-newstyle is also there.
project_file="$work_directory/cabal.project"
{
	# Cabal resolves the dependencies of all components of a local package.
	# The aihc-dev executable needs aihc-testing and the fuzz library of
	# aihc-tc. The build of exe:aihc does not compile them.
	echo "packages:"
	echo "  $repo_root/tooling/aihc-testing"
	while IFS=$'\t' read -r _ _ source; do
		case "$source" in
		local:*) echo "  $repo_root/${source#local:}" ;;
		esac
	done <<<"$packages"
	echo
	echo "constraints:"
	echo "  aihc -hackage -pretty-ui"
	echo "  , aihc-tc +fuzz"
	while IFS=$'\t' read -r name version source; do
		case "$source" in
		hackage:*)
			if ! grep -Fxq "$name" <<<"$global_packages"; then
				echo "  , any.$name ==$version"
			fi
			;;
		esac
	done <<<"$packages"
	echo
	echo "tests: False"
	echo "benchmarks: False"
	echo
	echo "package *"
	echo "  optimization: $level"
	echo "  documentation: False"
	echo "  library-profiling: False"
	echo "  executable-profiling: False"
} >"$project_file"

cabal_build() {
	cabal --store-dir="$work_directory/store" build \
		--project-dir="$work_directory" exe:aihc "$@"
}

echo "Updating the package index"
cabal update 2>&1 | tee "$log_dir/cabal-update.log"

echo "Downloading the package sources"
cabal_build --only-download 2>&1 | tee "$log_dir/download.log"

echo "Building the aihc executable with GHC $(ghc --numeric-version) at -O$level"
build_start="$(date +%s)"
cabal_build 2>&1 | tee "$log_dir/aihc.log"
if [ -n "$time_file" ]; then
	mkdir -p "$(dirname "$time_file")"
	echo "$(($(date +%s) - build_start))" >"$time_file"
fi

executable="$(cabal --store-dir="$work_directory/store" list-bin \
	--project-dir="$work_directory" exe:aihc)"
mkdir -p "$(dirname "$output")"
cp "$executable" "$output"
echo "Wrote $output"
