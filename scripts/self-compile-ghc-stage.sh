#!/usr/bin/env bash
# Build stage 0 of the aihc self-compile: compile the `aihc` executable with
# GHC and cabal, from an empty package store. Run this script in the
# development shell of the flake, which gives GHC and cabal.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/self-compile-ghc-stage.sh --work-dir DIR --output FILE [OPTION]...

  --work-dir DIR  Build in DIR. DIR must not exist or must be empty.
  --output FILE   Copy the compiled aihc executable to FILE
  --time-file FILE
                  Write the build time in seconds to FILE. The time is for
                  the local packages and the aihc executable only. The
                  script builds the dependencies before it starts the time.
  --help          Show this message

The build uses cabal.project of the repository, the default flags of aihc,
and the default optimization level of cabal. The package store is
DIR/store. Thus, cabal compiles each dependency. Cabal keeps the source
downloads in $CABAL_DIR/packages, so a cache of that directory removes the
downloads. The logs are in DIR/logs.
USAGE
}

work_directory=""
output=""
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

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

if [ ! -f cabal.project ]; then
	echo "Run this script from inside the repository." >&2
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

# The build directory and the package store are in the work directory, so
# the build does not use the earlier builds of the repository.
cabal_build() {
	cabal --store-dir="$work_directory/store" build \
		--builddir="$work_directory/dist-newstyle" \
		--disable-tests --disable-benchmarks \
		--disable-documentation \
		--disable-library-profiling --disable-executable-profiling \
		"$@"
}

echo "Updating the package index"
cabal update 2>&1 | tee "$log_dir/cabal-update.log"

# With --only-dependencies, cabal builds the packages below the targets and
# not the targets themselves. The target "all" is every package of the
# project, so the dependency step builds no local package.
echo "Building the dependencies with GHC $(ghc --numeric-version)"
cabal_build --only-dependencies all 2>&1 | tee "$log_dir/dependencies.log"

echo "Building the aihc executable with GHC $(ghc --numeric-version)"
build_start="$(date +%s)"
cabal_build exe:aihc 2>&1 | tee "$log_dir/aihc.log"
if [ -n "$time_file" ]; then
	mkdir -p "$(dirname "$time_file")"
	echo "$(($(date +%s) - build_start))" >"$time_file"
fi

executable="$(cabal --store-dir="$work_directory/store" list-bin \
	--builddir="$work_directory/dist-newstyle" exe:aihc)"
mkdir -p "$(dirname "$output")"
cp "$executable" "$output"
echo "Wrote $output"
