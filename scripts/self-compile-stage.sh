#!/usr/bin/env bash
# Build one stage of the aihc self-compile: compile the `aihc` executable of
# this repository with a given aihc compiler, with `aihc build exe:aihc`.
#
# The paths of the work directory get into the executable. Thus, to compare
# two stages byte for byte, build each stage in the same work directory path.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/self-compile-stage.sh --work-dir DIR --output FILE [OPTION]...

  --work-dir DIR  Build in DIR. DIR must not exist or must be empty. Use the
                  same DIR for each stage of a comparison.
  --output FILE   Copy the compiled aihc executable to FILE
  --target TARGET Compile for TARGET (default: linux-amd64)
  -O LEVEL        Compile at optimization level LEVEL: 0, 1, 2 or s
                  (default: 2)
  --timeout SECONDS
                  Stop the build after SECONDS (default: 18000)
  --time-file FILE
                  Install the Hackage packages and the core libraries
                  first. Then build the local packages and the aihc
                  executable, and write the time of that build in seconds
                  to FILE.
  --help          Show this message

The compiler is taken from $AIHC, and defaults to `aihc` on PATH.
The build uses the flags of cabal.project and the default flags of aihc.
The logs are in DIR/logs.
USAGE
}

work_directory=""
output=""
target="linux-amd64"
level="2"
build_timeout="18000"
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
	--target)
		target="${2:?--target needs a target}"
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
	--timeout)
		build_timeout="${2:?--timeout needs a number of seconds}"
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

aihc="${AIHC:-aihc}"

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

# A compiler that aihc compiled does not know the repository. It reads the
# runtime headers from aihc_datadir and the core libraries from
# AIHC_CORE_LIBS_ROOT. Each stage gets the same values.
export aihc_datadir="$repo_root/bin/aihc"
export AIHC_CORE_LIBS_ROOT="$repo_root"

# The level is part of the identity of an installed package, so each build
# step uses the level of the stage.
aihc_build=(
	"$aihc" build exe:aihc
	--store "$work_directory/store" --build-root "$work_directory/build"
	--target "$target" -O "$level"
)

run_with_timeout() {
	if command -v timeout >/dev/null 2>&1; then
		timeout "$build_timeout" "$@"
	else
		"$@"
	fi
}

# Copy compiler output to the job log before a runner shutdown can prevent
# the artifact upload. The pipefail option preserves compiler failures.
if [ -n "$time_file" ]; then
	echo "Installing the dependencies of aihc for $target at -O$level"
	if ! "${aihc_build[@]}" --only-dependencies 2>&1 | tee "$log_dir/dependencies.log"; then
		exit 1
	fi
fi

echo "Building the aihc executable for $target at -O$level"
build_start="$(date +%s)"
status=0
run_with_timeout "${aihc_build[@]}" -o "$work_directory/bin" \
	2>&1 | tee "$log_dir/aihc.log" || status=$?
if [ "$status" -ne 0 ]; then
	if [ "$status" -eq 124 ]; then
		echo "The build timed out after $build_timeout seconds." >&2
	fi
	exit "$status"
fi

if [ -n "$time_file" ]; then
	mkdir -p "$(dirname "$time_file")"
	echo "$(($(date +%s) - build_start))" >"$time_file"
fi

mkdir -p "$(dirname "$output")"
cp "$work_directory/bin/aihc" "$output"
echo "Wrote $output"
