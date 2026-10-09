#!/usr/bin/env bash
# Build one stage of the aihc self-compile: compile the `aihc` executable of
# the packages of docs/self-hosting-packages.md with a given aihc compiler.
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
  --list FILE     Read the package table from FILE
                  (default: docs/self-hosting-packages.md)
  --target TARGET Compile for TARGET (default: linux-amd64)
  -O LEVEL        Compile at optimization level LEVEL: 0, 1, 2 or s
                  (default: 2)
  --timeout SECONDS
                  Stop the build after SECONDS (default: 18000)
  --help          Show this message

The compiler is taken from $AIHC, and defaults to `aihc` on PATH.
The build disables the `hackage` and `pretty-ui` flags of aihc, so the
executable has the minimal configuration. The logs are in DIR/logs.
USAGE
}

work_directory=""
output=""
list_file=""
target="linux-amd64"
level="2"
build_timeout="18000"

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

if [ ! -f flake.nix ]; then
	echo "Run this script from inside the repository." >&2
	exit 1
fi

aihc="${AIHC:-aihc}"
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

workspace="$work_directory/workspace"
downloads="$work_directory/downloads"
log_dir="$work_directory/logs"
mkdir -p "$workspace" "$downloads" "$log_dir"

# A compiler that aihc compiled does not know the repository. It reads the
# runtime headers from aihc_datadir and the core libraries from
# AIHC_CORE_LIBS_ROOT. Each stage gets the same values.
export aihc_datadir="$repo_root/bin/aihc"
export AIHC_CORE_LIBS_ROOT="$repo_root"

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

# The last row is the package that compiles itself.
root_name="$(tail -n 1 <<<"$packages" | cut -f1)"

download() {
	curl --fail --silent --show-error --location \
		--retry 5 --retry-all-errors \
		--output "$1" "$2"
}

# Put the source of each package in the workspace. `aihc build` prefers the
# siblings of the package that it builds over Hackage, so every dependency is
# the version of the list.
while IFS=$'\t' read -r name version source; do
	destination="$workspace/$name"
	case "$source" in
	hackage:*)
		revision="${source#hackage:}"
		archive="$downloads/$name-$version.tar.gz"
		download "$archive" \
			"https://hackage.haskell.org/package/$name-$version/$name-$version.tar.gz"
		tar -xzf "$archive" -C "$downloads"
		mv "$downloads/$name-$version" "$destination"
		# The tarball carries revision 0 of the cabal file. Take the
		# revision of the plan, which can relax the version bounds.
		download "$destination/$name.cabal" \
			"https://hackage.haskell.org/package/$name-$version/revision/$revision.cabal"
		;;
	local:*)
		mkdir -p "$destination"
		tar -C "$repo_root/${source#local:}" \
			--exclude=./dist-newstyle --exclude=./.aihc-target \
			-cf - . | tar -C "$destination" -xf -
		;;
	*)
		echo "Unknown source '$source' for $name." >&2
		exit 1
		;;
	esac
done <<<"$packages"
rm -rf "$downloads"

run_with_timeout() {
	if command -v timeout >/dev/null 2>&1; then
		timeout "$build_timeout" "$@"
	else
		"$@"
	fi
}

# The level is part of the identity of an installed package, so aihc-base is
# installed at the level of the build.
echo "Installing aihc-base for $target at -O$level"
if ! "$aihc" install core-libs/aihc-base \
	--store "$work_directory/store" --immutable --target "$target" -O "$level" \
	>"$log_dir/aihc-base.log" 2>&1; then
	tail -n 40 "$log_dir/aihc-base.log" >&2
	exit 1
fi

echo "Building the aihc executable of $root_name for $target at -O$level"
status=0
run_with_timeout "$aihc" build "$workspace/$root_name" --executable aihc \
	--constraint "aihc -hackage -pretty-ui" \
	--store "$work_directory/store" --build-root "$work_directory/build" \
	--target "$target" -O "$level" -o "$work_directory/bin" \
	>"$log_dir/aihc.log" 2>&1 || status=$?
if [ "$status" -ne 0 ]; then
	if [ "$status" -eq 124 ]; then
		echo "The build timed out after $build_timeout seconds." >&2
	fi
	tail -n 40 "$log_dir/aihc.log" >&2
	exit "$status"
fi

mkdir -p "$(dirname "$output")"
cp "$work_directory/bin/aihc" "$output"
echo "Wrote $output"
