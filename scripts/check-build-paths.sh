#!/usr/bin/env bash
# Make sure that no build path got into a file: the paths of the checkout,
# of the work directory, and of the home directory. A build that keeps such
# a path gives different bytes in a different directory.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/check-build-paths.sh FILE PATH...

  Search the strings of FILE for each PATH. Write each string that contains
  a PATH, and fail when there is one.
USAGE
}

if [ "$#" -lt 2 ]; then
	usage >&2
	exit 2
fi

file="$1"
shift

found=0
for path in "$@"; do
	if [ -z "$path" ]; then
		continue
	fi
	matches="$(strings -a "$file" | grep -F -- "$path" || true)"
	if [ -n "$matches" ]; then
		echo "$file contains the build path $path:" >&2
		echo "$matches" | head -n 20 >&2
		found=1
	fi
done

if [ "$found" -ne 0 ]; then
	exit 1
fi
echo "$file contains no build path."
