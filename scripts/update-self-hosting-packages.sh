#!/usr/bin/env bash
# Write docs/self-hosting-packages.md: the packages that aihc plans for its own
# `aihc` executable, in dependency order.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-self-hosting-packages.sh [OPTION]...

  --output FILE   Write the table to FILE
                  (default: docs/self-hosting-packages.md)
  --target TARGET Plan for TARGET (default: llvm)
  --help          Show this message

The aihc executable is taken from $AIHC, and defaults to `aihc` on PATH.
The script runs `aihc plan bin/aihc --executable aihc`. The plan uses
bin/aihc/aihc.lock, and writes it when the lock is absent or stale.
USAGE
}

output=""
target="llvm"

while [ "$#" -gt 0 ]; do
	case "$1" in
	--output)
		output="${2:?--output needs a file}"
		shift 2
		;;
	--target)
		target="${2:?--target needs a target}"
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
output="${output:-docs/self-hosting-packages.md}"

work_directory="$(mktemp -d)"
trap 'rm -rf "$work_directory"' EXIT

# The packages of bin/aihc find their siblings under bin/ by themselves. The
# packages under components/ and tooling/ are in a workspace of links. Each
# link has the name of its directory, so "links.tsv" maps a link back to its
# path in the repository.
workspace="$work_directory/workspace"
mkdir -p "$workspace"
: >"$work_directory/links.tsv"
for directory in components/* tooling/*; do
	if compgen -G "$directory/*.cabal" >/dev/null; then
		ln -s "$repo_root/$directory" "$workspace/$(basename "$directory")"
		printf '%s\t%s\n' "$workspace/$(basename "$directory")" "$directory" >>"$work_directory/links.tsv"
	fi
done

"$aihc" plan bin/aihc --executable aihc --workspace "$workspace" --target "$target" >"$work_directory/plan.tsv"

# The core libraries come with aihc, so the table leaves them out, also from
# the dependencies. A local path becomes a path in the repository.
rows="$(
	awk -F'\t' -v repo="$repo_root/" '
		FNR == 1 { pass++ }
		pass == 1 {
			link[$1] = $2
			next
		}
		pass == 2 {
			if ($3 == "core") {
				core[$1] = 1
			}
			next
		}
		$3 == "core" { next }
		{
			source = $3
			if (source ~ /^local:/) {
				path = substr(source, 7)
				if (path in link) {
					path = link[path]
				} else if (index(path, repo) == 1) {
					path = substr(path, length(repo) + 1)
				}
				source = "local:" path
			}
			n = split($4, needs, ",")
			depends = ""
			for (i = 1; i <= n; i++) {
				if (needs[i] != "-" && !(needs[i] in core)) {
					depends = depends (depends == "" ? "" : ",") needs[i]
				}
			}
			print $1 "\t" $2 "\t" source "\t" (depends == "" ? "-" : depends)
		}
	' "$work_directory/links.tsv" "$work_directory/plan.tsv" "$work_directory/plan.tsv"
)"

{
	cat <<'HEADER'
# Self-hosting package list

AIHC is self-hosting when it can compile itself. The table below gives every
package that aihc plans for the `aihc` executable, and `aihc` itself last.
A package comes after all of its dependencies.

`scripts/update-self-hosting-packages.sh` writes this file with
`aihc plan bin/aihc --executable aihc`. The plan uses `bin/aihc/aihc.lock`.
The weekly
[Generated Reports](../.github/workflows/generated-reports-update.yml)
workflow runs the script, then compiles each package with
`scripts/self-hosting-progress.sh`. The workflow writes the result to the
"Self-compile" row of the README.

The table does not include the aihc core libraries, because they come with
aihc. The "Source" column is `hackage:REVISION` for a Hackage release at a
cabal file revision, or `local:PATH` for a package in this repository.

## Packages

| Package | Version | Source | Dependencies |
| ------- | ------- | ------ | ------------ |
HEADER
	while IFS=$'\t' read -r name version source depends; do
		if [ "$depends" = "-" ]; then
			depends=""
		else
			depends="${depends//,/, }"
		fi
		echo "| $name | $version | $source | $depends |"
	done <<<"$rows"
} >"$output"

echo "Wrote $(wc -l <<<"$rows" | tr -d ' ') packages to $output."
