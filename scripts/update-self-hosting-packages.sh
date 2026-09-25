#!/usr/bin/env bash
# Write docs/self-hosting-packages.md: the packages that GHC builds for the
# `aihc` executable, in dependency order, from the cabal plan of the repository.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-self-hosting-packages.sh [--output FILE]

  --output FILE  Write the table to FILE
                 (default: docs/self-hosting-packages.md)
  --help         Show this message

The script runs `cabal build --dry-run exe:aihc` to make the cabal plan.
If cabal has no package index, run `cabal update` first.
USAGE
}

output=""

while [ "$#" -gt 0 ]; do
	case "$1" in
	--output)
		output="${2:?--output needs a file}"
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

output="${output:-docs/self-hosting-packages.md}"

# The boot packages that the aihc core libraries replace. Keep this list equal
# to the boot libraries with a standin in tooling/aihc-hackage/src/Aihc/Hackage/Release.hs.
standins='["base","ghc-internal","ghc-prim","rts","system-cxx-std-lib","template-haskell"]'

cabal build --dry-run -v0 exe:aihc

rows="$(
	jq -r --arg root "$repo_root" --argjson standins "$standins" \
		-f scripts/self-hosting-packages.jq dist-newstyle/cache/plan.json
)"

{
	cat <<'HEADER'
# Self-hosting package list

AIHC is self-hosting when it can compile itself. The table below gives every
package that GHC builds for the `aihc` executable, and `aihc` itself last.
A package comes after all of its dependencies.

`scripts/update-self-hosting-packages.sh` writes this file from the cabal plan
of `exe:aihc`. The weekly
[Generated Reports](../.github/workflows/generated-reports-update.yml)
workflow runs it, then installs each package with
`scripts/self-hosting-progress.sh`. The workflow writes the result to the
"Self-compile" row of the README.

The table does not include the boot packages that the aihc core libraries
replace: `base`, `ghc-internal`, `ghc-prim`, `rts`, `system-cxx-std-lib`, and
`template-haskell`. The versions of the other boot packages are the versions
that GHC 9.12.4 ships.

The "Source" column is `hackage` for a Hackage release, `local:PATH` for a
package in this repository, or `git:URL@COMMIT` for a pinned Git commit.

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
