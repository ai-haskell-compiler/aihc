#!/usr/bin/env bash
# Merge the per-configuration reports of scripts/install-hackage-packages.sh
# into one report per package, so a package that fails for several targets or
# optimization levels gets one issue naming every configuration it fails in.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/merge-hackage-install-reports.sh REPORTS_DIR OUTPUT_DIR

REPORTS_DIR holds one directory per configuration, each written by
`install-hackage-packages.sh --report-dir` and named after its configuration,
such as hackage-install-report-llvm-O2. OUTPUT_DIR receives a failed.txt
naming every package that failed in any configuration, and a Markdown report
per such package with a section per failing configuration.
USAGE
}

if [ "$#" -ne 2 ]; then
	usage >&2
	exit 2
fi

reports_dir="$1"
output_dir="$2"

mkdir -p "$output_dir"
: >"$output_dir/failed.txt"

# The configuration directories, in name order so the sections of a report
# come out in the same order on every run.
configurations=()
for directory in "$reports_dir"/*/; do
	[ -s "$directory/failed.txt" ] || continue
	configurations+=("${directory%/}")
done

if [ "${#configurations[@]}" -eq 0 ]; then
	exit 0
fi

cat "${configurations[@]/%//failed.txt}" | sort -u >"$output_dir/failed.txt"

while read -r package; do
	report="$output_dir/$package.md"
	: >"$report"
	for directory in "${configurations[@]}"; do
		grep -Fxq "$package" "$directory/failed.txt" || continue
		configuration="$(basename "$directory")"
		configuration="${configuration#hackage-install-report-}"
		{
			echo "## $configuration"
			echo
			cat "$directory/$package.md"
			echo
		} >>"$report"
	done
done <"$output_dir/failed.txt"
