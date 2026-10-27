#!/usr/bin/env bash
# Write the self-compile timings of the Self-Compile Performance workflow into
# README.md.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/update-readme-self-compile-perf.sh RESULTS [README]

Updates the self-compile-perf section of README (default: README.md).

RESULTS is a file with one tab-separated line for each stage, in stage order:

  STAGE<TAB>COMPILER<TAB>SECONDS<TAB>BYTES

STAGE is the name of the stage, COMPILER is the compiler that built it,
SECONDS is the build time, and BYTES is the size of the executable. The first
line is the reference for the relative times.

Set COMMIT to the full commit hash of the measurement, and set RUNNER to the
name of the machine. If the README has no self-compile-perf section, the
script adds it after the self-hosting-details section.
USAGE
}

case "${1:-}" in
-h | --help)
	usage
	exit 0
	;;
esac
if [ "$#" -lt 1 ] || [ "$#" -gt 2 ]; then
	usage >&2
	exit 2
fi

results="$1"
readme="${2:-README.md}"
github_url="https://github.com/ai-haskell-compiler/aihc"
workflow_url="$github_url/actions/workflows/self-compile-perf.yml"
commit="${COMMIT:-$(git rev-parse HEAD)}"
runner="${RUNNER:-$(uname -sm)}"

marker="self-compile-perf"
start="<!-- AUTO-GENERATED: START ${marker} -->"
end="<!-- AUTO-GENERATED: END ${marker} -->"
anchor="<!-- AUTO-GENERATED: END self-hosting-details -->"

tmpdir="$(mktemp -d)"
cleanup() {
	rm -rf "$tmpdir"
}
trap cleanup EXIT

{
	echo "The [Self-Compile Performance workflow]($workflow_url) compiles aihc three times."
	echo "Each build starts with an empty package store."
	echo "Before the time starts, the build installs all dependencies of aihc."
	echo "Thus, each time is for the local packages of this repository and the \`aihc\` executable."
	echo "GHC compiles at its default level \`-O1\`."
	echo "Each aihc build uses the native backend at \`-O2\` for \`linux-amd64\`."
	echo "Lower is better."
	echo
	echo "| Stage | Compiler | Build time | Relative to stage0 | Executable size |"
	echo "| --- | --- | ---: | ---: | ---: |"
	awk -F'\t' '
		function duration(s) {
			if (s >= 3600) {
				return sprintf("%dh %02dm %02ds", s / 3600, (s % 3600) / 60, s % 60)
			}
			return sprintf("%dm %02ds", s / 60, s % 60)
		}
		NF != 4 || $3 !~ /^[0-9]+$/ || $4 !~ /^[0-9]+$/ {
			printf "Bad line %d in the results: %s\n", NR, $0 > "/dev/stderr"
			bad = 1
			exit 1
		}
		NR == 1 {
			if ($3 == 0) {
				print "The stage0 build time is zero." > "/dev/stderr"
				bad = 1
				exit 1
			}
			reference = $3
		}
		{
			printf "| %s | %s | %s | %.2f× | %.1f MB |\n", $1, $2, duration($3), $3 / reference, $4 / 1000000
		}
		END {
			if (bad) {
				exit 1
			}
			if (NR == 0) {
				print "The results file is empty." > "/dev/stderr"
				exit 1
			}
		}
	' "$results"
	echo
	echo "Machine \`$runner\`, commit [\`${commit:0:9}\`]($github_url/commit/$commit) ($(date -u +%Y-%m-%d))."
} >"$tmpdir/self-compile-perf.md"

if ! grep -Fxq "$start" "$readme"; then
	if [ "$(grep -Fxc "$anchor" "$readme" || true)" -ne 1 ]; then
		echo "Expected exactly one '$anchor' line in $readme" >&2
		exit 1
	fi
	awk -v anchor="$anchor" -v start="$start" -v end="$end" '
		{ print }
		$0 == anchor {
			print ""
			print "### Self-compile performance"
			print ""
			print start
			print end
		}
	' "$readme" >"$tmpdir/readme.out"
	cp "$tmpdir/readme.out" "$readme"
fi

if [ "$(grep -Fxc "$start" "$readme" || true)" -ne 1 ] || [ "$(grep -Fxc "$end" "$readme" || true)" -ne 1 ]; then
	echo "Expected exactly one block marker pair for '${marker}' in ${readme}" >&2
	exit 1
fi
awk -v start="$start" -v end="$end" -v content_file="$tmpdir/self-compile-perf.md" '
	$0 == start {
		print
		while ((getline line < content_file) > 0) print line
		skipping = 1
		next
	}
	$0 == end { skipping = 0 }
	!skipping { print }
' "$readme" >"$tmpdir/readme.out"
cp "$tmpdir/readme.out" "$readme"
