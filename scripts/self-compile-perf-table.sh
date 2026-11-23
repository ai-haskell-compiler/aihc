#!/usr/bin/env bash
# Print the self-compile timings of the Self-Compile Performance workflow as a
# Markdown table.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/self-compile-perf-table.sh RESULTS

Prints a Markdown table of the self-compile timings.

RESULTS is a file with one tab-separated line for each stage, in stage order:

  STAGE<TAB>COMPILER<TAB>LEVEL<TAB>SECONDS<TAB>BYTES

STAGE is the name of the stage, COMPILER is the compiler that built it,
LEVEL is the optimization level of the build (for example -O2), SECONDS is
the build time, and BYTES is the size of the executable. The first line is
the reference for the relative times.

Set COMMIT to the full commit hash of the measurement, and set RUNNER to the
name of the machine.
USAGE
}

case "${1:-}" in
-h | --help)
	usage
	exit 0
	;;
esac
if [ "$#" -ne 1 ]; then
	usage >&2
	exit 2
fi

results="$1"
github_url="https://github.com/ai-haskell-compiler/aihc"
workflow_url="$github_url/actions/workflows/self-compile-perf.yml"
commit="${COMMIT:-$(git rev-parse HEAD)}"
runner="${RUNNER:-$(uname -sm)}"

echo "The [Self-Compile Performance workflow]($workflow_url) compiles aihc $(grep -c . "$results") times."
echo "Each build starts with an empty package store."
echo "Before the time starts, the build installs all dependencies of aihc."
echo "Thus, each time is for the local packages of this repository and the \`aihc\` executable."
echo "GHC compiles at its default level \`-O1\`."
echo "Each aihc build uses the native backend for \`linux-amd64\`."
echo "The \`-O2\` stage1 compiles both stage2 builds."
echo "Lower is better."
echo
echo "| Stage | Compiler | Level | Build time | Relative to stage0 | Executable size |"
echo "| --- | --- | --- | ---: | ---: | ---: |"
awk -F'\t' '
	function duration(s) {
		if (s >= 3600) {
			return sprintf("%dh %02dm %02ds", s / 3600, (s % 3600) / 60, s % 60)
		}
		return sprintf("%dm %02ds", s / 60, s % 60)
	}
	NF != 5 || $3 !~ /^-O[0-2s]$/ || $4 !~ /^[0-9]+$/ || $5 !~ /^[0-9]+$/ {
		printf "Bad line %d in the results: %s\n", NR, $0 > "/dev/stderr"
		bad = 1
		exit 1
	}
	NR == 1 {
		if ($4 == 0) {
			print "The stage0 build time is zero." > "/dev/stderr"
			bad = 1
			exit 1
		}
		reference = $4
	}
	{
		printf "| %s | %s | `%s` | %s | %.2f× | %.1f MB |\n", $1, $2, $3, duration($4), $4 / reference, $5 / 1000000
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
