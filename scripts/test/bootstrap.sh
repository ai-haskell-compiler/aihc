#!/usr/bin/env bash
# Test the bootstrap command with compiler and package fixtures.
set -euo pipefail
cd "$(dirname "$0")/../.."
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp scripts/test/fixtures/bootstrap/compiler "$work/compiler"
chmod +x "$work/compiler"
for scenario in match mismatch fail first-fail; do
	mkdir -p "$work/$scenario"
	status=0
	AIHC="$work/compiler" BOOTSTRAP_CASE="$scenario" \
		bash scripts/self-hosting-progress.sh \
		--list scripts/test/fixtures/bootstrap/packages.md \
		--report "$work/$scenario/stage2.tsv" \
		--log-dir "$work/$scenario/stage2-logs" \
		--bootstrap-dir "$work/$scenario" >"$work/$scenario.log" 2>&1 || status=$?
	if [ "$scenario" = match ]; then
		test "$status" -eq 0
		test -f "$work/$scenario/sha256.txt"
	elif [ "$scenario" = mismatch ]; then
		test "$status" -ne 0
		test -s "$work/$scenario/aihc-comparison.txt"
		test -s "$work/$scenario/ghc-comparison.txt"
	elif [ "$scenario" = fail ]; then
		test "$status" -ne 0
		test -f "$work/$scenario/stage3.tsv"
	else
		test "$status" -ne 0
		test ! -e "$work/$scenario/stage3.tsv"
	fi
done
echo 'Bootstrap fixtures passed.'
