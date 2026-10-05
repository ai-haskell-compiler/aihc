#!/usr/bin/env bash
# Test the bootstrap command with compiler and package fixtures.
set -euo pipefail
cd "$(dirname "$0")/../.."
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cp scripts/test/fixtures/bootstrap/compiler "$work/compiler"
chmod +x "$work/compiler"
for scenario in match mismatch fail first-fail core-fail; do
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
		if cmp -s "$work/$scenario/ghc-aihc" "$work/$scenario/stage2/aihc"; then
			exit 1
		fi
	elif [ "$scenario" = mismatch ]; then
		test "$status" -ne 0
		test -s "$work/$scenario/aihc-comparison.txt"
	elif [ "$scenario" = fail ]; then
		test "$status" -ne 0
		test -f "$work/$scenario/stage3.tsv"
	elif [ "$scenario" = core-fail ]; then
		test "$status" -eq 139
		awk -F '\t' '$1 == "stage3" && $2 == "fail" && $3 == 139 { found = 1 } END { exit !found }' \
			"$work/$scenario/bootstrap.tsv"
		test -s "$work/$scenario/stage3-logs/core-libraries.log"
	else
		test "$status" -ne 0
		test ! -e "$work/$scenario/stage3.tsv"
	fi
done
echo 'Bootstrap fixtures passed.'
