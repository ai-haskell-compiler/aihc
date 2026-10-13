#!/usr/bin/env bash
# Link the bundles that `nix build .#cross-examples-<target>` wrote, run each
# program on this host, and compare its output with the expected files of the
# example.
#
# Usage: link-and-run-example-bundles.sh BUNDLES RESULTS
#
# BUNDLES holds a target file and one directory per example with a status
# file, a compile.log, the example sources and expected outputs under
# example/, and the link bundle under link/. RESULTS receives one directory per
# example holding status, link.log, and the actual and diffed outputs. The
# exit status is non-zero when any example fails.
#
# The link goes through the C driver of the target rather than `aihc
# link-exe`, so the host needs clang and jq but no compiler build.
set -euo pipefail

if [[ $# -ne 2 ]]; then
	echo "usage: $0 BUNDLES RESULTS" >&2
	exit 2
fi
# The programs run inside their own directory, so every path is absolute.
bundles=$(cd "$1" && pwd)
mkdir -p "$2"
results=$(cd "$2" && pwd)
target=$(<"$bundles/target")

# These follow executableLinkArguments and cxxStandardLibraryArguments in
# Aihc.Native, so the link is the same as the link of `aihc link-exe`.
case "$target" in
apple-arm64)
	triple=arm64-apple-darwin
	target_arguments=("-Wl,-dead_strip" "-Wl,-no_exported_symbols")
	cxx_arguments=(-lc++)
	;;
linux-amd64)
	triple=x86_64-unknown-linux-gnu
	target_arguments=("-Wl,--gc-sections")
	cxx_arguments=(-lstdc++)
	;;
*)
	echo "unsupported bundle target: $target" >&2
	exit 2
	;;
esac

# Run PROGRAM with ARGV0 and ARGS under a wall-clock limit of SECONDS. The
# alarm survives exec, so the program dies with SIGALRM (status 142) when the
# limit passes. macOS has no timeout(1), and this needs only perl.
#
# The function replaces the shell that calls it, so call it in a subshell.
# Bash reports a child that a signal stops, and the report goes to the stderr
# of the waiting shell. Without the exec, that shell has the stderr of the
# program, and the report gets into the compared output.
run_limited() {
	local seconds=$1 program=$2 argv0=$3
	shift 3
	exec perl -e 'my ($seconds, $program) = splice @ARGV, 0, 2; alarm $seconds; exec {$program} @ARGV or die "exec $program: $!\n"' \
		"$seconds" "$program" "$argv0" "$@"
}

failed=0
for bundle in "$bundles"/*/; do
	bundle=${bundle%/}
	name=$(basename "$bundle")
	example="$bundle/example"
	link="$bundle/link"
	result="$results/$name"
	mkdir -p "$result"
	status=$(<"$bundle/status")
	if [[ "$status" != ok ]]; then
		echo "$status" >"$result/status"
		echo "$name: $status"
		failed=1
		continue
	fi

	# The program is named after the example because system-environment
	# checks getProgName.
	executable="$result/$name"
	# The order is that of `aihc link-exe`: the system libraries come after
	# every archive, so the members of each archive can resolve symbols
	# from them.
	inputs=()
	while IFS= read -r input; do
		inputs+=("$link/$input")
	done < <(jq -r '.objects[], .archives[]' "$link/link.json")
	libraries=()
	while IFS= read -r argument; do
		libraries+=("$argument")
	done < <(jq -r '.linkArguments // [] | .[]' "$link/link.json")
	libraries+=(-lm)
	if [[ $(jq -r '.cxxStdLib // false' "$link/link.json") == true ]]; then
		libraries+=("${cxx_arguments[@]}")
	fi
	if ! clang "--target=$triple" "${target_arguments[@]}" "${inputs[@]}" "${libraries[@]}" \
		-o "$executable" >"$result/link.log" 2>&1; then
		echo link-failed >"$result/status"
		echo "$name: link failed"
		failed=1
		continue
	fi

	stdin_file=/dev/null
	if [[ -f "$example/stdin" ]]; then
		stdin_file="$example/stdin"
	fi
	args=()
	if [[ -f "$example/args" ]]; then
		while IFS= read -r argument; do
			args+=("$argument")
		done <"$example/args"
	fi
	expected_stderr=/dev/null
	if [[ -f "$example/stderr" ]]; then
		expected_stderr="$example/stderr"
	fi
	expected_exit=0
	if [[ -f "$example/exit" ]]; then
		expected_exit=$(<"$example/exit")
	fi

	run_directory="$result/run"
	mkdir -p "$run_directory"
	set +e
	(
		cd "$run_directory" &&
			run_limited 10 "$executable" "$name" +RTS -M100M -RTS ${args[@]+"${args[@]}"} \
				<"$stdin_file" >"$result/stdout" 2>"$result/stderr"
	)
	actual_exit=$?
	set -e
	echo "$actual_exit" >"$result/exit"

	problems=()
	if ((actual_exit == 142)); then
		problems+=("timed out after 10s")
	elif [[ "$expected_exit" == nonzero ]]; then
		if ((actual_exit == 0)); then
			problems+=("expected a non-zero exit status, got 0")
		fi
	elif ((actual_exit != expected_exit)); then
		problems+=("expected exit status $expected_exit, got $actual_exit")
	fi
	if ! diff --unified --label "$name/stdout-expected" --label "$name/stdout-actual" \
		"$example/stdout" "$result/stdout" >"$result/stdout.diff"; then
		problems+=("stdout differs")
	fi
	if ! diff --unified --label "$name/stderr-expected" --label "$name/stderr-actual" \
		"$expected_stderr" "$result/stderr" >"$result/stderr.diff"; then
		problems+=("stderr differs")
	fi

	if ((${#problems[@]} == 0)); then
		echo ok >"$result/status"
		echo "$name: ok"
	else
		{
			echo run-failed
			printf '%s\n' "${problems[@]}"
		} >"$result/status"
		echo "$name: run failed (${problems[*]})"
		failed=1
	fi
done

exit "$failed"
