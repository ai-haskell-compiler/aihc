#!/usr/bin/env bash
# Install each package of docs/self-hosting-packages.md with `aihc install`,
# build the executable of the last package with `aihc build`, and write the
# status of each package to a report.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/self-hosting-progress.sh --report FILE [OPTION]...

  --report FILE   Write one "name<TAB>version<TAB>status<TAB>detail" line per
                  package to FILE. The status is "pass", "fail", or "blocked".
                  For "blocked", the detail gives the failed dependencies.
  --list FILE     Read the package table from FILE
                  (default: docs/self-hosting-packages.md)
  --target TARGET Install for TARGET (default: llvm)
  -O LEVEL        Install at optimization level LEVEL: 0, 1, 2 or s
                  (default: 0)
  --store DIR     Use DIR as the package store (default: a temporary directory)
  --log-dir DIR   Keep the install log of each package in DIR
  --bootstrap-dir DIR
                  Build two compiler generations and compare their bytes.
                  Keep the executables, reports, and logs in DIR.
  --executable NAME
                  Build the executable NAME of the last package of the list,
                  which is the package that compiles itself (default: aihc)
  --timeout SECONDS
                  Stop the install of one package after SECONDS
                  (default: 1800)
  --help          Show this message

The aihc executable is taken from $AIHC, and defaults to `aihc` on PATH.
Without --bootstrap-dir, a package failure does not make the script fail.
With --bootstrap-dir, a package failure or byte difference makes it fail.
The script also fails when it cannot prepare the aihc core libraries.
USAGE
}

report=""
list_file=""
target="llvm"
level="0"
store=""
log_dir=""
package_timeout="1800"
runtime_args=()
root_executable="aihc"
bootstrap_dir=""
bootstrap_stage=""

while [ "$#" -gt 0 ]; do
	case "$1" in
	--report)
		report="${2:?--report needs a file}"
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
	--store)
		store="${2:?--store needs a directory}"
		shift 2
		;;
	--log-dir)
		log_dir="${2:?--log-dir needs a directory}"
		shift 2
		;;
	--executable)
		root_executable="${2:?--executable needs a name}"
		shift 2
		;;
	--bootstrap-dir)
		bootstrap_dir="${2:?--bootstrap-dir needs a directory}"
		shift 2
		;;
	--timeout)
		package_timeout="${2:?--timeout needs a number of seconds}"
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

if [ -z "$report" ]; then
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

# The rows of the package table, as "name<TAB>version<TAB>source<TAB>depends"
# lines. The header and its separator carry no version number, so they drop
# out. An empty dependency list becomes "-".
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
			depends = trim($5)
			gsub(/[ \t]/, "", depends)
			if (depends == "") {
				depends = "-"
			}
			if (version ~ /^[0-9]+(\.[0-9]+)*$/) {
				print name "\t" version "\t" source "\t" depends
			}
		}
	' "$list_file"
)"

if [ -z "$packages" ]; then
	echo "No packages found in $list_file." >&2
	exit 1
fi

work_directory="$(mktemp -d)"
finish_bootstrap() {
	local result=$?
	if [ -n "$bootstrap_stage" ]; then
		local outcome=fail
		if [ "$result" -eq 0 ]; then
			outcome=pass
		fi
		printf '%s\t%s\t%s\n' "$bootstrap_stage" "$outcome" "$result" >>"$bootstrap_dir/bootstrap.tsv"
	fi
	rm -rf "$work_directory"
}
trap finish_bootstrap EXIT

workspace="$work_directory/workspace"
if [ -z "$log_dir" ]; then
	log_dir="$work_directory/logs"
fi
mkdir -p "$workspace" "$log_dir"

if [ -z "$store" ]; then
	store="$work_directory/store"
fi
mkdir -p "$store"

output_args=()
if [ -n "$bootstrap_dir" ]; then
	mkdir -p "$bootstrap_dir"
	bootstrap_dir="$(cd "$bootstrap_dir" && pwd)"
	if [ -e "$bootstrap_dir/stage2" ] || [ -e "$bootstrap_dir/stage3" ]; then
		echo "The bootstrap output directory must contain no compiler stages." >&2
		exit 1
	fi
	cp "$(command -v "$aihc")" "$bootstrap_dir/ghc-aihc"
	printf 'stage\tstatus\texit_status\n' >"$bootstrap_dir/bootstrap.tsv"
	bootstrap_stage=stage2
	output_args=(--output "$bootstrap_dir/stage2")
fi

# Put the source of one package in the workspace, and write the messages to
# the standard output. `aihc install` prefers the siblings of the package it
# installs over Hackage, so every dependency is the version of the list.
fetch_package() {
	local name="$1"
	local version="$2"
	local source="$3"
	local destination="$workspace/$name"

	case "$source" in
	hackage:*)
		local revision="${source#hackage:}"
		local archive="$work_directory/$name-$version.tar.gz"
		curl --fail --silent --show-error --location \
			--output "$archive" \
			"https://hackage.haskell.org/package/$name-$version/$name-$version.tar.gz" || return 1
		tar -xzf "$archive" -C "$work_directory" || return 1
		rm -f "$archive"
		mv "$work_directory/$name-$version" "$destination" || return 1
		# The tarball carries revision 0 of the cabal file. Take the
		# revision of the plan, which can relax the version bounds.
		curl --fail --silent --show-error --location \
			--output "$destination/$name.cabal" \
			"https://hackage.haskell.org/package/$name-$version/revision/$revision.cabal" || return 1
		;;
	local:*)
		local path="${source#local:}"
		mkdir -p "$destination"
		tar -C "$repo_root/$path" \
			--exclude=./dist-newstyle --exclude=./.aihc-target \
			-cf - . | tar -C "$destination" -xf - || return 1
		;;
	*)
		echo "Unknown source '$source' for $name."
		return 1
		;;
	esac
}

run_with_timeout() {
	if command -v timeout >/dev/null 2>&1; then
		timeout "$package_timeout" "$@"
	else
		"$@"
	fi
}

# One short line that tells why an install failed: the first compiler error,
# or else the last line of the log. The line goes in a Markdown table, so a
# "|" is escaped and a tab becomes a space.
failure_reason() {
	local status="$1"
	local log="$2"
	local reason
	if [ "$status" -eq 124 ]; then
		echo "timed out after $package_timeout seconds"
		return
	fi
	reason="$(grep -m1 -o 'error: .*' "$log" | sed 's/^error: //' || true)"
	if [ -z "$reason" ]; then
		reason="$(grep -m1 '^aihc: ' "$log" | sed -e 's/^aihc: //' -e 's/^user error (//' || true)"
	fi
	if [ -z "$reason" ]; then
		reason="$(grep -v '^[[:space:]]*$' "$log" | tail -n 1 || true)"
	fi
	reason="$(printf '%s' "$reason" | tr '\t' ' ' | cut -c1-120 | sed 's/|/\\|/g')"
	echo "${reason:--}"
}

fetch_failed=" "
while IFS=$'\t' read -r name version source _depends; do
	echo "Fetching $name-$version from $source"
	log="$log_dir/$name.log"
	if ! fetch_package "$name" "$version" "$source" >>"$log" 2>&1; then
		fetch_failed="$fetch_failed$name "
		echo "  failed to fetch:"
		sed 's/^/  /' "$log"
	fi
done <<<"$packages"

run_stage() {
	# The level is part of the identity of an installed package, so aihc-base is
	# installed at the level of the packages.
	echo "Preparing the $target toolchain at -O$level in $store"
	"$aihc" install core-libs/aihc-base \
		--store "$store" --immutable --target "$target" -O "$level" \
		${runtime_args[@]+"${runtime_args[@]}"} \
		2>&1 | tee "$log_dir/core-libraries.log"

	# The last row is the package that compiles itself.
	root_name="$(tail -n 1 <<<"$packages" | cut -f1)"

	passed=" "
	: >"$report"
	while IFS=$'\t' read -r name version source depends; do
		log="$log_dir/$name.log"
		blocked_by=""
		if [ "$depends" != "-" ]; then
			for dependency in ${depends//,/ }; do
				case "$passed" in
				*" $dependency "*) ;;
				*) blocked_by="$blocked_by${blocked_by:+,}$dependency" ;;
				esac
			done
		fi

		case "$fetch_failed" in
		*" $name "*)
			printf '%s\t%s\tfail\tcould not fetch the source\n' "$name" "$version" >>"$report"
			continue
			;;
		esac

		if [ -n "$blocked_by" ]; then
			echo "Skipping $name-$version, which needs $blocked_by"
			printf '%s\t%s\tblocked\t%s\n' "$name" "$version" "$blocked_by" >>"$report"
			continue
		fi

		status=0
		if [ "$name" = "$root_name" ]; then
			# The last package is the one to compile itself. Its executable is
			# the goal, so it is built rather than installed. The plan of the
			# package list disables the `hackage` flag of aihc, so the build
			# disables it too.
			echo "Building the executable $root_executable of $name-$version"
			run_with_timeout "$aihc" build "$workspace/$name" --executable "$root_executable" \
				--constraint "aihc -hackage" \
				"${output_args[@]}" \
				--store "$store" --build-root "$work_directory/build" --target "$target" -O "$level" \
				${runtime_args[@]+"${runtime_args[@]}"} \
				>>"$log" 2>&1 || status=$?
		else
			echo "Installing $name-$version"
			run_with_timeout "$aihc" install "$workspace/$name" \
				--store "$store" --immutable --target "$target" -O "$level" \
				${runtime_args[@]+"${runtime_args[@]}"} \
				>>"$log" 2>&1 || status=$?
		fi
		if [ "$status" -eq 0 ]; then
			echo "  ok"
			passed="$passed$name "
			printf '%s\t%s\tpass\t-\n' "$name" "$version" >>"$report"
		else
			echo "  failed, last lines of the log:"
			tail -n 20 "$log" | sed 's/^/  /'
			printf '%s\t%s\tfail\t%s\n' "$name" "$version" "$(failure_reason "$status" "$log")" >>"$report"
		fi
	done <<<"$packages"

	total="$(wc -l <"$report" | tr -d ' ')"
	pass_count="$(awk -F'\t' '$3 == "pass"' "$report" | wc -l | tr -d ' ')"
	echo "Installed $pass_count of $total packages of $list_file."
}

run_stage

if [ -n "$bootstrap_dir" ]; then
	if [ "$report" -ef "$bootstrap_dir/stage2.tsv" ]; then
		:
	else
		cp "$report" "$bootstrap_dir/stage2.tsv"
	fi
	if [ "$log_dir" -ef "$bootstrap_dir/stage2-logs" ]; then
		:
	else
		cp -R "$log_dir" "$bootstrap_dir/stage2-logs"
	fi
	if awk -F '\t' '$3 != "pass" { failed = 1 } END { exit failed }' "$report"; then
		:
	else
		echo "The first AIHC build failed." >&2
		exit 1
	fi

	aihc="$bootstrap_dir/stage2/$root_executable"
	# Give the native compiler more space between minor collections.
	runtime_args=(+RTS -A64M -B128M -RTS)
	printf 'stage2\tpass\t0\n' >>"$bootstrap_dir/bootstrap.tsv"
	bootstrap_stage=stage3
	store="$work_directory/stage3-store"
	log_dir="$bootstrap_dir/stage3-logs"
	report="$bootstrap_dir/stage3.tsv"
	output_args=(--output "$bootstrap_dir/stage3")
	mkdir -p "$store" "$log_dir"
	# Remove the first generation of module artifacts before the next build.
	rm -rf "$work_directory/build"
	run_stage
	awk -F '\t' '$3 != "pass" { failed = 1 } END { exit failed }' "$report"
	printf 'stage3\tpass\t0\n' >>"$bootstrap_dir/bootstrap.tsv"
	bootstrap_stage=comparison

	sha256sum "$bootstrap_dir/ghc-aihc" \
		"$bootstrap_dir/stage2/$root_executable" \
		"$bootstrap_dir/stage3/$root_executable" >"$bootstrap_dir/sha256.txt"
	status=0
	cmp "$bootstrap_dir/stage2/$root_executable" "$bootstrap_dir/stage3/$root_executable" \
		>"$bootstrap_dir/aihc-comparison.txt" 2>&1 || status=1
	exit "$status"
fi
