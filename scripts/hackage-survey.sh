#!/usr/bin/env bash
# Add new Hackage packages to the survey of docs/hackage-survey.md. The script
# compiles each new package with GHC first. If GHC compiles it, the script
# compiles it with aihc too.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/hackage-survey.sh [OPTION]... [PACKAGE]...

  --output FILE   Read the survey from FILE and write it back
                  (default: docs/hackage-survey.md)
  --count N       Survey the N most downloaded Hackage packages that are not
                  in the survey (default: 10). PACKAGE arguments replace this
                  list. A PACKAGE is a name or NAME-VERSION.
  --target TARGET Compile with aihc for TARGET (default: llvm)
  -O LEVEL        Compile with aihc at optimization level LEVEL: 0, 1, 2 or s
                  (default: 0)
  --store DIR     Use DIR as the aihc package store
                  (default: a temporary directory)
  --log-dir DIR   Keep the GHC and aihc logs of each package in DIR
  --timeout SECONDS
                  Stop one compile after SECONDS (default: 1800)
  --help          Show this message

The aihc executable is taken from $AIHC, and defaults to `aihc` on PATH.
It runs with the GHC runtime options of $AIHC_GHCRTS. GHC and cabal are taken from PATH. The survey file has two tables. The first
table lists the packages that compile with GHC, with the aihc result. The
second table lists the packages that do not compile with GHC, with one line
of the error. A package that is in a table is not compiled again.
USAGE
}

output=""
count="10"
target="llvm"
level="0"
store=""
log_dir=""
compile_timeout="1800"
requested=()

while [ "$#" -gt 0 ]; do
	case "$1" in
	--output)
		output="${2:?--output needs a file}"
		shift 2
		;;
	--count)
		count="${2:?--count needs a number}"
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
	--timeout)
		compile_timeout="${2:?--timeout needs a number of seconds}"
		shift 2
		;;
	--help)
		usage
		exit 0
		;;
	--)
		shift
		requested+=("$@")
		break
		;;
	-*)
		usage >&2
		exit 2
		;;
	*)
		requested+=("$1")
		shift
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
output="${output:-docs/hackage-survey.md}"

work_directory="$(mktemp -d)"
trap 'rm -rf "$work_directory"' EXIT

sources="$work_directory/sources"
mkdir -p "$sources"
if [ -z "$log_dir" ]; then
	log_dir="$work_directory/logs"
fi
mkdir -p "$log_dir"
if [ -z "$store" ]; then
	store="$work_directory/store"
fi
mkdir -p "$store"

# The rows of the two tables, as tab separated lines. The GHC table has the
# columns "name version aihc error". The other table has "name version error".
ghc_rows="$work_directory/ghc.tsv"
no_ghc_rows="$work_directory/no-ghc.tsv"
: >"$ghc_rows"
: >"$no_ghc_rows"

# Read the rows of each table of the survey. The heading before a table tells
# which table it is. The header row and its separator are not rows. An
# escaped "\|" in an error is not a column separator.
if [ -f "$output" ]; then
	awk -v ghc="$ghc_rows" -v no_ghc="$no_ghc_rows" '
		function trim(s) {
			gsub(/^[ \t]+|[ \t]+$/, "", s)
			return s
		}
		# Split a table row at its "|" separators into column, and give the
		# number of fields. Column 2 is the first cell, as with -F.
		function split_row(row, column,    i, n) {
			gsub(/\\\|/, "\001", row)
			n = split(row, column, "|")
			for (i = 1; i <= n; i++) {
				gsub(/\001/, "\\|", column[i])
			}
			return n
		}
		/^## / {
			table = ""
			if ($0 ~ /do not compile with GHC/) {
				table = no_ghc
			} else if ($0 ~ /compile with GHC/) {
				table = ghc
			}
			next
		}
		/^\|/ && table != "" {
			split_row($0, cell)
			name = trim(cell[2])
			if (name == "Package" || name ~ /^-+$/) {
				next
			}
			if (table == ghc) {
				print name "\t" trim(cell[3]) "\t" trim(cell[4]) "\t" trim(cell[5]) >>table
			} else {
				print name "\t" trim(cell[3]) "\t" trim(cell[4]) >>table
			}
		}
	' "$output"
fi

is_known() {
	cut -f1 "$ghc_rows" "$no_ghc_rows" | grep -Fxq -- "$1"
}

# The packages that come with GHC and that cabal cannot build again. A survey
# of them tells nothing, so the download list skips them.
ghc_only_packages=" base ghc ghc-bignum ghc-boot ghc-boot-th ghc-experimental ghc-heap ghc-internal ghc-prim ghci integer-gmp rts system-cxx-std-lib template-haskell "

# The packages to survey, one "name" or "name-version" per line.
packages=()
if [ "${#requested[@]}" -gt 0 ]; then
	for package in "${requested[@]}"; do
		name="$(sed -E 's/-[0-9]+(\.[0-9]+)*$//' <<<"$package")"
		if is_known "$name"; then
			echo "Skipping $name, which is in $output."
			continue
		fi
		packages+=("$package")
	done
else
	echo "Asking Hackage for the download list..."
	download_list="$work_directory/top.json"
	curl --fail --silent --show-error --location --retry 5 --retry-all-errors \
		--header 'Accept: application/json' \
		--output "$download_list" \
		"https://hackage.haskell.org/packages/top"
	# The list is sorted by downloads. Each entry has a "packageName" field.
	while read -r name && [ "${#packages[@]}" -lt "$count" ]; do
		case "$ghc_only_packages" in
		*" $name "*) continue ;;
		esac
		if is_known "$name"; then
			continue
		fi
		packages+=("$name")
	done < <(grep -o '"packageName":"[^"]*"' "$download_list" | cut -d'"' -f4)
fi

if [ "${#packages[@]}" -eq 0 ]; then
	echo "No new packages to survey."
	exit 0
fi

# The directory where aihc keeps the Hackage sources it downloads.
hackage_cache="${XDG_CACHE_HOME:-$HOME/.cache}/aihc/hackage"

run_with_timeout() {
	if command -v timeout >/dev/null 2>&1; then
		timeout "$compile_timeout" "$@"
	else
		"$@"
	fi
}

# One short line that tells why a compile failed: the first error line of the
# log, or else the last line of the log. GHC puts the message on the line
# after "error:", so that line is added when the error line has no message.
# The line goes in a Markdown table, so a "|" is escaped and a tab becomes a
# space. A path into the Hackage cache of aihc loses its prefix.
failure_reason() {
	local status="$1"
	local log="$2"
	local reason conflict
	if [ "$status" -eq 124 ]; then
		echo "timed out after $compile_timeout seconds"
		return
	fi
	reason="$(
		awk '
			more && /[^[:space:]]/ {
				line = $0
				gsub(/^[ \t]+|[ \t]+$/, "", line)
				print " " line
				exit
			}
			/[Ee]rror:/ && !found {
				found = 1
				line = $0
				gsub(/^[ \t]+|[ \t]+$/, "", line)
				printf "%s", line
				# The message is on the next line when nothing but an
				# error code follows "error:".
				rest = line
				sub(/^.*[Ee]rror:/, "", rest)
				gsub(/^[ \t]+|[ \t]+$/, "", rest)
				if (rest == "" || rest ~ /^\[[^]]*\]$/) {
					more = 1
				}
			}
			END { if (found && !more) print "" }
		' "$log"
	)"
	# A cabal solver failure names the conflict some lines after the error.
	case "$reason" in
	*"Could not resolve dependencies:")
		conflict="$(grep -m1 -o '(conflict: [^)]*)' "$log" || true)"
		reason="$reason${conflict:+ $conflict}"
		;;
	esac
	if [ -z "$reason" ]; then
		reason="$(grep -m1 '^aihc: ' "$log" | sed -e 's/^aihc: //' -e 's/^user error (//' || true)"
	fi
	if [ -z "$reason" ]; then
		reason="$(grep -v '^[[:space:]]*$' "$log" | tail -n 1 || true)"
	fi
	reason="$(printf '%s' "$reason" | sed -e "s#$work_directory/##g" -e "s#$hackage_cache/##g" | tr '\t' ' ' | cut -c1-160 | sed 's/|/\\|/g')"
	echo "${reason:--}"
}

echo "Updating the cabal package index..."
cabal update -v0

# Compile one package with GHC and then with aihc, and add its row to the
# table that gives its result.
survey_package() {
	local package="$1"
	local name version status source_directory ghc_log aihc_log
	name="$(sed -E 's/-[0-9]+(\.[0-9]+)*$//' <<<"$package")"
	ghc_log="$log_dir/$name.ghc.log"
	aihc_log="$log_dir/$name.aihc.log"
	: >"$ghc_log"

	echo "Fetching $package"
	if ! cabal get "$package" --destdir "$sources" >>"$ghc_log" 2>&1; then
		echo "  failed to fetch:"
		tail -n 5 "$ghc_log" | sed 's/^/  /'
		printf '%s\t%s\t%s\n' "$name" "-" "$(failure_reason 1 "$ghc_log")" >>"$no_ghc_rows"
		return
	fi
	source_directory="$(find "$sources" -mindepth 1 -maxdepth 1 -type d -name "$name-[0-9]*" | head -n 1)"
	if [ -z "$source_directory" ]; then
		echo "  failed to fetch: no $name directory after cabal get"
		printf '%s\t%s\t%s\n' "$name" "-" "no $name directory after cabal get" >>"$no_ghc_rows"
		return
	fi
	version="${source_directory##*/"$name"-}"

	echo "Compiling $name-$version with GHC"
	status=0
	(cd "$source_directory" && run_with_timeout cabal build -v1 \
		--disable-tests --disable-benchmarks --disable-documentation) \
		>>"$ghc_log" 2>&1 || status=$?
	if [ "$status" -ne 0 ]; then
		echo "  failed, last lines of the log:"
		tail -n 10 "$ghc_log" | sed 's/^/  /'
		printf '%s\t%s\t%s\n' "$name" "$version" "$(failure_reason "$status" "$ghc_log")" >>"$no_ghc_rows"
		rm -rf "$source_directory"
		return
	fi
	echo "  ok"
	rm -rf "$source_directory"

	echo "Compiling $name-$version with aihc"
	status=0
	# aihc finds its core libraries from the repository root. The option
	# --ignore-project keeps it from the cabal.project and aihc.lock of
	# this repository.
	GHCRTS="${AIHC_GHCRTS:-}" run_with_timeout "$aihc" install "$name-$version" --ignore-project \
		--store "$store" --lint --target "$target" -O "$level" \
		>"$aihc_log" 2>&1 || status=$?
	if [ "$status" -eq 0 ]; then
		echo "  ok"
		printf '%s\t%s\tpass\t\n' "$name" "$version" >>"$ghc_rows"
	else
		echo "  failed, last lines of the log:"
		tail -n 10 "$aihc_log" | sed 's/^/  /'
		printf '%s\t%s\tfail\t%s\n' "$name" "$version" "$(failure_reason "$status" "$aihc_log")" >>"$ghc_rows"
	fi
}

# Sort the rows of each table by package name and write the survey file.
write_survey() {
	local name version aihc_status error
	sort -t$'\t' -k1,1 -o "$ghc_rows" "$ghc_rows"
	sort -t$'\t' -k1,1 -o "$no_ghc_rows" "$no_ghc_rows"

	ghc_count="$(wc -l <"$ghc_rows" | tr -d ' ')"
	aihc_count="$(awk -F'\t' '$3 == "pass"' "$ghc_rows" | wc -l | tr -d ' ')"
	no_ghc_count="$(wc -l <"$no_ghc_rows" | tr -d ' ')"

	mkdir -p "$(dirname "$output")"
	{
		cat <<-'HEADER'
	# Hackage survey

	This file tells which Hackage packages compile with GHC and with aihc.
	`scripts/hackage-survey.sh` adds packages to the two tables below.
	On each run, the script takes the most downloaded Hackage packages that are
	not in the tables. For each new package, the script compiles the latest
	release with `cabal build` and GHC. If GHC compiles it, the script compiles
	the same release with `aihc install NAME-VERSION --lint`.

	Run the script with `nix run .#hackage-survey`. Give the names of packages
	as arguments to survey those packages instead of the download list.
	The script does not compile a package that is in a table again. To compile
	a package again, remove its row.

	HEADER
		echo "## Summary"
		echo
		echo "- Packages that compile with GHC: $ghc_count"
		echo "- Packages that compile with aihc: $aihc_count"
		echo "- Packages that do not compile with GHC: $no_ghc_count"
		echo
		echo "## Packages that compile with GHC"
		echo
		echo "| Package | Version | AIHC | Error |"
		echo "| ------- | ------- | ---- | ----- |"
		while IFS=$'\t' read -r name version aihc_status error; do
			echo "| $name | $version | $aihc_status | $error |"
		done <"$ghc_rows"
		echo
		echo "## Packages that do not compile with GHC"
		echo
		echo "| Package | Version | Error |"
		echo "| ------- | ------- | ----- |"
		while IFS=$'\t' read -r name version error; do
			echo "| $name | $version | $error |"
		done <"$no_ghc_rows"
	} >"$output"
}

# The file is written after each package, so a stopped run keeps its results.
for package in "${packages[@]}"; do
	survey_package "$package"
	write_survey
done

echo "Wrote $output. GHC pass: $ghc_count. aihc pass: $aihc_count. GHC fail: $no_ghc_count."
