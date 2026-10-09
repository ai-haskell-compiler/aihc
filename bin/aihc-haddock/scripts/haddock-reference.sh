#!/usr/bin/env bash
# Generate the mainline Haddock reference outputs for one fixture.
#
# Usage: haddock-reference.sh FIXTURE_DIR
#
# Writes FIXTURE_DIR/reference/haddock.json (the --show-interface JSON),
# FIXTURE_DIR/reference/hoogle.txt (the --hoogle database) and
# FIXTURE_DIR/reference/environment.txt (tool versions and the commands).
# Run inside `nix develop` so that the pinned GHC and Haddock are used.
#
# Haddock 2.32.0 writes the --show-interface JSON to standard error, so both
# streams are captured separately and the JSON stream is checked before it is
# kept.
set -euo pipefail

fixture=$1
cabal_file=$(find "$fixture" -maxdepth 1 -name '*.cabal' | head -n 1)
name=$(sed -n 's/^name:[[:space:]]*//p' "$cabal_file" | head -n 1)
version=$(sed -n 's/^version:[[:space:]]*//p' "$cabal_file" | head -n 1)
reference="$fixture/reference"
mkdir -p "$reference"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

mapfile -t sources < <(find "$fixture/src" -name '*.hs' | sort)

# The global GHC package environment must not leak into the reference build.
common=(--optghc=-package-env=- --optghc=-i"$fixture/src" --package-name="$name" --package-version="$version")

if [ -f "$fixture/haddock-options.txt" ]; then
	mapfile -t extra < "$fixture/haddock-options.txt"
	common+=("${extra[@]}")
fi

# Cabal supplies package identities and interfaces for dependency fixtures.
if [ -d "$fixture/dependencies" ]; then
	fixture_absolute=$(cd "$fixture" && pwd)
	{
		printf 'packages:\n'
		find "$fixture_absolute" -name '*.cabal' | sort | sed 's/^/  /'
	} > "$work/cabal.project"
	(cd "$work" && cabal haddock all --offline --enable-documentation --haddock-hoogle) \
		>"$work/hoogle.stdout" 2>"$work/hoogle.stderr" || { cat "$work/hoogle.stdout" "$work/hoogle.stderr" >&2; exit 1; }
	interface=$(find "$work/dist-newstyle" -name "$name.haddock" | head -n 1)
	cp "$interface" "$work/interface.haddock"
	mkdir -p "$work/hoogle"
	cp "$(dirname "$interface")/$name.txt" "$work/hoogle/$name.txt"
else
	haddock "${common[@]}" --dump-interface="$work/interface.haddock" --hoogle -o "$work/hoogle" "${sources[@]}" \
		>"$work/hoogle.stdout" 2>"$work/hoogle.stderr" || { cat "$work/hoogle.stdout" "$work/hoogle.stderr" >&2; exit 1; }
fi
haddock "${common[@]}" --show-interface="$work/interface.haddock" \
	>"$work/show.stdout" 2>"$work/show.stderr"

if [ -s "$work/show.stderr" ] && python3 -c 'import json,sys; json.load(open(sys.argv[1]))' "$work/show.stderr"; then
	json_stream=stderr
elif python3 -c 'import json,sys; json.load(open(sys.argv[1]))' "$work/show.stdout"; then
	json_stream=stdout
else
	echo "no JSON interface on either stream" >&2
	cat "$work/show.stdout" "$work/show.stderr" >&2
	exit 1
fi

python3 -c 'import json,sys; json.dump(json.load(open(sys.argv[1])), open(sys.argv[2], "w"), indent=1, sort_keys=True); open(sys.argv[2], "a").write("\n")' \
	"$work/show.$json_stream" "$reference/haddock.json"
cp "$work/hoogle/$name.txt" "$reference/hoogle.txt"

{
	echo "haddock: $(haddock --version | head -n 1)"
	echo "ghc: $(ghc --numeric-version)"
	echo "json-stream: $json_stream"
	echo "sources: ${sources[*]#"$fixture"/}"
	if [ -d "$fixture/dependencies" ]; then
		echo "command: cabal haddock all --offline --enable-documentation --haddock-hoogle"
		echo "project:"
		sed "s#$fixture_absolute#.#g; s/^/  /" "$work/cabal.project"
	else
		echo "command: haddock ${common[*]} --dump-interface=interface.haddock --hoogle -o hoogle SOURCES"
	fi
	echo "command: haddock ${common[*]} --show-interface=interface.haddock"
	if [ -s "$work/hoogle.stderr" ]; then
		echo "diagnostics:"
		sed 's/^/  /' "$work/hoogle.stderr"
	fi
} | sed "s#$fixture/##g; s#$work#WORK#g" >"$reference/environment.txt"
