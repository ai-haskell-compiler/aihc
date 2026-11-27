#!/usr/bin/env bash
# Write the Homebrew formula of a published release of aihc to stdout.
# The formula installs the release executable for macOS on Apple silicon
# and for Linux on AMD64. The SHA-256 sums come from the SHA256SUMS file
# of the release.
#
# aihc runs clang to compile and link. On macOS, the Xcode Command Line
# Tools supply clang. On Linux, the system package manager must supply
# clang and the C library. The formula does not install a toolchain.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/homebrew-formula.sh --version VERSION --sums FILE

  --version VERSION  The release version, for example 0.3.0
  --sums FILE        The SHA256SUMS file of the release
  --help             Show this message
USAGE
}

version=""
sums=""

while [ "$#" -gt 0 ]; do
	case "$1" in
	--version)
		version="${2:?--version needs a version}"
		shift 2
		;;
	--sums)
		sums="${2:?--sums needs a file}"
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

if [ -z "$version" ] || [ -z "$sums" ]; then
	usage >&2
	exit 2
fi

sum_of() {
	local file="aihc-$version-$1.tar.gz"
	local sum
	sum="$(awk -v file="$file" '$2 == file || $2 == "*" file {print $1}' "$sums")"
	if ! [[ "$sum" =~ ^[0-9a-f]{64}$ ]]; then
		echo "$sums has no SHA-256 sum for $file." >&2
		exit 1
	fi
	echo "$sum"
}

apple_sum="$(sum_of apple-arm64)"
linux_sum="$(sum_of linux-amd64)"
base="https://github.com/ai-haskell-compiler/aihc/releases/download/v$version"

cat <<FORMULA
# This file comes from scripts/homebrew-formula.sh in
# https://github.com/ai-haskell-compiler/aihc. Do not edit it.
class Aihc < Formula
  desc "Haskell compiler that AI models write"
  homepage "https://aihc.app/"
  license "Unlicense"

  on_macos do
    on_arm do
      url "$base/aihc-$version-apple-arm64.tar.gz"
      sha256 "$apple_sum"
    end
  end

  on_linux do
    on_intel do
      url "$base/aihc-$version-linux-amd64.tar.gz"
      sha256 "$linux_sum"
    end
  end

  def install
    bin.install "aihc"
  end

  def caveats
    on_linux do
      <<~EOS
        aihc runs clang to compile and link. Install clang, binutils and the
        C library headers with the package manager of your system.
      EOS
    end
  end

  test do
    (testpath/"Main.hs").write <<~HASKELL
      main :: IO ()
      main = putStrLn "Hello from aihc"
    HASKELL
    system bin/"aihc", "build", "Main.hs", "-o", "hello"
    assert_equal "Hello from aihc\\n", shell_output("./hello")
  end
end
FORMULA
