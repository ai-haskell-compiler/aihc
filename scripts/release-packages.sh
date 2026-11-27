#!/usr/bin/env bash
# Make the Debian and RPM packages of aihc from a Linux release archive.
# packaging/nfpm.yaml gives the contents and the dependencies of the
# packages. The packages contain the same executable as the archive.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/release-packages.sh --archive FILE --version VERSION --output DIR

  --archive FILE     The release archive aihc-VERSION-linux-ARCH.tar.gz
  --version VERSION  The release version, for example 0.3.0 or 0.3.0-abc1234
  --output DIR       Write the .deb and .rpm files to DIR
  --help             Show this message
USAGE
}

archive=""
version=""
output=""

while [ "$#" -gt 0 ]; do
	case "$1" in
	--archive)
		archive="${2:?--archive needs a file}"
		shift 2
		;;
	--version)
		version="${2:?--version needs a version}"
		shift 2
		;;
	--output)
		output="${2:?--output needs a directory}"
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

if [ -z "$archive" ] || [ -z "$version" ] || [ -z "$output" ]; then
	usage >&2
	exit 2
fi

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
name="$(basename "$archive" .tar.gz)"
case "$name" in
aihc-"$version"-linux-amd64) arch=amd64 ;;
*)
	echo "The archive $archive is not aihc-$version-linux-amd64.tar.gz." >&2
	exit 1
	;;
esac

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
tar -C "$work" -xzf "$archive"
if [ ! -x "$work/$name/aihc" ]; then
	echo "The archive $archive has no executable $name/aihc." >&2
	exit 1
fi

mkdir -p "$output"
export AIHC_PACKAGE_ARCH="$arch"
export AIHC_PACKAGE_VERSION="$version"
output="$(cd "$output" && pwd)"
cd "$work/$name"
for packager in deb rpm; do
	nfpm package \
		--config "$repo_root/packaging/nfpm.yaml" \
		--packager "$packager" \
		--target "$output"
done
