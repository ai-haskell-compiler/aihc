#!/usr/bin/env bash
# Run a Nix app of the portable Linux release in a Debian 12 container.
#
# Debian 12 has glibc 2.36. An executable that links there runs on each
# distribution with glibc 2.36 or a newer glibc. An aihc executable has
# packed relative relocations (DT_RELR), and glibc 2.36 is the first glibc
# that reads them.
#
# The container mounts /nix read-only, so the app and its tools run from
# the Nix store of the host. The C headers, the C runtime files, and the C
# libraries come from Debian. The container mounts the repository, /tmp,
# and RUNNER_TEMP at the same paths as on the host, so the paths in the
# arguments stay correct. The app runs with the user of the host, so the
# host can remove the files that it writes.
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/release-container.sh APP [ARGUMENT]...

  APP       A Nix app of this flake, such as release-linux-stage
  ARGUMENT  An argument of the app

The CONTAINER_ENGINE variable selects docker (default) or podman.
USAGE
}

if [ "$#" -lt 1 ] || [ "$1" = "--help" ]; then
	usage
	[ "$#" -ge 1 ] && exit 0
	exit 2
fi

app="$1"
shift

engine="${CONTAINER_ENGINE:-docker}"
base_image="docker.io/library/debian:12@sha256:366a6da5efcceffbaa19c560a101b97b87a1a9664eeeac8bc3d24395ddbf14f3"
image="aihc-release-linux:debian-12"

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

# An app is a store path with the derivation in its string context. Build
# the derivation, because the container cannot run Nix.
derivation="$(nix eval --raw ".#apps.x86_64-linux.$app.program" \
	--apply 'program: builtins.head (builtins.attrNames (builtins.getContext program))')"
nix build --no-link "$derivation^out"
program="$(nix eval --raw ".#apps.x86_64-linux.$app.program")"

"$engine" build --quiet --tag "$image" - >/dev/null <<DOCKERFILE
FROM $base_image
RUN apt-get update \\
	&& DEBIAN_FRONTEND=noninteractive apt-get install --yes --no-install-recommends \\
		ca-certificates libc6-dev libgcc-12-dev libstdc++-12-dev zlib1g-dev libffi-dev libncurses-dev \\
	&& rm -rf /var/lib/apt/lists/*
DOCKERFILE

mounts=(--volume /nix:/nix:ro --volume "$repo_root:$repo_root" --volume /tmp:/tmp)
if [ -n "${RUNNER_TEMP:-}" ]; then
	mounts+=(--volume "$RUNNER_TEMP:$RUNNER_TEMP")
fi

user=(--user "$(id -u):$(id -g)")
if [ "$engine" = "podman" ]; then
	user+=(--userns=keep-id)
fi

home="$(mktemp -d /tmp/aihc-release-home.XXXXXX)"
trap 'rm -rf "$home"' EXIT

exec_arguments=(
	run --rm "${user[@]}" "${mounts[@]}"
	--env HOME="$home"
	--workdir "$repo_root"
)
for variable in AIHC GITHUB_STEP_SUMMARY; do
	if [ -n "${!variable:-}" ]; then
		exec_arguments+=(--env "$variable=${!variable}")
	fi
done

"$engine" "${exec_arguments[@]}" "$image" "$program" "$@"
