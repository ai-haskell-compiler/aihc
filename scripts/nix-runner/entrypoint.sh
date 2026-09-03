#!/usr/bin/env bash
# Configure and run a single ephemeral GitHub Actions runner.
#
# The runner takes exactly one job and then exits; the pool script starts a
# fresh container for the next job, so no state survives between jobs.
set -euo pipefail

: "${AIHC_REPO_URL:?AIHC_REPO_URL must be set}"
: "${AIHC_RUNNER_TOKEN:?AIHC_RUNNER_TOKEN must be set}"
: "${AIHC_RUNNER_NAME:?AIHC_RUNNER_NAME must be set}"
: "${AIHC_RUNNER_LABELS:?AIHC_RUNNER_LABELS must be set}"

cd /home/runner/actions-runner

if [ ! -S /nix/var/nix/daemon-socket/socket ]; then
	echo "No nix daemon socket at /nix/var/nix/daemon-socket/socket." >&2
	echo "Check the bind mounts on the container." >&2
	exit 1
fi

if ! command -v nix >/dev/null 2>&1; then
	echo "No nix binary on PATH (${PATH})." >&2
	echo "Expected the host store to be bind-mounted at /nix." >&2
	exit 1
fi

# Fail here rather than part-way through a job. `nix store info` supersedes
# `nix store ping`; try both so this works across Nix versions.
if ! nix store info >/dev/null 2>&1 && ! nix store ping >/dev/null 2>&1; then
	echo "Could not reach the host nix-daemon over ${NIX_REMOTE:-<unset>}." >&2
	echo "Diagnostics:" >&2
	ls -l /nix/var/nix/daemon-socket/socket >&2 || true
	nix store info >&2 || true
	echo "The socket must be writable by uid $(id -u) inside this container." >&2
	exit 1
fi

echo "Connected to the host nix-daemon."

./config.sh \
	--unattended \
	--ephemeral \
	--replace \
	--url "$AIHC_REPO_URL" \
	--token "$AIHC_RUNNER_TOKEN" \
	--name "$AIHC_RUNNER_NAME" \
	--labels "$AIHC_RUNNER_LABELS" \
	--work /home/runner/_work

exec ./run.sh
