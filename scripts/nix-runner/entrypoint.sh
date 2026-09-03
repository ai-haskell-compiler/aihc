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
