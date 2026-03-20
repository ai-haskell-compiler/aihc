#!/usr/bin/env bash
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/stop-lima-runner.sh [options]

Stop the Lima VM that hosts the optional GitHub Actions runner and remove the
runner registration from GitHub when possible.

Options:
  --repo OWNER/REPO         Repository to de-register from (default: current gh repo)
  --instance-name NAME      Lima instance name
  --runner-name NAME        GitHub runner name
  --delete-instance         Remove the Lima instance after stopping it
  -h, --help                Show this help text

Requirements:
  - `gh` authenticated with admin access to the repository (`repo` scope for classic PATs)
  - `limactl`
  - `python3`
USAGE
}

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$repo_root"

require_cmd() {
	if ! command -v "$1" >/dev/null 2>&1; then
		echo "Required command not found: $1" >&2
		exit 1
	fi
}

sanitize_name() {
	printf '%s' "$1" | tr '[:upper:]' '[:lower:]' | sed 's/[^a-z0-9._-]/-/g; s/--*/-/g; s/^-//; s/-$//'
}

gh_repo() {
	gh repo view --json nameWithOwner --jq '.nameWithOwner'
}

runner_query_json() {
	local repo="$1"
	local runner_name="$2"
	gh api "repos/${repo}/actions/runners?per_page=100&name=${runner_name}"
}

runner_field() {
	local field="$1"
	python3 - "$field" <<'PY'
import json
import sys

field = sys.argv[1]
data = json.load(sys.stdin)
runners = data.get("runners", [])
if not runners:
    raise SystemExit(1)
runner = runners[0]
value = runner.get(field)
if isinstance(value, bool):
    print("true" if value else "false")
elif value is not None:
    print(value)
PY
}

REPO=""
INSTANCE_NAME=""
RUNNER_NAME=""
DELETE_INSTANCE=0

while [ "$#" -gt 0 ]; do
	case "$1" in
	--repo)
		REPO="$2"
		shift 2
		;;
	--instance-name)
		INSTANCE_NAME="$2"
		shift 2
		;;
	--runner-name)
		RUNNER_NAME="$2"
		shift 2
		;;
	--delete-instance)
		DELETE_INSTANCE=1
		shift
		;;
	-h | --help)
		usage
		exit 0
		;;
	*)
		echo "Unknown argument: $1" >&2
		usage >&2
		exit 2
		;;
	esac
done

require_cmd gh
require_cmd limactl
require_cmd python3

gh auth status >/dev/null

if [ -z "$REPO" ]; then
	REPO="$(gh_repo)"
fi

host_tag="$(sanitize_name "${USER:-user}-$(hostname -s 2>/dev/null || hostname)")"
if [ -z "$INSTANCE_NAME" ]; then
	INSTANCE_NAME="$(sanitize_name "aihc-gh-runner-${host_tag}")"
fi
if [ -z "$RUNNER_NAME" ]; then
	RUNNER_NAME="$(sanitize_name "${INSTANCE_NAME}")"
fi

LIMA_INSTANCE_DIR="${LIMA_HOME:-$HOME/.lima}/${INSTANCE_NAME}"
instance_exists=0
if [ -d "$LIMA_INSTANCE_DIR" ]; then
	instance_exists=1
fi

runner_json="$(runner_query_json "$REPO" "$RUNNER_NAME")"
runner_id="$(printf '%s' "$runner_json" | runner_field id || true)"

if [ "$instance_exists" -eq 1 ]; then
	echo "Stopping runner service inside '${INSTANCE_NAME}'..."
	limactl shell "$INSTANCE_NAME" bash -lc '
set -euo pipefail
sudo systemctl stop aihc-github-runner.service >/dev/null 2>&1 || true
if [ -x "$HOME/actions-runner/svc.sh" ]; then
	"$HOME/actions-runner/svc.sh" stop >/dev/null 2>&1 || true
fi
' >/dev/null 2>&1 || true
fi

if [ "$instance_exists" -eq 1 ] && [ -n "$runner_id" ]; then
	remove_token="$(gh api --method POST "repos/${REPO}/actions/runners/remove-token" --jq '.token')"
	echo "Attempting graceful de-registration for '${RUNNER_NAME}'..."
	limactl shell "$INSTANCE_NAME" env REMOVE_TOKEN="$remove_token" bash -lc '
set -euo pipefail
if [ -x "$HOME/actions-runner/config.sh" ]; then
	cd "$HOME/actions-runner"
	./config.sh remove --token "$REMOVE_TOKEN"
fi
' >/dev/null 2>&1 || true
fi

if [ "$instance_exists" -eq 1 ]; then
	echo "Stopping Lima instance '${INSTANCE_NAME}'..."
	limactl stop "$INSTANCE_NAME" >/dev/null 2>&1 || true
fi

runner_json="$(runner_query_json "$REPO" "$RUNNER_NAME")"
runner_id="$(printf '%s' "$runner_json" | runner_field id || true)"
if [ -n "$runner_id" ]; then
	echo "Removing lingering runner registration '${RUNNER_NAME}' (id ${runner_id})..."
	gh api --method DELETE "repos/${REPO}/actions/runners/${runner_id}" >/dev/null
fi

if [ "$DELETE_INSTANCE" -eq 1 ] && [ "$instance_exists" -eq 1 ]; then
	echo "Deleting Lima instance '${INSTANCE_NAME}'..."
	limactl delete -f "$INSTANCE_NAME" >/dev/null
fi

echo "Runner '${RUNNER_NAME}' has been stopped."
