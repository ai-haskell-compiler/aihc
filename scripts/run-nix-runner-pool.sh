#!/usr/bin/env bash
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/run-nix-runner-pool.sh [options]

Run a foreground pool of sandboxed, ephemeral GitHub Actions runners for
`nix flake check`.

Each pool slot starts a rootless container holding a single-job runner. The
container has no Nix of its own: the host store is bind-mounted read-only and
all builds are performed by the host nix-daemon over its socket. When a job
finishes the runner de-registers, the container is discarded, and the slot
starts a fresh one.

Nothing is installed on the host and no services are created. Press Ctrl-C to
stop the pool; containers are removed and runners de-registered on exit.

Options:
  --repo OWNER/REPO      Repository to register against (default: current gh repo)
  --count N              Number of concurrent runners (default: 4)
  --runner-label LABEL   Custom runner label (default: aihc-nix)
  --name-prefix PREFIX   Runner/container name prefix (default: aihc-nix-<host>)
  --runner-version VER   actions/runner version to bake into the image
                         (default: latest advertised by the repository)
  --image REF            Container image tag to build/use
  --rebuild-image        Rebuild the image even if the tag already exists
  --cpus N               CPU limit per container (default: unlimited)
  --memory SIZE          Memory limit per container, e.g. 8g (default: unlimited)
  --container-cmd CMD    Container runtime (default: podman, else docker)
  -h, --help             Show this help text

Note: --cpus/--memory bound the runner and the job script only. Nix builds run
in the host nix-daemon, so throttle those with `max-jobs`/`cores` in
/etc/nix/nix.conf or via the workflow's `nix flake check --max-jobs` flag.

Requirements:
  - `gh` authenticated with admin access to the repository
  - `podman` (preferred, rootless) or `docker`
  - a running nix-daemon on the host
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

REPO=""
COUNT=4
RUNNER_LABEL="${AIHC_RUNNER_LABEL:-aihc-nix}"
NAME_PREFIX=""
RUNNER_VERSION=""
IMAGE=""
REBUILD_IMAGE=0
CPUS=""
MEMORY=""
CONTAINER_CMD=""

while [ "$#" -gt 0 ]; do
	case "$1" in
	--repo)
		REPO="$2"
		shift 2
		;;
	--count)
		COUNT="$2"
		shift 2
		;;
	--runner-label)
		RUNNER_LABEL="$2"
		shift 2
		;;
	--name-prefix)
		NAME_PREFIX="$2"
		shift 2
		;;
	--runner-version)
		RUNNER_VERSION="$2"
		shift 2
		;;
	--image)
		IMAGE="$2"
		shift 2
		;;
	--rebuild-image)
		REBUILD_IMAGE=1
		shift
		;;
	--cpus)
		CPUS="$2"
		shift 2
		;;
	--memory)
		MEMORY="$2"
		shift 2
		;;
	--container-cmd)
		CONTAINER_CMD="$2"
		shift 2
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

if ! printf '%s' "$COUNT" | grep -Eq '^[1-9][0-9]*$'; then
	echo "--count must be a positive integer, got: $COUNT" >&2
	exit 2
fi

require_cmd gh
require_cmd python3

if [ -z "$CONTAINER_CMD" ]; then
	if command -v podman >/dev/null 2>&1; then
		CONTAINER_CMD=podman
	elif command -v docker >/dev/null 2>&1; then
		CONTAINER_CMD=docker
	else
		echo "Neither podman nor docker was found. Install podman (preferred)." >&2
		exit 1
	fi
fi
require_cmd "$CONTAINER_CMD"

gh auth status >/dev/null

if [ -z "$REPO" ]; then
	REPO="$(gh repo view --json nameWithOwner --jq '.nameWithOwner')"
fi
REPO_URL="https://github.com/${REPO}"

if [ -z "$NAME_PREFIX" ]; then
	NAME_PREFIX="$(sanitize_name "aihc-nix-$(hostname -s 2>/dev/null || hostname)")"
fi

SOCKET_DIR=/nix/var/nix/daemon-socket
SOCKET_PATH="${SOCKET_DIR}/socket"

preflight_nix() {
	if [ ! -d /nix/store ]; then
		echo "No /nix/store on this host. Install Nix before starting the pool." >&2
		exit 1
	fi

	if [ ! -S "$SOCKET_PATH" ]; then
		echo "No nix daemon socket at ${SOCKET_PATH}." >&2
		echo "The pool shares the host daemon; start it before running this script." >&2
		exit 1
	fi

	# Container users are unprivileged and unmapped on the host, so the socket
	# must be world-writable for them to connect.
	local mode
	mode="$(stat -c '%a' "$SOCKET_PATH" 2>/dev/null || echo '')"
	case "$mode" in
	*6 | *7) ;;
	'')
		echo "Warning: could not stat ${SOCKET_PATH}; continuing." >&2
		;;
	*)
		echo "Warning: ${SOCKET_PATH} has mode ${mode}; containers may not be able to connect." >&2
		;;
	esac

	# A trusted user can turn off the build sandbox, which would defeat the
	# isolation this pool relies on.
	local trusted
	trusted="$(nix config show 2>/dev/null | sed -n 's/^trusted-users = //p' || true)"
	if [ -n "$trusted" ]; then
		echo "Note: nix trusted-users = ${trusted}"
		echo "      Anything in that list can disable the Nix build sandbox."
	fi
}

resolve_runner_download() {
	local arch="$1"
	gh api "repos/${REPO}/actions/runners/downloads" | python3 -c '
import json
import sys

target_arch = sys.argv[1]
wanted_version = sys.argv[2]

for entry in json.load(sys.stdin):
    if entry.get("os") != "linux" or entry.get("architecture") != target_arch:
        continue
    version = entry.get("version", "")
    if wanted_version and version != wanted_version:
        continue
    print(entry["download_url"])
    print(version)
    print(entry.get("sha256_checksum") or "")
    raise SystemExit(0)

raise SystemExit(1)
' "$arch" "$RUNNER_VERSION"
}

build_image_if_needed() {
	local download_url="$1"
	local sha256="$2"

	if [ "$REBUILD_IMAGE" -eq 0 ] && "$CONTAINER_CMD" image exists "$IMAGE" >/dev/null 2>&1; then
		echo "Using existing image ${IMAGE}."
		return 0
	fi

	# `docker` has no `image exists`; fall back to inspect.
	if [ "$REBUILD_IMAGE" -eq 0 ] && [ "$CONTAINER_CMD" = docker ] &&
		"$CONTAINER_CMD" image inspect "$IMAGE" >/dev/null 2>&1; then
		echo "Using existing image ${IMAGE}."
		return 0
	fi

	echo "Building image ${IMAGE}..."
	"$CONTAINER_CMD" build \
		--build-arg "RUNNER_URL=${download_url}" \
		--build-arg "RUNNER_SHA256=${sha256}" \
		--tag "$IMAGE" \
		scripts/nix-runner
}

STOP_FILE=""
SLOT_PIDS=()

cleanup() {
	local exit_code=$?
	trap - EXIT INT TERM

	echo
	echo "Stopping runner pool..."

	if [ -n "$STOP_FILE" ]; then
		printf 'stop' >"$STOP_FILE"
	fi

	local slot
	for slot in $(seq 1 "$COUNT"); do
		"$CONTAINER_CMD" rm -f "${NAME_PREFIX}-${slot}" >/dev/null 2>&1 || true
	done

	local pid
	for pid in "${SLOT_PIDS[@]:-}"; do
		[ -n "$pid" ] || continue
		kill "$pid" >/dev/null 2>&1 || true
	done
	wait >/dev/null 2>&1 || true

	deregister_leftovers

	if [ -n "$STOP_FILE" ]; then
		rm -f "$STOP_FILE"
	fi

	echo "Pool stopped."
	exit "$exit_code"
}

# Ephemeral runners de-register themselves after a job, but a container killed
# mid-job leaves an offline registration behind.
deregister_leftovers() {
	local runners_json
	runners_json="$(gh api "repos/${REPO}/actions/runners?per_page=100" 2>/dev/null || true)"
	[ -n "$runners_json" ] || return 0

	local ids
	ids="$(printf '%s' "$runners_json" | python3 -c '
import json
import sys

prefix = sys.argv[1]
data = json.load(sys.stdin)
for runner in data.get("runners", []):
    name = runner.get("name", "")
    if name.startswith(prefix) and runner.get("status") != "online":
        print(runner["id"])
' "$NAME_PREFIX" 2>/dev/null || true)"

	local id
	for id in $ids; do
		echo "Removing stale runner registration ${id}..."
		gh api --method DELETE "repos/${REPO}/actions/runners/${id}" >/dev/null 2>&1 || true
	done
}

run_slot() {
	local slot="$1"
	local container="${NAME_PREFIX}-${slot}"

	while [ ! -s "$STOP_FILE" ]; do
		local token
		if ! token="$(gh api --method POST \
			"repos/${REPO}/actions/runners/registration-token" --jq '.token' 2>/dev/null)"; then
			echo "[slot ${slot}] could not get a registration token; retrying in 30s" >&2
			sleep 30
			continue
		fi

		local run_args=(
			run --rm
			--name "$container"
			--hostname "$container"
			# The job script is sandboxed here; Nix builds are sandboxed by the
			# daemon instead, since they execute on the host.
			--volume /nix:/nix:ro
			--volume "${SOCKET_DIR}:${SOCKET_DIR}:rw"
			--env "AIHC_REPO_URL=${REPO_URL}"
			--env "AIHC_RUNNER_TOKEN=${token}"
			--env "AIHC_RUNNER_NAME=${container}"
			--env "AIHC_RUNNER_LABELS=${RUNNER_LABEL}"
			# Also set in the image, but repeated here so the daemon wiring is
			# visible alongside the mounts it depends on.
			--env NIX_REMOTE=daemon
			--env "NIX_CONFIG=experimental-features = nix-command flakes"
		)

		# Share the host client config (experimental-features, substituters).
		if [ -d /etc/nix ]; then
			run_args+=(--volume /etc/nix:/etc/nix:ro)
		fi

		# Relabelling would rewrite SELinux labels on the host /nix store, so
		# disable enforcement for the mount instead.
		if [ "$CONTAINER_CMD" = podman ]; then
			run_args+=(--security-opt label=disable)
		fi
		if [ -n "$CPUS" ]; then
			run_args+=(--cpus "$CPUS")
		fi
		if [ -n "$MEMORY" ]; then
			run_args+=(--memory "$MEMORY")
		fi

		run_args+=("$IMAGE")

		"$CONTAINER_CMD" "${run_args[@]}" || true

		[ -s "$STOP_FILE" ] && break
		sleep 2
	done
}

preflight_nix

host_arch="$(uname -m)"
case "$host_arch" in
x86_64) runner_arch="x64" ;;
aarch64 | arm64) runner_arch="arm64" ;;
*)
	echo "Unsupported host architecture: ${host_arch}" >&2
	exit 1
	;;
esac

if ! download_info="$(resolve_runner_download "$runner_arch")"; then
	if [ -n "$RUNNER_VERSION" ]; then
		# The downloads API only advertises the current release, so fall back to
		# the canonical release URL when pinning to an older version.
		download_info="$(printf '%s\n%s\n\n' \
			"https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/actions-runner-linux-${runner_arch}-${RUNNER_VERSION}.tar.gz" \
			"$RUNNER_VERSION")"
	else
		echo "Could not resolve an actions/runner download for linux/${runner_arch}." >&2
		exit 1
	fi
fi

download_url="$(printf '%s\n' "$download_info" | sed -n '1p')"
resolved_version="$(printf '%s\n' "$download_info" | sed -n '2p')"
download_sha256="$(printf '%s\n' "$download_info" | sed -n '3p')"

if [ -z "$IMAGE" ]; then
	IMAGE="aihc-nix-runner:${resolved_version:-latest}"
fi

build_image_if_needed "$download_url" "$download_sha256"

STOP_FILE="$(mktemp)"
trap cleanup EXIT INT TERM

echo "Starting ${COUNT} runner(s)."
echo "  Repo:    ${REPO}"
echo "  Label:   ${RUNNER_LABEL}"
echo "  Image:   ${IMAGE}"
echo "  Runtime: ${CONTAINER_CMD}"
echo "  Names:   ${NAME_PREFIX}-1 .. ${NAME_PREFIX}-${COUNT}"
echo
echo "Press Ctrl-C to stop."
echo

for slot in $(seq 1 "$COUNT"); do
	run_slot "$slot" &
	SLOT_PIDS+=("$!")
done

wait
