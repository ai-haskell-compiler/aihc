#!/usr/bin/env bash
set -euo pipefail

usage() {
	cat <<'USAGE'
Usage: scripts/start-lima-runner.sh [options]

Start or recreate a Lima VM that hosts an optional repository-scoped GitHub Actions runner.

Options:
  --repo OWNER/REPO         Repository to register against (default: current gh repo)
  --instance-name NAME      Lima instance name
  --runner-name NAME        GitHub runner name
  --runner-label LABEL      Custom runner label (default: aihc-lima)
  --cpus N                  VM CPU count (default: 4)
  --memory GiB              VM memory in GiB (default: 8)
  --disk GiB                VM disk size in GiB (default: 50)
  --vm-type TYPE            Lima VM type, for example qemu or vz
  --force-recreate          Delete any existing Lima instance before starting
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
	python3 -c '
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
' "$field"
}

wait_for_runner_online() {
	local repo="$1"
	local runner_name="$2"
	local max_attempts=30
	local attempt=1

	while [ "$attempt" -le "$max_attempts" ]; do
		local status=""
		local busy=""
		local query_json
		query_json="$(runner_query_json "$repo" "$runner_name")"
		status="$(printf '%s' "$query_json" | runner_field status || true)"
		busy="$(printf '%s' "$query_json" | runner_field busy || true)"

		if [ "$status" = "online" ] && [ "$busy" = "false" ]; then
			return 0
		fi

		sleep 2
		attempt=$((attempt + 1))
	done

	return 1
}

REPO=""
INSTANCE_NAME=""
RUNNER_NAME=""
RUNNER_LABEL="${RUNNER_LABEL:-aihc-lima}"
CPUS="${LIMA_RUNNER_CPUS:-4}"
MEMORY_GIB="${LIMA_RUNNER_MEMORY_GIB:-8}"
DISK_GIB="${LIMA_RUNNER_DISK_GIB:-50}"
VM_TYPE="${LIMA_RUNNER_VM_TYPE:-}"
FORCE_RECREATE=0

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
	--runner-label)
		RUNNER_LABEL="$2"
		shift 2
		;;
	--cpus)
		CPUS="$2"
		shift 2
		;;
	--memory)
		MEMORY_GIB="$2"
		shift 2
		;;
	--disk)
		DISK_GIB="$2"
		shift 2
		;;
	--vm-type)
		VM_TYPE="$2"
		shift 2
		;;
	--force-recreate)
		FORCE_RECREATE=1
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
RUNNER_ADMIN_TOKEN="$(gh auth token)"

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

RUNNER_URL="https://github.com/${REPO}"
LIMA_INSTANCE_DIR="${LIMA_HOME:-$HOME/.lima}/${INSTANCE_NAME}"

if [ "$FORCE_RECREATE" -eq 1 ] && [ -d "$LIMA_INSTANCE_DIR" ]; then
	echo "Deleting existing Lima instance '${INSTANCE_NAME}'..."
	limactl delete -f "$INSTANCE_NAME"
fi

existing_json="$(runner_query_json "$REPO" "$RUNNER_NAME")"
existing_id="$(printf '%s' "$existing_json" | runner_field id || true)"
existing_status="$(printf '%s' "$existing_json" | runner_field status || true)"
existing_busy="$(printf '%s' "$existing_json" | runner_field busy || true)"

if [ -n "$existing_id" ] && [ "$existing_status" = "online" ] && [ "$existing_busy" = "true" ]; then
	echo "Runner '${RUNNER_NAME}' is already online and busy; refusing to replace it." >&2
	exit 1
fi

if [ -n "$existing_id" ] && [ "$existing_status" != "online" ]; then
	echo "Deleting stale runner registration '${RUNNER_NAME}' (id ${existing_id})..."
	gh api --method DELETE "repos/${REPO}/actions/runners/${existing_id}" >/dev/null
fi

if [ -d "$LIMA_INSTANCE_DIR" ]; then
	echo "Starting existing Lima instance '${INSTANCE_NAME}'..."
	limactl -y start "$INSTANCE_NAME"
else
	echo "Creating Lima instance '${INSTANCE_NAME}'..."
	start_cmd=(
		limactl
		-y
		start
		--name="$INSTANCE_NAME"
		--cpus="$CPUS"
		--memory="$MEMORY_GIB"
		--disk="$DISK_GIB"
	)
	if [ -n "$VM_TYPE" ]; then
		start_cmd+=("--vm-type=$VM_TYPE")
	fi
	start_cmd+=("template://default")
	"${start_cmd[@]}"
fi

guest_arch="$(limactl shell "$INSTANCE_NAME" uname -m | tr -d '\r\n')"
case "$guest_arch" in
x86_64) runner_arch="x64" ;;
aarch64 | arm64) runner_arch="arm64" ;;
*)
	echo "Unsupported guest architecture: ${guest_arch}" >&2
	exit 1
	;;
esac

download_url="$(gh api "repos/${REPO}/actions/runners/downloads" | python3 -c '
import json
import sys

target_arch = sys.argv[1]
for entry in json.load(sys.stdin):
    if entry.get("os") == "linux" and entry.get("architecture") == target_arch:
        print(entry["download_url"])
        raise SystemExit(0)
raise SystemExit(1)
' "$runner_arch")"

echo "Configuring runner '${RUNNER_NAME}' with label '${RUNNER_LABEL}'..."
limactl shell "$INSTANCE_NAME" env \
	DOWNLOAD_URL="$download_url" \
	REPO="$REPO" \
	RUNNER_LABEL="$RUNNER_LABEL" \
	RUNNER_NAME="$RUNNER_NAME" \
	RUNNER_ADMIN_TOKEN="$RUNNER_ADMIN_TOKEN" \
	RUNNER_URL="$RUNNER_URL" \
	bash -lc '
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
export NEEDRESTART_MODE=a

runner_user="$(id -un)"
runner_home="$HOME"
runner_env_file="/etc/aihc-github-runner.env"

sudo apt-get update
sudo apt-get install -y awscli curl git tar locales python3

# Configure UTF-8 locale for proper Unicode handling
sudo sed -i "s/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/" /etc/locale.gen
sudo locale-gen en_US.UTF-8
sudo update-locale LANG=en_US.UTF-8

sudo systemctl stop aihc-github-runner.service >/dev/null 2>&1 || true
sudo systemctl disable aihc-github-runner.service >/dev/null 2>&1 || true
sudo rm -f /etc/systemd/system/aihc-github-runner.service
sudo systemctl daemon-reload

rm -rf "$runner_home/actions-runner"
mkdir -p "$runner_home/actions-runner"
cd "$runner_home/actions-runner"

curl -fsSL "$DOWNLOAD_URL" -o runner.tar.gz
tar xzf runner.tar.gz
rm -f runner.tar.gz

sudo ./bin/installdependencies.sh

mkdir -p "$runner_home/actions-runner-hooks"
cat >"$runner_home/actions-runner-hooks/prepare-runner.sh" <<EOF
#!/usr/bin/env bash
set -euo pipefail

runner_dir="\$HOME/actions-runner"

# Clear per-job state before the runner accepts another assignment.
rm -rf "\$runner_dir/_work" "\$runner_dir/_diag/pages"
mkdir -p "\$runner_dir/_work" "\$runner_dir/_diag/pages"
EOF
chmod +x "$runner_home/actions-runner-hooks/prepare-runner.sh"

cat >"$runner_home/actions-runner-hooks/register-and-run.sh" <<EOF
#!/usr/bin/env bash
set -euo pipefail

source "${runner_env_file}"
export HOME="${RUNNER_HOME}"

api_token() {
	local endpoint="\$1"
	curl -fsSL --request POST \
		-H "Accept: application/vnd.github+json" \
		-H "Authorization: Bearer \${RUNNER_ADMIN_TOKEN}" \
		-H "X-GitHub-Api-Version: 2022-11-28" \
		"https://api.github.com/repos/\${REPO}/actions/runners/\${endpoint}"
}

extract_token() {
	python3 -c "import json, sys; print(json.load(sys.stdin)['token'])"
}

cleanup_registration() {
	if [ -f "${runner_home}/actions-runner/.runner" ]; then
		remove_token="\$(api_token remove-token | extract_token)"
		sudo -u "\${RUNNER_USER}" env HOME="\${RUNNER_HOME}" \
			"\${RUNNER_HOME}/actions-runner/config.sh" remove --token "\${remove_token}" >/dev/null 2>&1 || true
	fi
	rm -f \
		"${runner_home}/actions-runner/.runner" \
		"${runner_home}/actions-runner/.credentials" \
		"${runner_home}/actions-runner/.credentials_rsaparams"
}

trap cleanup_registration EXIT

"${runner_home}/actions-runner-hooks/prepare-runner.sh"
registration_token="\$(api_token registration-token | extract_token)"

cleanup_registration

sudo -u "\${RUNNER_USER}" env HOME="\${RUNNER_HOME}" \
	"\${RUNNER_HOME}/actions-runner/config.sh" \
		--unattended \
		--ephemeral \
		--replace \
		--url "\${RUNNER_URL}" \
		--token "\${registration_token}" \
		--name "\${RUNNER_NAME}" \
		--labels "\${RUNNER_LABEL}" \
		--work "_work"

sudo -u "\${RUNNER_USER}" env HOME="\${RUNNER_HOME}" \
	"\${RUNNER_HOME}/actions-runner/run.sh" --once
EOF
chmod +x "$runner_home/actions-runner-hooks/register-and-run.sh"

cat >"$runner_home/actions-runner/.env" <<EOF
ACTIONS_RUNNER_HOOK_JOB_STARTED=${runner_home}/actions-runner-hooks/prepare-runner.sh
EOF

sudo tee "$runner_env_file" >/dev/null <<EOF
REPO=${REPO}
RUNNER_ADMIN_TOKEN=${RUNNER_ADMIN_TOKEN}
RUNNER_HOME=${runner_home}
RUNNER_LABEL=${RUNNER_LABEL}
RUNNER_NAME=${RUNNER_NAME}
RUNNER_URL=${RUNNER_URL}
RUNNER_USER=${runner_user}
EOF
sudo chmod 600 "$runner_env_file"

sudo tee /etc/systemd/system/aihc-github-runner.service >/dev/null <<EOF
[Unit]
Description=GitHub Actions runner for aihc
After=network-online.target
Wants=network-online.target

[Service]
WorkingDirectory=${runner_home}/actions-runner
ExecStart=${runner_home}/actions-runner-hooks/register-and-run.sh
Restart=always
RestartSec=5
KillMode=process
Environment=HOME=${runner_home}
Environment=LANG=en_US.UTF-8
Environment=LC_ALL=en_US.UTF-8

[Install]
WantedBy=multi-user.target
EOF

sudo systemctl daemon-reload
sudo systemctl enable --now aihc-github-runner.service
'

if ! wait_for_runner_online "$REPO" "$RUNNER_NAME"; then
	echo "Runner '${RUNNER_NAME}' did not come online in time." >&2
	limactl shell "$INSTANCE_NAME" bash -lc '
set -euo pipefail
sudo systemctl status aihc-github-runner.service --no-pager -l || true
echo ---
sudo journalctl -u aihc-github-runner.service -n 80 --no-pager || true
' >&2 || true
	exit 1
fi

echo "Runner '${RUNNER_NAME}' is online."
echo "  Repo:     ${REPO}"
echo "  Instance: ${INSTANCE_NAME}"
echo "  Label:    ${RUNNER_LABEL}"
