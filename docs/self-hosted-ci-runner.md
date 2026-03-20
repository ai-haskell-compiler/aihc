# Optional Self-Hosted CI Runner

This repository can use an optional Lima-backed self-hosted GitHub Actions runner.
If no matching self-hosted runner is online and idle, workflows fall back to a
hosted runner instead of queueing indefinitely on `self-hosted`.

## How CI Chooses a Runner

The workflows in [`.github/workflows/nix-flake-check.yml`](../.github/workflows/nix-flake-check.yml)
and [`.github/workflows/generated-reports-update.yml`](../.github/workflows/generated-reports-update.yml)
start with a small `select-runner` job on `ubuntu-24.04`.

That job:

- checks whether a repository self-hosted runner with the configured custom label is online and idle,
- only enables self-hosted execution for trusted contexts,
- falls back to a hosted runner when the admin token is missing or the probe fails.

The self-hosted runner label defaults to `aihc-lima`.

## Repository Configuration

The workflow probe first tries:

- `SELF_HOSTED_RUNNER_ADMIN_TOKEN`

If that secret is not set, it falls back to:

- `AUTOMATION_PR_TOKEN`

The token is used only by the workflow probe job. The default workflow
`GITHUB_TOKEN` cannot call the repository self-hosted runner admin endpoints.

For the most predictable setup, `SELF_HOSTED_RUNNER_ADMIN_TOKEN` should be a
classic PAT with `repo` scope and repository admin access. Reusing
`AUTOMATION_PR_TOKEN` can also work if that token is accepted by the runner API.

Optional repository variables:

- `CI_SELF_HOSTED_RUNNER_LABEL`: defaults to `aihc-lima`.
- `CI_HOSTED_FALLBACK_RUNNER`: defaults to `blacksmith-4vcpu-ubuntu-2404`.

If you want to fall back to a GitHub-hosted runner instead of Blacksmith, set:

```text
CI_HOSTED_FALLBACK_RUNNER=ubuntu-24.04
```

## Local Setup

Requirements on the host:

- `gh`
- `limactl`
- `python3`

`gh` must be authenticated with a token that can administer self-hosted runners
for this repository.

Start the runner:

```bash
scripts/start-lima-runner.sh
```

Stop the runner and de-register it from GitHub:

```bash
scripts/stop-lima-runner.sh
```

Stop and delete the Lima VM completely:

```bash
scripts/stop-lima-runner.sh --delete-instance
```

Useful overrides:

```bash
scripts/start-lima-runner.sh \
  --runner-label aihc-lima \
  --instance-name aihc-gh-runner-me \
  --runner-name aihc-gh-runner-me \
  --cpus 6 \
  --memory 12 \
  --disk 80
```

## Operational Notes

- Fork PRs do not use the self-hosted runner path.
- A hard VM stop can still leave an offline runner registration behind.
- The start script deletes stale offline registrations with the same runner name before registering again.
- The stop script first tries a graceful `config.sh remove`, then force-deletes the runner registration if it still exists.
- Do not use plain `limactl stop` if you want immediate cleanup on GitHub; use `scripts/stop-lima-runner.sh` instead.
