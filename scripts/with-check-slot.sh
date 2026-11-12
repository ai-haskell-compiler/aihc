#!/usr/bin/env bash
# Run a command while it holds one check slot of the host.
#
# The runners of a pool share one host and one nix-daemon. If many checks run
# at the same time, each check becomes much slower. The pool script makes a
# directory with one file for each slot and mounts it into each runner. This
# script locks one of those files and then runs the command. The kernel
# releases the lock when the command stops, also after a failure.
#
# If the directory does not exist, there is no limit and the command runs
# immediately. A hosted runner and an old pool are examples.
set -euo pipefail

if [ "$#" -eq 0 ]; then
	echo "Usage: scripts/with-check-slot.sh COMMAND [ARGUMENT...]" >&2
	exit 2
fi

slot_dir="${AIHC_CHECK_SLOT_DIR:-/run/aihc-check-slots}"
# The time limit of the job includes the wait. After this number of seconds,
# the command runs without a slot.
max_wait="${AIHC_CHECK_SLOT_MAX_WAIT:-900}"
poll_interval="${AIHC_CHECK_SLOT_POLL_INTERVAL:-5}"

slots=()
if [ -d "$slot_dir" ]; then
	for slot in "$slot_dir"/slot-*; do
		if [ -f "$slot" ]; then
			slots+=("$slot")
		fi
	done
fi

if [ "${#slots[@]}" -eq 0 ]; then
	echo "No check slots in ${slot_dir}. Run the command without a limit."
	exec "$@"
fi

if ! command -v flock >/dev/null 2>&1; then
	echo "The flock command is not available. Run the command without a limit."
	exec "$@"
fi

start=$SECONDS
reported=0
while true; do
	for slot in "${slots[@]}"; do
		# A read-only descriptor is sufficient for a lock. Thus, the slot
		# files can be read-only in the runner.
		exec {slot_fd}<"$slot"
		if flock --nonblock "$slot_fd"; then
			echo "Got check slot $(basename "$slot") after $((SECONDS - start)) s."
			# The command inherits the descriptor and thus holds the lock.
			exec "$@"
		fi
		exec {slot_fd}<&-
	done

	if [ $((SECONDS - start)) -ge "$max_wait" ]; then
		echo "No check slot became free in ${max_wait} s. Run the command without a slot."
		exec "$@"
	fi
	if [ "$reported" -eq 0 ]; then
		echo "All ${#slots[@]} check slots are in use. Wait for a free slot."
		reported=1
	fi
	sleep "$poll_interval"
done
