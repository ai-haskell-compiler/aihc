#!/usr/bin/env bash
# Print the files of this repository that match the pathspecs in the
# arguments. A NUL character follows each file name.
#
# The list contains tracked files and untracked files that Git does not
# ignore. It does not contain files of submodules, of other worktrees, or of
# build directories. It does not contain tracked files that are deleted.
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"
git ls-files -z --cached --others --exclude-standard -- "$@" |
	while IFS= read -r -d '' file; do
		if [ -f "$file" ]; then
			printf '%s\0' "$file"
		fi
	done
