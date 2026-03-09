# AGENTS

- Tools: `gh`, `nix` (use `nix` to run other tools like `ghc` or `pandoc`)
- Run tests: `nix flake check`
- Update generated docs: `./scripts/update-generated-content.sh --update` (verify with `--check`)
- Use `git worktree` for PR branches: `git worktree add -b <branch> worktrees/<branch>`
- Recompute parser progress: `nix run .#parser-progress`
- Recompute extension status and regenerate doc: `nix run .#parser-extension-progress -- --markdown | sed -n '/^# Haskell Parser Extension Support Status/,$p' > docs/haskell-parser-extension-support.md`
- Recompute CPP progress: `nix run .#cpp-progress`
- Recompute name-resolution progress: `nix run .#name-resolution-progress`
- Create PRs: `gh pr create --base main --head <branch> --title "<title>" --body <file>`
- Keep README progress sections updated before opening PRs
