# AGENTS

- `gh` and `nix` are available for agents.
- Other tools (for example `ghc` and `pandoc`) are not directly available, but can be run via `nix`.
- Run tests with `nix flake check`.
- `README.md` files include progress stats and must be kept up to date when working on PRs.
- Always use `git worktree` when working on PRs locally.

## Recompute progress (agents)

Recompute parser progress:

```bash
nix run .#parser-progress
```

Recompute extension status:

```bash
nix run .#parser-extension-progress
```

Regenerate the extension markdown report:

```bash
nix run .#parser-extension-progress -- --markdown \
  | sed -n '/^# Haskell Parser Extension Support Status/,$p' \
  > docs/haskell-parser-extension-support.md
```

Recompute CPP progress:

```bash
nix run .#cpp-progress
```

Recompute name-resolution progress:

```bash
nix run .#name-resolution-progress
```
