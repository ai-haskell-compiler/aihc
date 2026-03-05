# aihc

## Docs

- [Haskell 2010 Language Report (Markdown)](docs/haskell2010-language-report.md)
- [Compiler Pipeline Overview](PIPELINE.md)

## Parser Progress

The from-scratch parser lives in `components/haskell-parser`.

Current Haskell2010 progress:
- `41/194` syntax cases implemented (`24.22%` complete)
- status breakdown: `PASS=41`, `XFAIL=147`, `XPASS=6`, `FAIL=0`

Recompute progress with:

```bash
nix run .#parser-progress
```

## Name-Resolution Progress

The name-resolution component lives in `components/haskell-name-resolution`.

Current progress:
- `8/12` capability cases implemented (`66.66%` complete)
- status breakdown: `PASS=8`, `XFAIL=4`, `XPASS=0`, `FAIL=0`

Recompute progress with:

```bash
nix run .#name-resolution-progress
```
