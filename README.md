# aihc

## Parser Progress

The from-scratch parser lives in `components/haskell-parser`.

Current Haskell2010 progress:
- `5/44` syntax cases implemented (`11.36%` complete)
- status breakdown: `PASS=5`, `XFAIL=39`, `XPASS=0`, `FAIL=0`

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
