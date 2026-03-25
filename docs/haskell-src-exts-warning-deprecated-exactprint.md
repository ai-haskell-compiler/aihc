# `haskell-src-exts` `exactPrint` bug for module `WARNING`/`DEPRECATED` pragmas

## Summary

With `haskell-src-exts-1.23.1`, `exactPrint` can emit invalid Haskell for module-header warning pragmas.

Given input like:

```haskell
module M {-# DEPRECATED "!801X.6!   " #-} where
```

`parse -> exactPrint` may produce:

```haskell
module M {-# DEPRECATED !801X.6!      #-} where
```

The quotes are dropped, and reparsing fails at `!`.

The same behavior is observed for `{-# WARNING ... #-}` in module headers.

## Impact in this repo

`parser-fuzz` normalizes candidates via `parse -> exactPrint -> parse`. The bug can trigger an internal failure during shrink/normalization even when the original generated source was valid.

## Workaround used here

Before `exactPrint`, `parser-fuzz` now normalizes module-header warning/deprecation message nodes to quoted string literals when needed, and verifies the emitted source reparses. This keeps fuzz normalization robust while preserving the normal `exactPrint` path when it already produces parseable output.
