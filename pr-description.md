## Summary

Implement support for Safe Haskell `safe` imports in the parser. This resolves a known test failure (previously marked `xfail`) where the parser couldn't handle the `safe` keyword in import declarations.

## Root Cause

The parser didn't recognize the `safe` keyword in import declarations, failing with:
```
unexpected TkVarId "safe"
expecting module name
```

**Root cause:** The `ImportDecl` AST node lacked an `importDeclSafe` field, and the parser had no logic to parse the `safe` keyword token.

## Solution

Added `importDeclSafe :: Bool` field to `ImportDecl` and updated the parser to recognize the `safe` keyword after `import`, following the Safe Haskell grammar:
```
impdecl -> import [safe] [qualified] modid [as modid] [impspec]
```

## Changes

### Parser Implementation
- Add `TkVarSafe` token pattern to lexer
- Add `importDeclSafe :: Bool` field to `ImportDecl` AST node
- Update `importDeclParser` to parse optional `safe` keyword
- Update pretty printer to output `safe` keyword
- Update shorthand document printer to include `safe` field

### Test Infrastructure
- Update `Arbitrary ImportDecl` instance in ModuleRoundTrip
- Update `normalizeImportDecl` function
- Convert `safe-package-import` from `xfail` to `pass`
- Add `safe-import-variations` comprehensive test fixture

## Supported Syntax

The parser now correctly handles all safe import variations:
- `import safe Module`
- `import safe "package" Module`
- `import safe qualified Module`
- `import safe qualified "package" Module`
- `import safe Module qualified as M` (post-qualified)

## Testing

All 1204 tests pass with no regressions:
- ✅ Originally failing `safe-package-import` test now passes
- ✅ New comprehensive safe import variation tests pass
- ✅ All existing oracle and golden tests pass

## Progress Count Changes

- Oracle tests: +1 pass (safe-package-import converted from xfail)
- New test fixture: safe-import-variations.hs (4 variations)
