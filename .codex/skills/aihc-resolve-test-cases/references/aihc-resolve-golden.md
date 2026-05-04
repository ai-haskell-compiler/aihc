# AIHC Resolve Golden Fixtures

Resolver fixtures live at:
`components/aihc-resolve/test/Test/Fixtures/golden/*.yaml`

Loader format:

- `extensions`: list of parser extensions
- `modules`: list of module source strings (parsed as separate modules)
- `status`: `pass`, `xpass`, or `xfail`
- `expected`: required for `pass` and `xpass`
- `annotated`: required key for all statuses (can be `[]` for `xfail`)
- `reason`: required for `xfail`

Status behavior:

- `pass`: output must match `expected` and `annotated`.
- `xpass`: treated like strict `pass` but known to still fail CI intentionally.
- `xfail`: must fail; if it unexpectedly passes, status becomes `xpass` and the suite flags regression.

Useful pattern for class/import regressions:

1. One module defines a class or type with minimal members.
2. Second module imports and uses the target name directly.
3. Keep other declarations minimal to avoid parser/lexer noise.

Example import-member failure shape:

```haskell
module ClassMod where
class Box a where
  pick :: a -> a

module Main where
import ClassMod (Box (pick))
f = pick
```

Recommended quick check:

- Run resolver tests filtered to this fixture and ensure the fixture loads and gets categorized as expected.
- If it fails to load before execution, fix schema keys first (most often missing `expected`/`annotated`).
