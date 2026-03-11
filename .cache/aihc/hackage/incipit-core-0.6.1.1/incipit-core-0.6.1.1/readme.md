# About

This library provides a `Prelude` for [Polysemy] libraries, building upon [incipit-base] and exporting most of
Polysemy's core modules.
For a more comprehensive variant that includes some basic libraries, consider [incipit].

# Usage

Using a custom `Prelude` requires the use of Cabal mixins to hide the module from `base` and replace it with
`IncipitCore`:

For `hpack`:
```yaml
dependencies:
  - name: base
    version: '>= 4 && < 5'
    mixin:
      - hiding (Prelude)
  - name: incipit-core
    version: '>= 0.3'
    mixin:
      - (IncipitCore as Prelude)
      - hiding (IncipitCore)
```

For `cabal`:
```cabal
build-depends:
    base >=4 && <5, incipit-core >= 0.3
mixins:
    base hiding (Prelude), incipit-core (IncipitCore as Prelude), incipit-core hiding (IncipitCore)
```

`incipit-core` used to export `Prelude`, but
[stack can't deal with that](https://github.com/commercialhaskell/stack/issues/5414).

# Custom Prelude

In order to extend `incipit-core` with a local `Prelude`, the module `IncipitCore` has to be reexported:

```yaml
dependencies:
  - name: base
    version: '>= 4 && < 5'
    mixin:
      - hiding (Prelude)
  - incipit-core >= 0.3
```

```haskell
module Prelude (
  module Prelude,
  module IncipitCore,
) where

import IncipitCore

projectName :: Text
projectName =
  "spaceship"
```

[incipit-base]: https://hackage.haskell.org/package/incipit-base
[incipit]: https://hackage.haskell.org/package/incipit
[Polysemy]: https://hackage.haskell.org/package/polysemy
