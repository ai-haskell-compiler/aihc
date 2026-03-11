# Verset

[![Haskell-CI](https://github.com/andrevdm/verset/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/andrevdm/verset/actions/workflows/haskell-ci.yml)

Small Haskell alternative prelude based on [Protolude](https://hackage.haskell.org/package/protolude) and [Intro](https://hackage.haskell.org/package/intro)

## Why
(*At least it is not a monad tutorial*)

There are some great alternative preludes around but I find most of them either too large or too opinionated. 
What I'd rather have is a minimal prelude and then layer additional changes over it. 
Obviously if the other preludes suit you better, then Verset is not for you :) 

## Goals
 - Very small
 - Minimal dependencies
 - Removes partial functions where possible
 - It be easy to switch from `Verset` to other preludes

## Notes
 - `catch`, `finally` etc are not exported. This makes it easier to use e.g. `Control.Exception.Safe`, `UnliftIO.Exception` etc for safer exception without having to hide all the defaults
 - No transformers are exposed but `lift` is
 - Simple `IsString` helpers (from Protolude)
 - `identity` rather than `id` (from Protolude)
 - `String` is not exported use `[Char]`. This is to discourage its use.


### If you want to reduce imports

If you want to use Verset but would rather avoid imports in all your modules. There are at least two optios

1) Create a module with the imports you always use and import that along with Verset

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Verse
    ( (Control.Lens.^.)
    , (Control.Lens.^..)
    ) where

import           Verset
import qualified Control.Lens
```

```haskell
module Demo where
  import           Verset
  import           Verse
  import qualified Whatever as W
```


2) Similar to the method above but reexport verset

```haskell
{-# LANGUAGE NoImplicitPrelude #-}

module Verse
    ( module Verset
    , (Control.Lens.^.)
    , (Control.Lens.^..)
    ) where

import           Verset
import qualified Control.Lens
```

```haskell
module Demo where
  import           Verse
  import qualified Whatever as W
```

#### But...

Personally I happy with imports as I think it makes the code more explicit. However if you want to avoid them using something like the above methods I think there are still benefits to Verset.
  - Verset is not getting in your way.
  - You can define company wide defaults, project wide defaults or both


## Compatibility

Tested with GHC 8.7.10, 9.0.1 and 9.2.1
