# om-show

Haskell utilities for turning values into string representations. It is mainly
just a couple of very useful functions that eliminate a lot of conversion to
from text types. It's so small that I can just post the entire source code in
this README:

```haskell
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Utilities for showing string-like things. -}
module OM.Show (
  showt,
  showj,
  ShowJ(..),
) where


import Data.Aeson (encode, ToJSON)
import Data.String (IsString, fromString)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE


{- | Like 'show', but for any string-like thing. -}
showt :: (Show a, IsString b) => a -> b
showt = fromString . show


{- |
  Show the JSON representation as any kind of string-like thing.
  Primarily useful for dumping JSON values into log messages without having to
  jump through too many hoops.
-}
showj :: (ToJSON a, IsString b) => a -> b
showj = fromString . TL.unpack . TLE.decodeUtf8 . encode


{- |
  Wrapper whose 'Show' instance outputs JSON.

  Especially useful with `-XDerivingVia`
  e.g.

  > newtype Foo = Foo SomeType
  >   deriving Show via (ShowJ SomeType)

  This will cause @show (foo :: Foo) to output the JSON representation
  of SomeType.
-}
newtype ShowJ a = ShowJ a
  deriving stock (Eq, Ord)
  deriving newtype (ToJSON)
instance (ToJSON a) => Show (ShowJ a) where
  show = showj
```
