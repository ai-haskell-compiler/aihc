module GHC.Internal.Data.NonEmpty
  ( NonEmpty (..),
  )
where

import GHC.Internal.Base (List)

data NonEmpty a = (:|) a [a]

infixr 5 :|
