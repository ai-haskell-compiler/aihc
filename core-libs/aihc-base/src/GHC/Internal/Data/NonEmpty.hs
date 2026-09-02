module GHC.Internal.Data.NonEmpty
  ( NonEmpty (..),
  )
where

import GHC.Base (List)

data NonEmpty a = (:|) a [a]

infixr 5 :|
