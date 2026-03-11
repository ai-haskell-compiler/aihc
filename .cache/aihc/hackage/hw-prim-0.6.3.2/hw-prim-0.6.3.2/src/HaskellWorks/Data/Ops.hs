{-# LANGUAGE Rank2Types #-}

module HaskellWorks.Data.Ops
  ( (<|)
  , (|>)
  , (><)
  ) where

import qualified HaskellWorks.Data.Cons      as HW
import qualified HaskellWorks.Data.Container as HW
import qualified HaskellWorks.Data.Snoc      as HW

infixr 5 <|, ><

infixl 5 |>

(<|) :: HW.Cons v => HW.Elem v -> v -> v
(<|) = HW.cons
{-# INLINE (<|) #-}

(|>) :: HW.Snoc v => v -> HW.Elem v -> v
(|>) = HW.snoc
{-# INLINE (|>) #-}

(><) :: (Semigroup v, HW.Container v) => v -> v -> v
(><) = (<>)
{-# INLINE (><) #-}
