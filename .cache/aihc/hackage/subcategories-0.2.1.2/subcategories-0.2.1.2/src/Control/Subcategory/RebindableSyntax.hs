{-# LANGUAGE NoImplicitPrelude #-}
module Control.Subcategory.RebindableSyntax
  ( ifThenElse, Eq(..), Num(..)
  , (>>=), (>>), fromLabel, return
  , module Control.Arrow
  ) where
import Control.Subcategory.Applicative
import Control.Subcategory.Bind
import Control.Subcategory.Functor
import Control.Subcategory.Pointed

import Control.Arrow
import GHC.OverloadedLabels
import Prelude              (Bool (..), Eq (..), Num (..))

ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _  = t
ifThenElse False _ f = f
{-# INLINE ifThenElse #-}

(>>=) :: (Dom m a, Dom m b, CBind m)
      => m a -> (a -> m b) -> m b
(>>=) = (>>-)
{-# INLINE (>>=) #-}

(>>) :: (Dom m a, Dom m b, CApplicative m)
      => m a -> m b -> m b
(>>) = (.>)
{-# INLINE (>>) #-}

return :: (Dom m a, CPointed m) => a -> m a
return = cpure
{-# INLINE return #-}
