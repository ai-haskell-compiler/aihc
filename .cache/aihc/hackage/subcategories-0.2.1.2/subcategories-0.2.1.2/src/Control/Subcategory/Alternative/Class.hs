{-# LANGUAGE EmptyCase, UndecidableSuperClasses #-}
module Control.Subcategory.Alternative.Class
  (CChoice(..), CAlternative(..)) where
import Control.Subcategory.Functor

import qualified Control.Applicative as App

infixl 3 <!>
class CFunctor f => CChoice f where
  (<!>) :: Dom f a => f a -> f a -> f a
  default (<!>) :: App.Alternative f => f a -> f a -> f a
  (<!>) = (App.<|>)
  {-# INLINE (<!>) #-}

class CChoice f => CAlternative f where
  cempty :: Dom f a => f a
  default cempty :: App.Alternative f => f a
  cempty = App.empty
  {-# INLINE cempty #-}

