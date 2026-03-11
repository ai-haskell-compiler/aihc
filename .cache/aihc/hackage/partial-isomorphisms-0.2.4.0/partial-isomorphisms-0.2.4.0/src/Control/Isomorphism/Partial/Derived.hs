module Control.Isomorphism.Partial.Derived 
  ( foldl
  ) where

import Prelude ()
import Control.Category (Category (id, (.)))
import Control.Isomorphism.Partial.Prim (Iso, inverse, unit, associate, iterate, (***))
import Control.Isomorphism.Partial.Constructors (cons, nil)

foldl :: Iso (alpha, beta) alpha -> Iso (alpha, [beta]) alpha
foldl i = inverse unit
        . (id *** inverse nil)
        . iterate (step i) where

  step i' = (i' *** id)
         . associate
         . (id *** inverse cons)
