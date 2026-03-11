{-# LANGUAGE UndecidableInstances #-}

module Generic.Type.Function.FoldMap where

import DeFun.Core ( type (~>), type (@@) )
import Data.Kind ( type Type )
import GHC.Generics

-- | 'foldMap' on generic type representations (field product level).
--
-- Will work for single-constructor types as well thanks to meta unwrapping.
type GTFoldMapC
    :: (m ~> m ~> m) -- | type-level 'mappend' defun symbol
    -> m             -- | type-level 'mempty'
    -> (Type ~> m)   -- | base case defun symbol
    -> (k -> Type)   -- | generic representation (field product level)
    -> m
type family GTFoldMapC tmappend tmempty f gf where
    GTFoldMapC tmappend tmempty f U1          = tmempty
    GTFoldMapC tmappend tmempty f (K1 i c)    = f @@ c
    GTFoldMapC tmappend tmempty f (l :*: r)   = tmappend
        @@ GTFoldMapC tmappend tmempty f l
        @@ GTFoldMapC tmappend tmempty f r
    GTFoldMapC tmappend tmempty f (M1 _ _ gf) = GTFoldMapC tmappend tmempty f gf
