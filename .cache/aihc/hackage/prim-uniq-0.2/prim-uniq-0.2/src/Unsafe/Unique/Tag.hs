{-# LANGUAGE GADTs, FlexibleInstances #-}
module Unsafe.Unique.Tag
    ( Tag
    , newTag
    , veryUnsafeMkTag
    ) where

import Data.GADT.Compare
import Data.GADT.Show
import Unsafe.Unique.Prim
import Unsafe.Coerce
import Control.Monad.Primitive
import Control.Monad
import Data.Type.Equality ((:~:)(..))

-- |The 'Tag' type is like an ad-hoc GADT allowing runtime creation of new 
-- constructors.  Specifically, it is like a GADT \"enumeration\" with one
-- phantom type.
-- 
-- A 'Tag' constructor can be generated in any primitive monad (but only tags
-- from the same one can be compared).  Every tag is equal to itself and to 
-- no other.  The 'GOrdering' class allows rediscovery of a tag's phantom type,
-- so that 'Tag's and values of type @'DSum' ('Tag' s)@ can be tested for
-- equality even when their types are not known to be equal.
--
-- 'Tag' uses a 'Uniq' as a witness of type equality, which is sound as long
-- as the 'Uniq' is truly unique and only one 'Tag' is ever constructed from
-- any given 'Uniq'.  The type of 'newTag' enforces these conditions.
-- 'veryUnsafeMkTag' provides a way for adventurous (or malicious!) users to 
-- assert that they know better than the type system.
newtype Tag s a = Tag (Uniq s) deriving (Eq, Ord)
instance Show (Tag RealWorld a) where showsPrec p (Tag u) = showsPrec p u
instance GShow (Tag RealWorld)  where gshowsPrec = showsPrec
instance GEq (Tag s) where
    geq (Tag a) (Tag b)
        | a == b    = Just (unsafeCoerce Refl)
        | otherwise = Nothing
instance GCompare (Tag s) where
    gcompare (Tag a) (Tag b) = case compare a b of
        LT -> GLT
        EQ -> unsafeCoerce (GEQ :: GOrdering () ())
        GT -> GGT

-- |Create a new tag witnessing a type @a@.  The 'GEq' or 'GOrdering' instance 
-- can be used to discover type equality of two occurrences of the same tag.
-- 
-- (I'm not sure whether the recovery is sound if @a@ is instantiated as a
-- polymorphic type, so I'd advise caution if you intend to try it.  I suspect 
-- it is, but I have not thought through it very deeply and certainly have not
-- proved it.)
newTag :: PrimMonad m => m (Tag (PrimState m) a)
newTag = liftM Tag getUniq

-- |Very dangerous! This is essentially a deferred 'unsafeCoerce': by creating
-- a tag with this function, the user accepts responsibility for ensuring 
-- uniqueness of the 'Integer' across the lifetime of the 'Tag' (including
-- properly controlling the lifetime of the 'Tag' if necessary 
-- by universal quantification when discharging the @s@ phantom type)
veryUnsafeMkTag :: Integer -> Tag s a
veryUnsafeMkTag = Tag . unsafeMkUniq
