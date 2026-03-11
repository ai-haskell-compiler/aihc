{-# LANGUAGE CPP #-}

module Data.Bijection where

#if MIN_VERSION_GLASGOW_HASKELL(9,6,0,0)
#else
import Control.Applicative (liftA2)
#endif
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntMap.Merge.Strict as IM


compatibleInsert :: (Eq a) => Int -> a -> IntMap a -> Maybe (IntMap a)
compatibleInsert x y = let
  f (Just z) = if y == z then Just (Just y) else Nothing
  f Nothing  = Just (Just y)
  in IM.alterF f x

compatibleUnion :: (Eq a) => IntMap a -> IntMap a -> Maybe (IntMap a)
compatibleUnion = let
  f _ x y = if x == y then Just x else Nothing
  in IM.mergeA IM.preserveMissing IM.preserveMissing (IM.zipWithAMatched f)

data Bij = Bij {
  rightwards :: IntMap Int,
  leftwards :: IntMap Int
} deriving (Eq, Ord)

empty :: Bij
empty = Bij IM.empty IM.empty

singleton :: Int -> Int -> Bij
singleton x y = Bij (IM.singleton x y) (IM.singleton y x)

match :: Int -> Int -> Bij -> Maybe Bij
match x y (Bij r l) = liftA2 Bij (compatibleInsert x y r) (compatibleInsert y x l)

combine :: Bij -> Bij -> Maybe Bij
combine (Bij r1 l1) (Bij r2 l2) = liftA2 Bij (compatibleUnion r1 r2) (compatibleUnion l1 l2)

pop :: Bij -> Maybe ((Int, Int), Bij)
pop (Bij r l) = case IM.minViewWithKey r of
  Nothing -> Nothing
  Just ((i, j), r') -> Just ((i, j), Bij r' (IM.delete j l))

-- | Don't check consistency, just take a union
unsafeUnion :: Bij -> Bij -> Bij
unsafeUnion (Bij r1 l1) (Bij r2 l2) = Bij (IM.union r1 r2) (IM.union l1 l2)

-- | Don't check consistency, just take a diff
unsafeDifference :: Bij -> Bij -> Bij
unsafeDifference (Bij r1 l1) (Bij r2 l2) = Bij (IM.difference r1 r2) (IM.difference l1 l2)


-- | A newtype, just to get a partial monoidal structure representing consistent
-- unions.
newtype MaybeBij = MaybeBij {
  getMaybeBij :: Maybe Bij
} deriving (Eq, Ord)

instance Semigroup MaybeBij where
  MaybeBij Nothing <> _ = MaybeBij Nothing
  _ <> MaybeBij Nothing = MaybeBij Nothing
  MaybeBij (Just a) <> MaybeBij (Just b) = MaybeBij (combine a b)

instance Monoid MaybeBij where
  mempty = MaybeBij (Just empty)

msingleton :: Int -> Int -> MaybeBij
msingleton i j = MaybeBij . Just $ singleton i j


closeBijection :: (Int -> Int -> Maybe Bij) -> Bij -> Maybe Bij
closeBijection f s = let
  inner a n = case pop n of
    Nothing -> Just a
    Just ((i,j), n') -> case f i j of
      Nothing -> Nothing
      Just b -> case combine a b of
        Nothing -> Nothing
        Just a' -> inner a' (unsafeUnion n' (unsafeDifference b a))
  in inner s s
