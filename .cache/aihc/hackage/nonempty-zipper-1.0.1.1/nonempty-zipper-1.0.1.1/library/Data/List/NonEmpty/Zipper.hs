{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.List.NonEmpty.Zipper
  ( Zipper

    -- * Accessors
  , lefts
  , rights
  , current

    -- * Traversal
  , left
  , right
  , leftN
  , rightN
  , findLeft
  , findRight
  , start
  , end

    -- * Construction
  , fromNonEmpty
  , fromNonEmptyEnd

    -- ** Update
  , replace
  , delete
  , push
  , pop
  , shift
  , unshift
  , reverse

    -- * Predicates
  , isStart
  , isEnd
  )
where

import Prelude hiding (reverse)
import qualified Prelude

import Control.Comonad
import Control.DeepSeq (NFData)
import Control.Monad (foldM)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Safe (headMay, tailMay)

data Zipper a = Zipper [a] a [a]
  deriving stock (Eq, Show, Functor, Generic)
  deriving anyclass (NFData)

instance Foldable Zipper where
  foldMap f (Zipper ls x rs) = foldMap f (Prelude.reverse ls) `mappend` f x `mappend` foldMap f rs

instance Traversable Zipper where
  traverse f (Zipper ls x rs) =
    Zipper
      <$> (Prelude.reverse <$> traverse f (Prelude.reverse ls))
      <*> f x
      <*> traverse f rs

-- | The list zipper is a basic comonad
--
-- This instance allows us to create a zipper of all possible states of
-- traversing the zipper with 'left' and 'right'.
--
-- >>> duplicate $ fromNonEmpty $ NE.fromList [1, 2, 3]
-- Zipper [] (Zipper [] 1 [2,3]) [Zipper [1] 2 [3],Zipper [2,1] 3 []]
instance Comonad Zipper where
  extract = current
  duplicate z =
    let dupWith f r =
          case f r of
            Nothing -> [r]
            Just x -> r : dupWith f x
    in  Zipper
          (maybe [] (dupWith left) $ left z)
          z
          (maybe [] (dupWith right) $ right z)

-- | Get the current focus of the @'Zipper'@ cursor
--
-- This is a synonym for 'Control.Comonad.extract'
--
-- >>> current . fromNonEmpty $ NE.fromList [1, 2, 3]
-- 1
current :: Zipper a -> a
current (Zipper _ curr _) = curr

-- | Get all values on the left of the cursor
--
-- >>> lefts . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- [1,2]
lefts :: Zipper a -> [a]
lefts (Zipper ls _ _) = Prelude.reverse ls

-- | Get all values on the right of the cursor
--
-- >>> rights . fromNonEmpty $ NE.fromList [1, 2, 3]
-- [2,3]
rights :: Zipper a -> [a]
rights (Zipper _ _ rs) = rs

-- | Move the current focus of the cursor to the left
--
-- >>> left . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- Just (Zipper [1] 2 [3])
left :: Zipper a -> Maybe (Zipper a)
left (Zipper ps curr ns) = do
  newCurr <- headMay ps
  pure $ Zipper (fromMaybe [] $ tailMay ps) newCurr (curr : ns)

-- | Move the current focus of the cursor to the right
--
-- >>> right . fromNonEmpty $ NE.fromList [1, 2, 3]
-- Just (Zipper [1] 2 [3])
right :: Zipper a -> Maybe (Zipper a)
right (Zipper ps curr ns) = do
  newCurr <- headMay ns
  pure $ Zipper (curr : ps) newCurr (fromMaybe [] $ tailMay ns)

-- | Move the current focus of the cursor some distance to the left
--
-- Returns 'Nothing' if the requested movement distance is too great
-- and the cursor falls off the left end of the zipper.
--
-- >>> leftN 2 . fromNonEmptyEnd $ NE.fromList [1..7]
-- Just (Zipper [4,3,2,1] 5 [6,7])
leftN :: Word -> Zipper a -> Maybe (Zipper a)
leftN = repeatedly left

-- | Move the current focus of the cursor some distance to the right
--
-- Returns 'Nothing' if the requested movement distance is too great
-- and the cursor falls off the right end of the zipper.
--
-- >>> rightN 2 . fromNonEmpty $ NE.fromList [1..7]
-- Just (Zipper [2,1] 3 [4,5,6,7])
rightN :: Word -> Zipper a -> Maybe (Zipper a)
rightN = repeatedly right

repeatedly :: Monad f => (a -> f a) -> Word -> a -> f a
repeatedly f n x = foldM (const . f) x [1 .. n]

-- | Move the current focus of the cursor to the first occurrence of a value on the left
--
-- >>> findLeft 2 . fromNonEmptyEnd $ NE.fromList [2, 1, 2, 1, 1, 3]
-- Just (Zipper [1,2] 2 [1,1,3])
findLeft :: Eq a => a -> Zipper a -> Maybe (Zipper a)
findLeft target z@(Zipper ps curr ns)
  | curr == target = Just z
  | otherwise = case ps of
      [] -> Nothing
      (x : xs) -> findLeft target (Zipper xs x (curr : ns))

-- | Move the current focus of the cursor to the first occurrence of a value on the right
--
-- >>> findRight 3 . fromNonEmpty $ NE.fromList [2, 1, 3, 1, 1, 3]
-- Just (Zipper [1,2] 3 [1,1,3])
findRight :: Eq a => a -> Zipper a -> Maybe (Zipper a)
findRight target z@(Zipper ps curr ns)
  | curr == target = Just z
  | otherwise = case ns of
      [] -> Nothing
      (x : xs) -> findRight target (Zipper (curr : ps) x xs)

-- | Move the current focus of the cursor to the start of the @'Zipper'@
--
-- >>> start . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- Zipper [] 1 [2,3]
start :: Zipper a -> Zipper a
start z
  | isStart z = z
  | otherwise = fromNonEmpty $ toNonEmpty z

-- | Move the current focus of the cursor to the end of the @'Zipper'@
--
-- >>> end . fromNonEmpty $ NE.fromList [1, 2, 3]
-- Zipper [2,1] 3 []
end :: Zipper a -> Zipper a
end z
  | isEnd z = z
  | otherwise = fromNonEmptyEnd $ toNonEmpty z

fromNonEmpty :: NE.NonEmpty a -> Zipper a
fromNonEmpty ne = Zipper [] (NE.head ne) (NE.tail ne)

fromNonEmptyEnd :: NE.NonEmpty a -> Zipper a
fromNonEmptyEnd ne = Zipper (NE.tail reversed) (NE.head reversed) []
 where
  reversed = NE.reverse ne

toNonEmpty :: Zipper a -> NE.NonEmpty a
toNonEmpty (Zipper ls x rs) = NE.fromList $ Prelude.reverse ls ++ [x] ++ rs

-- | Replace the current item under the cursor
--
-- >>> replace 4 . fromNonEmpty $ NE.fromList [1, 2, 3]
-- Zipper [] 4 [2,3]
replace :: a -> Zipper a -> Zipper a
replace x (Zipper ls _ rs) = Zipper ls x rs

-- | Delete the item currently under the cursor
--
-- The item currently under the cursor is removed. The cursors focus will move
-- right. If at the end of the @'Zipper'@ the cursor will move left.
--
-- >>> delete . fromNonEmpty $ NE.fromList [1, 2, 3]
-- Just (Zipper [] 2 [3])
--
-- >>> delete . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- Just (Zipper [1] 2 [])
delete :: Zipper a -> Maybe (Zipper a)
delete (Zipper [] _ []) = Nothing
delete (Zipper ls _ (r : rs)) = Just $ Zipper ls r rs
delete (Zipper (l : ls) _ rs) = Just $ Zipper ls l rs

-- | Insert a value to the left of the cursor
--
-- >>> push 0 . fromNonEmpty $ NE.fromList [1, 2, 3]
-- Zipper [0] 1 [2,3]
push :: a -> Zipper a -> Zipper a
push l (Zipper ls x rs) = Zipper (l : ls) x rs

-- | Remove a value to the left of the cursor
--
-- >>> pop . fromNonEmpty $ NE.fromList [1, 2, 3]
-- (Zipper [] 1 [2,3],Nothing)
--
-- >>> pop . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- (Zipper [1] 3 [],Just 2)
pop :: Zipper a -> (Zipper a, Maybe a)
pop (Zipper [] x rs) = (Zipper [] x rs, Nothing)
pop (Zipper (l : ls) x rs) = (Zipper ls x rs, Just l)

-- | Remove a value to the right of the cursor
--
-- >>> shift . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- (Zipper [2,1] 3 [],Nothing)
--
-- >>> shift . fromNonEmpty $ NE.fromList [1, 2, 3]
-- (Zipper [] 1 [3],Just 2)
shift :: Zipper a -> (Zipper a, Maybe a)
shift (Zipper ls x []) = (Zipper ls x [], Nothing)
shift (Zipper ls x (r : rs)) = (Zipper ls x rs, Just r)

-- | Insert a value to the right of the cursor
--
-- >>> unshift 4 . fromNonEmpty $ NE.fromList [1, 2, 3]
-- Zipper [] 1 [4,2,3]
unshift :: a -> Zipper a -> Zipper a
unshift r (Zipper ls x rs) = Zipper ls x (r : rs)

-- | Reverse the zipper keeping the cursor focus intact
--
-- >>> reverse . fromNonEmpty $ NE.fromList [1, 2, 3]
-- Zipper [2,3] 1 []
reverse :: Zipper a -> Zipper a
reverse (Zipper ls x rs) = Zipper rs x ls

-- | Determine if the @'Zipper'@ is at the beginning
--
-- >>> isStart . fromNonEmpty $ NE.fromList [1, 2, 3]
-- True
--
-- >>> isStart . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- False
isStart :: Zipper a -> Bool
isStart (Zipper [] _ _) = True
isStart _ = False

-- | Determine if the @'Zipper'@ is at the end
--
-- >>> isEnd . fromNonEmptyEnd $ NE.fromList [1, 2, 3]
-- True
--
-- >>> isEnd . fromNonEmpty $ NE.fromList [1, 2, 3]
-- False
isEnd :: Zipper a -> Bool
isEnd (Zipper _ _ []) = True
isEnd _ = False
