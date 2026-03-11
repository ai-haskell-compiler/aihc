{-# OPTIONS -Wall -Wno-orphans #-}
module Main where

import Test.QuickCheck
import Data.CircularList.Internal

instance Arbitrary a => Arbitrary (CList a) where
    arbitrary = frequency [(1, return Empty), (10, arbCList)]
        where arbCList = do
                l <- arbitrary
                f <- arbitrary
                r <- arbitrary
                return $ CList l f r
    shrink (CList l f r) = Empty : [ CList l' f' r' | l' <- shrink l,
                                                      f' <- shrink f,
                                                      r' <- shrink r]
    shrink Empty = []

-- Make sure empty really is empty.
prop_empty :: Bool
prop_empty = length (toList empty) == 0

-- Make sure converting to/from lists works.
prop_list :: CList Int -> Bool
prop_list c = c == (fromList . toList $ c)

prop_focus :: CList Int -> Int -> Bool
prop_focus c v = (Just v) == (focus $ insertR v c)

prop_rot :: CList Int -> Bool
prop_rot c = c == (rotR $ rotL c)

prop_packL :: CList Int -> Bool
prop_packL c = c == (packL c)

prop_packR :: CList Int -> Bool
prop_packR c = c == (packR c)

prop_isEmpty :: [Int] -> Bool
prop_isEmpty l = null l == isEmpty (fromList l)

prop_size :: [Int] -> Bool
prop_size l = (length l) == (size . fromList $ l)

main :: IO ()
main = do
    putStrLn "prop_empty"
    quickCheck prop_empty

    putStrLn "prop_list"
    quickCheck prop_list
    
    putStrLn "prop_rot"
    quickCheck prop_rot

    putStrLn "prop_focus"
    quickCheck prop_focus

    putStrLn "prop_packL"
    quickCheck prop_packL

    putStrLn "prop_packR"
    quickCheck prop_packR

    putStrLn "prop_isEmpty"
    quickCheck prop_isEmpty

    putStrLn "prop_size"
    quickCheck prop_size
