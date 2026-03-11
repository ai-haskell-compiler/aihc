module Main (main) where

import Control.Applicative               (liftA2)
import Data.Function.Step
       (SF, (!))
import Test.QuickCheck                   (quickCheck)
import Test.QuickCheck.Arbitrary         (Arbitrary (..))
import Test.QuickCheck.Property          (Property, counterexample, (===))

merge :: Ord x => (a -> b -> c) -> SF x a -> SF x b -> SF x c
merge = liftA2

mergeProp
    :: (Eq c, Ord x, Show c,Show x)
    => (a -> b -> c)
    -> x
    -> SF x a
    -> SF x b
    -> Property
mergeProp f x sf1 sf2 =
    counterexample ("merged: " ++ show merged) $
    merged ! x === f (sf1 ! x) (sf2 ! x)
  where
    merged = merge f sf1 sf2

main :: IO ()
main = do
    -- "merge: Int addition"
    quickCheck (mergeProp (+) :: Int -> SF Int Int -> SF Int Int -> Property)
    -- "merge: Int subtraction"
    quickCheck (mergeProp (-) :: Int -> SF Int Int -> SF Int Int -> Property)
    -- "merge: Int multiplication"
    quickCheck (mergeProp (*) :: Int -> SF Int Int -> SF Int Int -> Property)
    -- "merge: Bool logical or"
    quickCheck (mergeProp (||) :: Bool -> SF Bool Bool -> SF Bool Bool -> Property)
    -- "merge: Bool logical and"
    quickCheck (mergeProp (&&) :: Bool -> SF Bool Bool -> SF Bool Bool -> Property)
    -- "merge: Double addition"
    quickCheck (mergeProp (+) :: Double -> SF Double Double -> SF Double Double -> Property)
    -- "merge: Double subtraction"
    quickCheck (mergeProp (-) :: Double -> SF Double Double -> SF Double Double -> Property)
    -- "merge: Double multiplication"
    quickCheck (mergeProp (*) :: Double -> SF Double Double -> SF Double Double -> Property)
