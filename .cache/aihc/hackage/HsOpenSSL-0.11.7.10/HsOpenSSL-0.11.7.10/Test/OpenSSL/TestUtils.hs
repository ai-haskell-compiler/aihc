module TestUtils where

import qualified Control.Exception as E
import Control.Monad

assertBool :: String -> Bool -> IO ()
assertBool n ok =
    unless ok $ E.throw $ E.AssertionFailed $ "Assertion failed: " ++ n

assertEqual :: (Show a, Eq a) => String -> a -> a -> IO ()
assertEqual n a b =
    assertBool (n ++ "\n" ++ show a ++ " /= " ++ show b) (a == b)

assertFunction
    :: (Show x, Show y, Eq y) => String -> (x -> y) -> [(x, y)] -> IO ()
assertFunction n f points =
    forM_ points $ \ (x, y) ->
        let r = f x in
        assertBool
            (n ++ " " ++ showsPrec 11 x "" ++ " == " ++ show r
             ++ " /= " ++ show y)
            (r == y)

--  assertFunction "asdf" (fmap (+1)) [(Just 1, Nothing)]
--  *** Exception: Assertion failed: asdf (Just 1) == Just 2 /= Nothing
