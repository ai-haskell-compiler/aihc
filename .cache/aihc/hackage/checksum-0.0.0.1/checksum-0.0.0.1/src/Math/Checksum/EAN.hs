{- |
European Article Number

<https://en.wikipedia.org/wiki/International_Article_Number>
-}
module Math.Checksum.EAN (checksum, valid) where

import qualified Math.Checksum.Utility as Util

import Control.Monad.Exception.Synchronous (Exceptional)

import qualified Data.List as List


{- |
> checksum "400638129240" == Success 5
-}
checksum :: String -> Exceptional String Int
checksum xs = do
   ws <- mapM Util.intFromDigit xs
   return $ 9 - remainder (ws++[9])

{- |
> valid "4006381292405" == Nothing
> valid "4006381292406" == Just "check sum does not match"
-}
valid :: String -> Maybe String
valid xs = Util.processValid $ do
   ws <- mapM Util.intFromDigit xs
   return (0 == remainder ws)

{-
Not stream-friendly way, but more declarative.
-}
_remainder :: [Int] -> Int
_remainder ws = mod (sum $ zipWith (*) (cycle [1,3]) (reverse ws)) 10

remainder :: [Int] -> Int
remainder ws =
   let (x,y) = sum2 ws
   in  mod (3*x+y) 10

sum2 :: (Num a) => [a] -> (a,a)
sum2 = List.foldl' (\(a,b) x -> strictPair b (a+x)) (0,0)

strictPair :: a -> b -> (a,b)
strictPair a b = ((,) $! a) $! b
