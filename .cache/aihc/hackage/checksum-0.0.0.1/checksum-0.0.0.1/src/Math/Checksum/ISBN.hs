{- |
International Standard Book Number

<https://en.wikipedia.org/wiki/International_Standard_Book_Number>

Covers only ISBN-10. For ISBN-13 simply use "Math.Checksum.EAN".
-}
module Math.Checksum.ISBN (checksum, valid) where

import qualified Math.Checksum.Utility as Util

import qualified Control.Monad.Exception.Synchronous as ME
import Control.Monad.Exception.Synchronous (Exceptional)
import Control.Monad (zipWithM)
import Control.Applicative ((<$>))


{- |
> checksum "346811124" == Success 10
-}
checksum :: String -> Exceptional String Int
checksum xs = remainder [1..] <$> mapM Util.intFromDigit xs

{- |
> valid "346811124X" == Nothing
> valid "3468111240" == Just "check sum does not match"
-}
valid :: String -> Maybe String
valid xs = Util.processValid $ do
   ds <-
      zipWithM id
         (replicate 9 (Util.intFromDigit=<<) ++ (intFromCheckdigit=<<) :
          [ME.switch
            (const $ return 0) (const $ ME.throw "more than 10 characters")])
         (map return xs ++ [ME.throw "less than 10 characters"])
   return $ 0 == remainder weights ds

intFromCheckdigit :: Char -> Exceptional String Int
intFromCheckdigit 'X' = return 10
intFromCheckdigit c = Util.intFromDigit c

remainder :: [Int] -> [Int] -> Int
remainder ws ds = mod (sum (zipWith (*) ws ds)) 11

weights :: [Int]
weights = [1..10] ++ [-1]
