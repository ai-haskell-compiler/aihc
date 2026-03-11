module Numeric.Probability.Show where

import qualified Data.List.HT as ListHT

{-
showL :: Show a => Int -> a -> String
showL n x = s ++ replicate (n-length s) ' '
            where s=show x

showL n x =
   ListHT.padRight1 ' ' n (show x)
-}

showR :: Show a => Int -> a -> String
showR n x =
   ListHT.padLeft ' ' n (show x)

--showP :: Float -> String
--showP f =  showR 3 (round (f*100))++"%"
