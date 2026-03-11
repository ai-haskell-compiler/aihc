{- |
International Bank Account Number

<https://en.wikipedia.org/wiki/International_Bank_Account_Number>
-}
module Math.Checksum.IBAN (checksum, valid) where

import qualified Math.Checksum.Utility as Util

import qualified Control.Monad.Exception.Synchronous as ME
import Control.Monad.Exception.Synchronous (Exceptional(Success), throw)
import Control.Applicative (Applicative, liftA2, pure, (<$>))

import Data.Ix (inRange, index)
import Data.Bool.HT (if')


{- |
> checksum "DE" "210501700012345678" == Success 68
-}
checksum :: String -> String -> Exceptional String Int
checksum country bban =
   (98-) . remainder <$>
      mapM intFromAlphaNum bban +++
      mapM intFromAlpha country +++ pure [(100,0)]

{- |
> valid "DE68210501700012345678" == Nothing
> valid "DE68210501700012345679" == Just "check sum does not match"
-}
valid :: String -> Maybe String
valid (country0:country1:sum0:sum1:bban) = Util.processValid $ do
   k <-
      remainder <$>
         mapM intFromAlphaNum bban +++
         mapM intFromAlpha [country0,country1] +++
         mapM intFromDigit [sum0,sum1]
   return (k==1)
valid _ = Just "too few characters"

infixr 5 +++

(+++) :: (Applicative f) => f [a] -> f [a] -> f [a]
(+++) = liftA2 (++)

remainder :: [(Int,Int)] -> Int
remainder = divide 97

divide :: Int -> [(Int,Int)] -> Int
divide divisor = foldl (\r (base,x) -> mod (base*r+x) divisor) 0

intFromDigit :: Char -> Exceptional String (Int,Int)
intFromDigit c = (,) 10 <$> Util.intFromDigit c

intFromAlpha :: Char -> Exceptional String (Int,Int)
intFromAlpha c =
   fmap ((,) 100 . (10+)) $
   if' (inRange ('a','z') c) (Success $ index ('a','z') c) $
   if' (inRange ('A','Z') c) (Success $ index ('A','Z') c) $
   throw $ "not a letter: " ++ [c]

intFromAlphaNum :: Char -> Exceptional String (Int,Int)
intFromAlphaNum c =
   ME.mapException (const $ "invalid alphanumeric character: " ++ [c]) $
      ME.alternative (intFromDigit c) (intFromAlpha c)
