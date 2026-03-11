{- |
Personalausweisnummer

<https://de.wikipedia.org/wiki/Ausweisnummer>
-}
module Math.Checksum.CitizenCard.Germany (construct) where

import qualified Data.List.HT as ListHT
import Math.Checksum.Utility (decomposePositional)


digits :: Int -> Int -> [Int]
digits n x = reverse $ ListHT.padRight 0 n $ decomposePositional 10 x

check3 :: [Int] -> Int
check3 = flip mod 10 . sum . zipWith (*) (cycle [7,3,1])

addCheck3 :: [Int] -> [Int]
addCheck3 xs = xs ++ [check3 xs]

string :: [Int] -> String
string = concatMap show

{- |
> construct city no birthDate expiration
> construct 1234 56789 980706 180706 == "1234567897D<<9807062<1807066<<<<<<<8"
-}
construct :: Int -> Int -> Int -> Int -> String
construct city no birth expire =
   let dno     = addCheck3 $ digits 4 city ++ digits 5 no
       dbirth  = addCheck3 $ digits 6 birth
       dexpire = addCheck3 $ digits 6 expire
   in  string dno ++ "D<<" ++
       string dbirth ++ "<" ++
       string dexpire ++ replicate 7 '<' ++
       show (check3 $ dno ++ dbirth ++ dexpire)
