module Note where

pitch :: Char -> Int -> Int -> (Char, Int)
pitch char nt oct = (char, nt+oct*12)

c,d,e,f,g,a,b :: Int -> (Char, Int)
c = pitch 'C'  0
d = pitch 'D'  2
e = pitch 'E'  4
f = pitch 'F'  5
g = pitch 'G'  7
a = pitch 'A'  9
b = pitch 'B' 11

(#) :: (a -> (Char, Int)) -> a -> (Char, Int)
noteFunc # oct = ('#', succ $ snd $ noteFunc oct)


infixr 5 .:, |:

(.:), (|:) :: a -> [(Bool, a)] -> [(Bool, a)]
x .: xs  = (False,x) : xs
x |: xs  = (True, x) : xs


pitches15, pitches30 :: [(Bool, (Char, Int))]
pitches15 =
   c 0 .: d 0 .: e 0 |: f 0 .: g 0 |: a 0 .: b 0 |:
   c 1 .: d 1 |: e 1 .: f 1 |: g 1 .: a 1 .: b 1 .:
   c 2 .: []

pitches30 =
   c 0 .: d 0 .: g 0 .: a 0 .: b 0 .:
   c 1 .: d 1 .: e 1 |:
   f 1 .: f#1 .: g 1 |: g#1 .: a 1 .: a#1 .: b 1 |:
   c 2 .: c#2 .: d 2 |: d#2 .: e 2 .:
   f 2 |: f#2 .: g 2 .: g#2 .: a 2 .: a#2 .: b 2 .:
   c 3 .: d 3 .: e 3 .:
   []
