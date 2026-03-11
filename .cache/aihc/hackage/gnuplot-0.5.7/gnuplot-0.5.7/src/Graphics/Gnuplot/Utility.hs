module Graphics.Gnuplot.Utility where

import Text.Printf (printf, )

import qualified Data.Char as Char
import Data.List (intersperse, )


functionToGraph :: [x] -> (x -> y) -> [(x,y)]
functionToGraph args f = map (\x -> (x, f x)) args
-- functionToGraph args f = map swap $ attachKey f args


linearScale :: Fractional a => Integer -> (a,a) -> [a]
linearScale n (x0,x1) =
   map (\m -> x0 + (x1-x0) * fromIntegral m / fromIntegral n) [0..n]


showTriplet :: (Show a, Show b, Show c) => (a,b,c) -> String
showTriplet (x,y,z) = unwords [show x, show y, show z]


commaConcat, semiColonConcat :: [String] -> String
commaConcat = concat . intersperse ", "
semiColonConcat = concat . intersperse "; "


{-
In former versions this was simply 'show'.
However, Haskell formats non-ASCII characters with decimal backslash sequences
whereas gnuplot expects octal ones.
gnuplot does also not accept bigger octal numbers.
The current version writes non-ASCII printable characters
with the system default encoding.
-}
quote, _quote :: String -> String
quote str = '"' : foldr (++) "\"" (map escapeChar str)
_quote str = '"' : concatMap escapeChar str ++ "\""

escapeChar :: Char -> String
escapeChar c =
   case c of
      '"'  -> "\\\""
      '\\' -> "\\\\"
      '\t' -> "\\t"
      '\n' -> "\\n"
      _ -> if Char.isPrint c then [c] else printf "\\%o" $ Char.ord c

assembleCells :: [[ShowS]] -> String
assembleCells ps =
   foldr ($) ""
      (concatMap
         (\p ->
            intersperse (showString ", ") p ++
            [showString "\n"])
         ps)


listFromMaybeWith :: (a -> b) -> Maybe a -> [b]
listFromMaybeWith f = maybe [] ((:[]) . f)

formatBool :: String -> Bool -> String
formatBool name b =
   if b then name else "no"++name
