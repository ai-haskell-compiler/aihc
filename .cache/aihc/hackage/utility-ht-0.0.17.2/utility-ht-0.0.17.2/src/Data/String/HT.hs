module Data.String.HT where

import qualified Data.List.Reverse.StrictSpine as Rev
import Data.Char (isSpace, )

{- |
Remove leading and trailing spaces.

We use spine strict 'Rev.dropWhile' instead of the element strict version.
This is more efficient for finite 'String's because 'isSpace' is expensive.
The downside is that 'trim' does not work for infinite 'String's.
-}
trim :: String -> String
trim = Rev.dropWhile isSpace . dropWhile isSpace
