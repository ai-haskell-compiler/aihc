module Demo where

import Data.Array.Byte (ByteArray)
import Prelude

empty :: Maybe ByteArray
empty = pure []

bytes :: ByteArray
bytes = [0, 15, 255]

nested :: [ByteArray]
nested = [[], [1]]

match :: [Bool] -> Maybe ByteArray
match [] = pure []
match (_ : xs) = match xs
