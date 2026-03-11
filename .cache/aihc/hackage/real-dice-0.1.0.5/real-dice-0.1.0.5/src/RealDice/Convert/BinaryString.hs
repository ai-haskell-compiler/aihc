-- | Module for converting between boolean values and strings containing '0's
-- | and '1's
module RealDice.Convert.BinaryString
  ( boolToBin,
    boolsToBin,
    binToBool,
    binToBools,
    stringToBools,
  )
where

-- | Converts a Bool to a '1' or '0'

-- | ==== __Examples__
--  >>> boolToBin True
--  '1'
--  >>> boolToBin False
--  '0'
boolToBin :: Bool -> Char
boolToBin False = '0'
boolToBin True = '1'

-- | Converts a list of Bools to a binary string

-- | ==== __Examples__
-- >>> boolsToBin [True, False, True, True]
-- "1011"
boolsToBin :: [Bool] -> String
boolsToBin = map boolToBin

-- | Converts a binary character to a Bool

-- | ==== __Examples__
-- >>> binToBool '0'
-- False
-- >>> binToBool '1'
-- True
-- >>> binToBool '2'
-- *** Exception: Invalid binary character: '2' (valid binary characters are '0' and '1')
binToBool :: Char -> Bool
binToBool '0' = False
binToBool '1' = True
binToBool x =
  error
    ( "Invalid binary character: '"
        ++ [x]
        ++ "' (valid binary characters are '0' and '1')"
    )

-- | Converts a binary string to a list of Bools

-- | ==== __Examples__
-- >>> binToBools "1011"
-- [True,False,True,True]
-- >>> binToBools "1012"
-- *** Exception: Invalid binary character: '2' (valid binary characters are '0' and '1')
binToBools :: String -> [Bool]
binToBools = map binToBool

-- | Converts a string containing '0's and '1's to a list of Bools, ignoring
--   any characters that are not '0' or '1'

-- | ==== __Examples__
-- >>> stringToBools "1011"
-- [True,False,True,True]
-- >>> stringToBools "1012"
-- [True,False,True]
-- >>> stringToBools "Hello, Haskell!"
-- []
-- >>> stringToBools "On the 10th of March, 1901, Hacksell Kerry placed 21st\n\ \ in the 100m dash, with a time of 12.3 seconds, wearing the number 101"
-- [True,False,True,False,True,True,True,False,False,True,True,False,True]
stringToBools :: String -> [Bool]
stringToBools = acc []
  where
    acc :: [Bool] -> String -> [Bool]
    acc boolList [] = boolList
    acc boolList ('0' : xs) = acc (boolList ++ [False]) xs
    acc boolList ('1' : xs) = acc (boolList ++ [True]) xs
    acc boolList (_ : xs) = acc boolList xs
