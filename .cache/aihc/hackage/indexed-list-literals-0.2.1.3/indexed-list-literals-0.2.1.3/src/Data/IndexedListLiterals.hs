{-# Language
    DataKinds
  , MultiParamTypeClasses
  , TypeInType
  , ConstraintKinds
  , FunctionalDependencies
  , FlexibleInstances
  , TypeApplications
  , RankNTypes
  , ScopedTypeVariables
  #-}
module Data.IndexedListLiterals (
    IndexedListLiterals(..)
  , ILL
  , module Data.Tuple.Only
  , ZeroTuple(..)
  , len
  , fromList
  , fromListP
  ) where

import GHC.TypeLits
import Data.Kind
import Data.Tuple.Only
import Data.Proxy
import GHC.Stack
import Control.Monad

-- | An alias for IndexedListLiterals
type ILL = IndexedListLiterals

-- | the fromList variants take a list and convert it into a tuple
--   it's sort of the inverse of toList
--
--   >> fromListP (len @3) [1,2,3]
--   >  Just (1,2,3)
--
--   >> fromListP (len @3) ["word","up"]
--   >  Nothing
--
--   >> fromListP (len @1) ['z']
--   >  Just (Only 'z') @
fromListP :: forall input (length :: Nat) output len.
             (KnownNat length, ILL input length output)
          => len length -> [output] -> Maybe input
fromListP _length = fromList

len :: Proxy a
len = Proxy

-- | >> fromList [1,2,3] :: Maybe (Int, Int, Int)
--   >  Just (1,2,3)
--
--   >> fromList ["word","up"] :: Maybe (String, String, String)
--   >  Nothing
-- 
--   >> fromList ['z'] :: Maybe (Only Char)
--   >  Just (Only 'z')
fromList :: forall input (length :: Nat) output.
            (KnownNat length, ILL input length output)
         => [output] -> Maybe input
fromList xs
  | length xs == i = Just $ fromList' xs
  | otherwise      = Nothing
  where i = fromIntegral $ natVal (len @length)

-- | A type class which allows you to write tuples which can be transformed to and from a list
--   the length of the list is also provided as a Nat
class IndexedListLiterals (input :: Type) (length :: Nat) (output :: Type) | output length -> input, input -> output length where
  -- | >> toList (Only 1)
  --   >  [1]
  --
  --   >> toList (1,2,3)
  --   >  [1,2,3]
  --
  --   >> toList ZeroTuple
  --   >  []
  toList    :: input -> [output]
  -- | a partial fromList with bad error messages
  fromList' :: [output] -> input

instance IndexedListLiterals (ZeroTuple a) 0 a where
  toList ZeroTuple = []
  fromList' [] = ZeroTuple

instance IndexedListLiterals (Only a) 1 a where
  toList (Only a) = [a]
  fromList' [a] = Only a

-- | Intuitively the zero tuple is () or Void but this breaks the Functional Dependency
--   "input -> output length" stopping reliable inference, so this constructor is used to preserve type information
data ZeroTuple a = ZeroTuple

-- all code generated below comes from this function
generate :: Int -- ^ up to N
         -> String
generate n = unlines $ join $ map ("":) $ take n $ dropOneTuple res where
  values = map ((:) 'a' . show) [1 :: Int ..]
  types  = "a" : types
  withCommas = scanl1 (\a b -> a++","++b)
  className = "IndexedListLiterals"
  template tys vals length =
      ["instance " ++ className ++ " (" ++ tys ++ ") " ++ show length ++ " " ++ head types ++ " where"
    ,"  toList    (" ++ vals ++ ") = [" ++ vals ++ "]"
    ,"  fromList' [" ++ vals ++ "] = (" ++ vals ++ ")"
    ]
  res = zipWith3 template (withCommas types) (withCommas values) [1 :: Int ..]
  dropOneTuple = tail

instance IndexedListLiterals (a,a) 2 a where
  toList    (a1,a2) = [a1,a2]
  fromList' [a1,a2] = (a1,a2)

instance IndexedListLiterals (a,a,a) 3 a where
  toList    (a1,a2,a3) = [a1,a2,a3]
  fromList' [a1,a2,a3] = (a1,a2,a3)

instance IndexedListLiterals (a,a,a,a) 4 a where
  toList    (a1,a2,a3,a4) = [a1,a2,a3,a4]
  fromList' [a1,a2,a3,a4] = (a1,a2,a3,a4)

instance IndexedListLiterals (a,a,a,a,a) 5 a where
  toList    (a1,a2,a3,a4,a5) = [a1,a2,a3,a4,a5]
  fromList' [a1,a2,a3,a4,a5] = (a1,a2,a3,a4,a5)

instance IndexedListLiterals (a,a,a,a,a,a) 6 a where
  toList    (a1,a2,a3,a4,a5,a6) = [a1,a2,a3,a4,a5,a6]
  fromList' [a1,a2,a3,a4,a5,a6] = (a1,a2,a3,a4,a5,a6)

instance IndexedListLiterals (a,a,a,a,a,a,a) 7 a where
  toList    (a1,a2,a3,a4,a5,a6,a7) = [a1,a2,a3,a4,a5,a6,a7]
  fromList' [a1,a2,a3,a4,a5,a6,a7] = (a1,a2,a3,a4,a5,a6,a7)

instance IndexedListLiterals (a,a,a,a,a,a,a,a) 8 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8) = [a1,a2,a3,a4,a5,a6,a7,a8]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8] = (a1,a2,a3,a4,a5,a6,a7,a8)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a) 9 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9) = [a1,a2,a3,a4,a5,a6,a7,a8,a9]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9] = (a1,a2,a3,a4,a5,a6,a7,a8,a9)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a) 10 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a) 11 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a) 12 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a) 13 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a) 14 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 15 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 16 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 17 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 18 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 19 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)

instance IndexedListLiterals (a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a) 20 a where
  toList    (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20) = [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20]
  fromList' [a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20] = (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
