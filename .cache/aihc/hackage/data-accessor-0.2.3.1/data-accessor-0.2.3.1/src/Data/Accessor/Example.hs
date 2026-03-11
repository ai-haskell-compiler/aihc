module Data.Accessor.Example where

import Data.Accessor.Basic ((.>), ($%), (^.), (^:), (^=), )
import Data.Accessor.Tuple (first, second, first3, second3, )

import qualified Data.Accessor.Container as Container
import qualified Data.Accessor.BinaryRead as Read
import qualified Data.Accessor.Show as Show
import qualified Data.Accessor.Basic as Accessor

import qualified Data.Array as Array
import qualified Data.Set   as Set
import qualified Data.Map   as Map

import Data.Char (ord, toUpper, )

import Prelude hiding (init)


{- * Example accesses -}

{- | Example of using 'set', 'get', 'modify'. -}
plain :: Int
plain =
   Accessor.get second $
   Accessor.modify second succ $
   Accessor.set first 'a' $
   ('b',7)

init :: (Char,Int)
init =
   Accessor.compose
      [Accessor.set first 'b',
       Accessor.modify first succ,
       Accessor.set second 7]
      (undefined,undefined)
--   setMany [first 'b', second 7] (undefined,undefined)

initInfix :: (Char,Int)
initInfix =
   (undefined,undefined)
   $% first ^= 'b'
   $% first ^: succ
   $% second ^= 7

read :: Maybe ((Char,Int), Read.Stream)
read =
   Read.runParser
      (Read.record [Read.field first, Read.field second])
      ((undefined,undefined),
       fromIntegral (ord 'c') : 59 : 154 : 202 : 0 : [])

infix0 :: Int
infix0 =
   (('b',7),"hallo")^.first^.second

infix1 :: ((Char, Int), String)
infix1 =
   (('b',7),"hallo")$%first^:second^:(1+)

infix2 :: ((Char, Int), String)
infix2 =
   (('b',7),"hallo")$%first^:second^=10

infix3 :: Int
infix3 =
   (('b',7),"hallo")^.(first.>second)

infix4 :: ((Char, Int), String)
infix4 =
   (('b',7),"hallo")$%(first.>second)^:(1+)


showsPair :: Int -> (Char, Int) -> ShowS
showsPair =
   Show.showsPrec
      [Show.field "first"  first,
       Show.field "second" second]
      "init" init

show0 :: String
show0 = showsPair 11 init ""

show1 :: String
show1 = showsPair 5 ('d',8) ""


self :: Char
self = Accessor.self ^: succ $ 'a'

null :: Char
null = Accessor.null ^= () $ 'a'

{- |
Modify a value of the 'ord' function.
-}
result :: [Int]
result =
   let f = (Accessor.result 'a' ^= 65) ord
   in  map f "abcABC"

{- |
Modify a value of a curried function.
-}
result2 :: [Int]
result2 =
   let f = (Accessor.result 0 ^: Accessor.result 0 ^= 1) div
   in  map (uncurry f) [(4,2), (2,1), (0,0)]


merge :: (Int, Char, Ordering)
merge =
   Accessor.merge first3 second3 ^= (42, 'c') $
   (23, 'a', GT)

accessHourMinute :: Accessor.T (Int, Int, Int) Int
accessHourMinute =
   Accessor.merge first3 second3 .>
   Accessor.fromWrapper (\h -> divMod h 60) (\(h,m) -> h*60+m)

mergeHourMinute :: (Int, Int, Int)
mergeHourMinute =
   accessHourMinute ^: (15+) $
   (12, 58, 13)


array :: Array.Array Int Char
array =
   Container.array 7 ^: toUpper $
   Container.array 2 ^= 'z' $
   Array.listArray (0,9) ['a'..]

set :: Set.Set Char
set =
   Container.set 'a' ^= False $
   Container.set 'd' ^: not $
   Container.set 'b' ^= True $
   Set.fromList ['a','c']

mapDefault :: Map.Map Int Char
mapDefault =
   Container.mapDefault ' ' 1 ^= '-' $
   Container.mapDefault ' ' 3 ^= 'z' $
   Container.mapDefault ' ' 5 ^: toUpper $
   Container.mapDefault ' ' 9 ^: toUpper $
   Map.fromList $ zip (map (^(2::Int)) [0..7]) ['a'..]

mapMaybe :: Map.Map Int Char
mapMaybe =
   Container.mapMaybe 1 ^= Just '-' $
   Container.mapMaybe 2 ^= Nothing $
   Container.mapMaybe 3 ^= Just 'z' $
   Container.mapMaybe 4 ^= Nothing $
   Container.mapMaybe 5 ^: fmap toUpper $
   Container.mapMaybe 9 ^: fmap toUpper $
   Map.fromList $ zip (map (^(2::Int)) [0..7]) ['a'..]
