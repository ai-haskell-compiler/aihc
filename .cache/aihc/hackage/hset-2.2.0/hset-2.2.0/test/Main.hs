module Main where

import Control.Monad
import Data.HSet
import Data.Proxy
import Data.Tagged
import Test.HUnit

hset :: HSet '[Int, Double, Integer, Float]
hset = HSCons 1
      $ HSCons 2
      $ HSCons 3
      $ HSCons 4
      $ HSNil

hsetEq :: Bool
hsetEq = hset == hset

hsetCompare :: Ordering
hsetCompare = compare hset hset

hsetInt :: Int
hsetInt = hget hset

hsetDoube :: Double
hsetDoube = hget hset

hsetInteger :: Integer
hsetInteger = hget hset

hsetFloat :: Float
hsetFloat = hget hset

data Label = This | That

labeledHSet :: HSet '[Tagged 'This Int, Tagged 'That Int, Tagged "hello" String]
labeledHSet = HSCons (Tagged 1)
              $ HSCons (Tagged 2)
              $ HSCons (Tagged "olleh")
              HSNil

main :: IO ()
main = void $ runTestTT $ TestList
    [ TestList
      [ TestCase $ hsetEq @?= True
      , TestCase $ hsetCompare @?= EQ
      , TestCase $ hsetInt @?= 1
      , TestCase $ hsetDoube @?= 2
      , TestCase $ hsetInteger @?= 3
      , TestCase $ hsetFloat @?= 4
      ]
    , TestList
      [ TestCase $ (hgetTagged (Proxy :: Proxy 'This) labeledHSet) @?= (1 :: Int)
      , TestCase $ (hgetTagged (Proxy :: Proxy 'That) labeledHSet) @?= (2 :: Int)
      , TestCase $ (hgetTagged (Proxy :: Proxy "hello") labeledHSet) @?= "olleh"
      ]
    ]
