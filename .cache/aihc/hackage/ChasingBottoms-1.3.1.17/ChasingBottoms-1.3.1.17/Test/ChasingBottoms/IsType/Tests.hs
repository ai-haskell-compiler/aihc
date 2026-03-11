-- | Tests of the functions in "Test.ChasingBottoms.IsType".

module Test.ChasingBottoms.IsType.Tests (tests) where

import Test.ChasingBottoms.IsType

tests :: [Bool]
tests =
    -- isFunction identifies functions.
  [ isFunction (id :: Char -> Char)  ==  True
  , isFunction ((==) :: Char -> Char -> Bool)  ==  True
  , isFunction 'c'  ==  False
  , isFunction [not]  ==  False

  , isTuple [not]  ==  False
  , isTuple ()  ==  False
  , isTuple ('a', 'c')  ==  True

  , isList ""  ==  True
  , isList [not]  ==  True
  , isList ('a', 'c')  ==  False

  , isString ""  ==  True
  , isString [not]  ==  False
  , isString ('a', 'c')  ==  False
  ]
