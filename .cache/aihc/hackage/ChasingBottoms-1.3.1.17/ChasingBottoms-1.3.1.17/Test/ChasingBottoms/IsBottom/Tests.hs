{-# LANGUAGE ScopedTypeVariables #-}

-- The code below intentionally triggers some GHC warnings, so these
-- warnings are turned off.
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-missing-fields #-}

-- | Tests of the functions in "Test.ChasingBottoms.IsBottom".
--
-- Note that the warnings given when compiling this module are
-- intentional. See the internal comments for more information.

module Test.ChasingBottoms.IsBottom.Tests (tests) where

import Test.ChasingBottoms.IsBottom
import System.IO.Unsafe
import Data.Array
import System.Exit
import qualified Control.Exception as E

isException f = unsafePerformIO $
  (E.evaluate f >> return False)
    `E.catch` (\(_ :: E.SomeException) -> return True)

bot = bot
notbot x = notbot x

data T' a = L' | B' (T' a) (T' a) deriving Eq

instance Functor T'

leftInfinite' = B' leftInfinite' L'

infiniteRecursion = leftInfinite' == leftInfinite'

data A2 = A2 { aaa :: A2 } | C { ccc :: A2 }

tests :: [Bool]
tests =
    -- Basic cases.
  [ isBottom bottom
  , isBottom undefined
  , isBottom (error "...")
    -- This sometimes leads to a stack overflow.
    -- , isBottom bot

    -- const bottom /= bottom.
  , not (isBottom notbot)
  , not (isBottom (const bottom))

    -- Other types also lifted.
  , not (isBottom (bottom, bottom))
  , not (isBottom (Just bottom))

    -- Pattern match failure.
  , isBottom (let (x, y) = bottom in x :: Bool)
  , isBottom (let Just x = Nothing in x :: Char)

    -- Nonterminating, but not bottom.
  , not (isBottom [1..])

    -- Missing methods.
    -- Skip this test to avoid compiler warnings.
  , isBottom (fmap id L')

    -- Array stuff.
  , isBottom (array (1,0) [] ! 0)
  , isBottom (array (0,0) [] ! 0)

    -- Record stuff.
    -- Skip the first one to avoid compiler warnings.
  , isBottom (let x = A2 {} in aaa x)
  , isBottom (let x = A2 { aaa = x } in ccc x)
  , isBottom (let x = A2 { aaa = x } in x { ccc = x })

    -- Infinite recursion, no data produced, should yield stack
    -- overflow...
    -- Not a quick test (on some machines, anyway). And the result
    -- might be optimisation dependent.
    -- , isException (isBottom infiniteRecursion)

    -- Some other exceptions that are not caught, including
    -- nonBottomError.
  , isException (isBottom (unsafePerformIO $ exitWith ExitSuccess))
  , isException (isBottom (1 `div` 0))
  , isException (nonBottomError "...")
  ]
