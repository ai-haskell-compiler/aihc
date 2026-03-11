{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.State (execState)
import Data.Optics.Operators ((*=), (+=), (-=), (//=))
import GHC.Generics (Generic)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

newtype Foo = Foo {bar :: Float}
  deriving
    ( Floating
    , Fractional
    , Ord
    , Num
    , Real
    , RealFrac
    , RealFloat
    , Eq
    , Generic
    )

-- (+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()

main :: IO ()
main = defaultMain $ testGroup "Data.Optic.Operators" testTree
 where
  -- A test case for simple operators e.g. +=, -=, etc.
  testBasic name f g =
    testProperty name $ \(x :: Float, y :: Float) ->
      let a = Foo (x `f` y)
          b = execState (#bar `g` y) (Foo x)
       in a == b || isNaN a && isNaN b
  testTree =
    [ testBasic "(+=)" (+) (+=)
    , testBasic "(-=)" (-) (-=)
    , testBasic "(*=)" (*) (*=)
    , testBasic "(//=)" (/) (//=)
    ]
