# swizzle-lens

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import Control.Lens
import Data.SwizzleLens qualified as SwzL
import Data.SwizzleLens.TH

foo :: (Int, Int, Int)
foo = (0, 1, 2, 3, 4, 5, 6, 7, 8) ^. SwzL.ywv	-- (1, 3, 4)

bar :: (Int, Int, Char, Int, String, Int, Int, Int, Int)
bar = (0, 1, 2, 3, 4, 5, 6, 7, 8) & SwzL.zv .~ ('c', "hello")
	-- (0, 1, 'c', 3, "hello", 5, 6, 7, 8)

swizzleLens "" "ywtr"

baz :: (Int, Int, Int, Int)
baz = (0, 1, 2, 3, 4, 5, 6, 7, 8) ^. ywtr	-- (1, 3, 6, 8)

qux :: (Int, Bool, Int, String, Int, Int, Char, Int, ())
qux = (0, 1, 2, 3, 4, 5, 6, 7, 8) & ywtr .~ (False, "hello", 'c', ())
	-- (0, False, 2, "hello", 4, 5, 'c', 7, ())
```

```haskell
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Color where

import GHC.Generics
import Control.Lens

import Data.SwizzleLens as SwzL
import Data.Swizzle.Class qualified as Swz
import Data.SwizzleSet.Class qualified as SwzS

newtype Alpha = Alpha Double deriving Show
newtype Red = Red Double deriving Show
newtype Green = Green Double deriving Show
newtype Blue = Blue Double deriving Show

data Argb = Argb Alpha Red Green Blue deriving (Show, Generic)

instance Swz.Swizzle1 Argb where type X Argb = Alpha
instance Swz.Swizzle2 Argb where type Y Argb = Red
instance Swz.Swizzle3 Argb where type Z Argb = Green
instance Swz.Swizzle4 Argb where type W Argb = Blue

instance SwzS.SwizzleSet1 Argb Alpha where type X Argb Alpha = Argb
instance SwzS.SwizzleSet2 Argb Red where type Y Argb Red = Argb
instance SwzS.SwizzleSet3 Argb Green where type Z Argb Green = Argb
instance SwzS.SwizzleSet4 Argb Blue where type W Argb Blue = Argb

sample :: Argb
sample = Argb (Alpha 0.8) (Red 0.3) (Green 0.8) (Blue 0.1)

foo :: (Red, Blue)
foo = sample ^. SwzL.yw

bar :: Argb
bar = sample & SwzL.xz .~ (Alpha 0.5, Green 0.9)
```

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Try.Prefix where

import Control.Lens
import Data.SwizzleLens.TH

swizzleLens "lens" "wtsq"

foo :: (Int, Int, Int, Int)
foo = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) ^. lensWtsq	-- (3, 6, 7, 9)

bar :: (Int, Int, Int, Char, Int, Int, Bool, String, Int, ())
bar = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9) & lensWtsq .~ ('c', True, "hello", ())
	-- (0, 1, 2, 'c', 4, 5, True, "hello", 8, ())
```
