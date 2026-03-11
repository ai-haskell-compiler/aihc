# swizzle

```haskell
import Data.Tuple
import Data.Swizzle qualified as Swz
import Data.Swizzle.TH

nums :: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)
nums = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

foo :: (Int, Int, Int, Int)
foo = Swz.zyxw nums -- (2, 1, 0, 3)

foo2, foo3, foo4, foo5, foo9 :: Int
foo2 = Swz.z nums -- 2
foo3 = Swz.w nums -- 3
foo4 = Swz.v nums -- 4
foo5 = Swz.u nums -- 5
foo9 = Swz.q nums -- 9

swizzle "" "wyvyuqztuwurqsq"

bar = wyvyuqztuwurqsq (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	-- (3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9)
```

```haskell
import GHC.Generics
import Data.Tuple
import Data.Swizzle qualified as Swz
import Data.Swizzle.Class

newtype Red = Red Double deriving Show
newtype Green = Green Double deriving Show
newtype Blue = Blue Double deriving Show
newtype Alpha = Alpha Double deriving Show

data Argb = Argb Alpha Red Green Blue deriving (Show, Generic)

instance Swizzle1 Argb where type X Argb = Alpha
instance Swizzle2 Argb where type Y Argb = Red
instance Swizzle3 Argb where type Z Argb = Green
instance Swizzle4 Argb where type W Argb = Blue

sample :: Argb
sample = Argb (Alpha 0.5) (Red 0.9) (Green 0.3) (Blue 0.2)

red :: Red
red = Swz.y sample

alphaGreen :: (Alpha, Green)
alphaGreen = Swz.xz sample

rgba :: (Red, Green, Blue, Alpha)
rgba = Swz.yzwx sample
```

```haskell
import Data.Curry
import Data.Swizzle qualified as Swz

flip13 :: (a -> b -> c -> r) -> c -> b -> a -> r
flip13 f = crr3 $ unc3 f . Swz.zyx

foo :: Int -> Int -> Int -> Int -> String
foo x y z w = show x ++ show y ++ show z ++ show w

bar :: String
bar = flip13 foo 1 2 3 4 -- "3214"
```

```haskell
import Data.Swizzle.TH

swizzle "get" "yywu"

sample :: (Int, Int, Int, Int)
sample = getYywu (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) -- (2, 2, 4, 6)
```
