# Haskell pattern: cast

This pattern allows to incapsulate convert from one type of object for another.

## Example

Suppose you want convert different speed units to meter per second:

``` haskell
{-# LANGUAGE MultiParamTypeClasses #-}

import Pattern.Cast

newtype MeterPerSecond   = MeterPerSecond Float
  deriving (Ord, Eq)
newtype KilometerPerHour = KilometerPerHour Float
newtype MilesPerHour     = MilesPerHour Float

instance Cast KilometerPerHour MeterPerSecond where
  cast (KilometerPerHour v) = MeterPerSecond (0.277778 * v)

instance Cast MilesPerHour MeterPerSecond where
  cast (MilesPerHour v) = MeterPerSecond (0.44704 * v)
```

As you see, you have to use `MultiParamTypeClasses` language extension.

Then in every place you can just call one function `cast`:

``` haskell
> cast (KilometerPerHour 100) :: MeterPerSecond
MeterPerSecond 27.7778
> cast (MilesPerHour 100) :: MeterPerSecond
MeterPerSecond 44.704
```

You can type your functions more abstractly. Let's look at this synthetic example:

``` haskell
type Second = Float
type Meter  = Float

data Aircraft = Aircraft { distance :: Meter
                         , time     :: Second
                         }

instance Cast Aircraft MeterPerSecond where
  cast (Aircraft d t) = MeterPerSecond (d / t)
```

Then you can use `Cast` in type of your fuction like this (`FlexibleContexts` extension has to be used):

``` haskell
{-# LANGUAGE FlexibleContexts #-}

slowerThenSound :: Cast a MeterPerSecond => a -> Bool
slowerThenSound x = cast x < MeterPerSecond 340.29
```

And this fuction can be used with every type that can be converted in `MeterPerSecond`:

``` haskell
> slowerThenSound $ MeterPerSecond 200
True
> slowerThenSound $ KilometerPerHour 1000
True
> slowerThenSound $ Aircraft 1200 3
False
```

