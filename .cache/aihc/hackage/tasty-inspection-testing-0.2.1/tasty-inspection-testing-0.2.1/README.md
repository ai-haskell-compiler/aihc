# tasty-inspection-tasting

Integrate [`inspection-testing`](http://hackage.haskell.org/package/inspection-testing)
into [`tasty`](http://hackage.haskell.org/package/tasty) test suites.

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O -dsuppress-all -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}

import Test.Tasty
import Test.Tasty.Inspection

lhs :: (a -> b) -> Maybe a -> Bool
lhs f x = case fmap f x of
  Nothing -> True
  Just{}  -> False

rhs :: (a -> b) -> Maybe a -> Bool
rhs _ Nothing = True
rhs _ Just{}  = False

main :: IO ()
main = defaultMain $(inspectTest $ 'lhs === 'rhs)
```

Real world examples:

* https://hackage.haskell.org/package/random-1.2.1/src/test-inspection/Spec/Inspection.hs
* https://hackage.haskell.org/package/linear-base-0.2.0/src/test/Test/Data/V.hs
* https://hackage.haskell.org/package/text-2.0/src/tests/Tests/Properties/LowLevel.hs
* https://hackage.haskell.org/package/text-1.2.5.0/src/tests/Tests/Inspection/Strict.hs
* https://hackage.haskell.org/package/sized-1.0.0.1/src/test/opt-test.hs
* https://hackage.haskell.org/package/vector-0.13.0.0/src/tests-inspect/Inspect/DerivingVia.hs
* https://hackage.haskell.org/package/infinite-list-0.1/src/test/Fusion.hs
