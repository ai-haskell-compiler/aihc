Module provides convenient functions to do some assertions in QuickCheck
properties with pretty printed reasons.  For example you can do something like
that:

```haskell
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Assertions
import Test.QuickCheck.Property

someProp :: Int -> Int -> Result
someProp a b = (a ?> b)

someOtherProp :: Double -> Double -> Result
someOtherProp a b = (a ?== b)
               
main = hspec $ describe "failing test" $ do
  prop "must fail" $ someProp
  prop "must fail again" $ someOtherProp
```

And receive pretty printed fail message when testing:

```
failing test
  - must fail FAILED [1]                    
  - must fail again FAILED [2]                             

1) failing test must fail FAILED
*** Failed! (after 1 test): 
>>>>>>>>>>>>>> the value
0
>>>>>>>>>>>>>> should be greater than value
0
0
0


2) failing test must fail again FAILED
*** Failed! (after 2 tests and 4 shrinks): 
>>>>>>>>>>>>>> expected
0.0
>>>>>>>>>>>>>> but got
1.0
0.0
1.0
```
