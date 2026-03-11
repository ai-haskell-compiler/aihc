This is an example of using tasty-th with literate haskell files.
First, we need to import the library and enable template haskell:

> {-# LANGUAGE TemplateHaskell #-}
> import Test.Tasty
> import Test.Tasty.TH
> import Test.Tasty.QuickCheck
> import Test.Tasty.HUnit

Now, we can write some quickcheck properties:

> prop_length_append :: [Int] -> [Int] -> Bool
> prop_length_append as bs = length (as ++ bs) == length as + length bs

Or write a HUnit test case:

> case_length_1 :: Assertion
> case_length_1 = 1 @=? length [()]

Properties in comments are not run:

prop_comment :: Assertion
prop_comment = assertFailure "property in comment should not be run"

We can also create test trees:

> test_plus :: [TestTree]
> test_plus =
>   [ testCase "3 + 4" (7 @=? (3 + 4))
>     --  ...
>   ]

We only need a main now that collects all our tests:

> main :: IO ()
> main = $(defaultMainGenerator)
