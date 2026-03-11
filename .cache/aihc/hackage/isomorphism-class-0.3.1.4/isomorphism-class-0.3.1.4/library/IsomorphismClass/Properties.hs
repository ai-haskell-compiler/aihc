module IsomorphismClass.Properties
  ( isomorphicToProperties,
  )
where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude
import Test.QuickCheck

-- |
-- Properties testing whether an instance satisfies the laws of 'IsomorphicTo'.
--
-- The instance is identified via the proxy types that you provide.
--
-- E.g., here's how you can integrate it into an Hspec test-suite:
--
-- > spec = do
-- >   describe "IsomorphicTo laws" do
-- >     traverse_
-- >       (uncurry prop)
-- >       (isomorphicToProperties @Int32 @Word32 Proxy Proxy)
isomorphicToProperties ::
  (IsomorphicTo a b, Eq a, Eq b, Arbitrary a, Show a, Arbitrary b, Show b) =>
  Proxy a ->
  Proxy b ->
  [(String, Property)]
isomorphicToProperties aProxy bProxy =
  [ ( "from . to = id",
      property \b -> b === from' (to' b)
    ),
    ( "to . from = id",
      property \b -> b === to' (from' b)
    )
  ]
  where
    to' = as aProxy . to . as bProxy
    from' = as bProxy . from . as aProxy
    as = flip asProxyTypeOf
