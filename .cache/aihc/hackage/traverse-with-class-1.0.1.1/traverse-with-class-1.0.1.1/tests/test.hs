{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
import Test.Tasty
import Test.Tasty.HUnit
import Data.Generics.Traversable
import Data.Generics.Traversable.Zipper
import Data.Generics.Traversable.TH

data User a = User
 { name :: String
 , age  :: Int
 , misc :: a
 } deriving Show

alice :: User (Bool, Int)
alice = User "Alice" 22 (True, 12)

deriveGTraversable ''User

class U a
instance U a

main = defaultMain $ testGroup "Tests"
  [ testCase "size via gfoldl'" $
      gfoldl' @U (\a x -> a+1) 0 alice @?= 3
  , testCase "show immediate children via gfoldMap" $
      gfoldMap @Show (pure . show) alice @?= ["\"Alice\"","22","(True,12)"]
  , testCase "show all children via everything" $
      -- don't copy-paste this code; it's quadratic!
      everything @Show (++) (pure . show) alice @?=
        ["User {name = \"Alice\", age = 22, misc = (True,12)}","\"Alice\"","'A'","'l'","'i'","'c'","'e'","22","(True,12)","True","12"]
  , testCase "A zipper walk" $ do
      let z0 = toZipper @Show alice
      let Just z1 = down' z0 -- down' moves to the leftmost child
      query show z1 @?= "\"Alice\""
      let Just z2 = right z1
      query show z2 @?= "22"
      let Just z3 = down =<< right z2 -- down moves to the rightmost child
      query show z3 @?= "12"
      let Just z4 = left z3 -- down moves to the rightmost child
      query show z4 @?= "True"
  ]
