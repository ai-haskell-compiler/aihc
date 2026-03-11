import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad.Amb
import Control.Monad
import Data.List
      
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
  [ testCase "Branches" $
    allValues (do b <- aBoolean
                  if b then mzero else return 1) @?= [1]
  , testCase "Branches" $
    allValues (do b <- aBoolean
                  if b then return 1 else mzero) @?= [1]
  , testCase "aMemberOf all values ==" $
    (sort $ allValues $ do a <- aMemberOf [1,2,3,4]
                           return $ a == 4) @?= [False,False,False,True]
  , testCase "aMemberOf possible ==" $
    isPossible (do a <- aMemberOf [1,2,3,4]
                   return $ a == 4) @?= True
  , testCase "aMemberOf all values <" $
    (sort $ allValues $ do a <- aMemberOf [1,2,3,4]
                           return $ a < 5) @?= [True,True,True,True]
  , testCase "aMemberOf possible <" $
    isPossible (do a <- aMemberOf [1,2,3,4]
                   return $ a < 5) @?= True
  , testCase "aMemberOf all values >" $
    (sort $ allValues $ do a <- aMemberOf [1,2,3,4]
                           return $ a > 4) @?= [False,False,False,False]
  , testCase "aMemberOf possible >" $
    isPossible (do a <- aMemberOf [1,2,3,4]
                   return $ a > 4) @?= False
  , testCase "test2" $
    (sort $ allValues test2) @?= [(False,False),(False,True)]
  , testCase "example1" $
    (sort $ allValues example1) @?= [(2,5)]
  , testCase "example2" $
    (sort $ allValues example2) @?= [(2,6),(3,4),(3,5),(3,6)]
  , testCase "pyTriple" $
    (sort $ allValues $ pyTriple 10) @?= [(3,4,5),(6,8,10)]
  ]

test2 :: Monad m => AmbT r m (Bool, Bool)
test2 = do a <- aBoolean
           b <- aBoolean
           case (a,b) of
             (True,True) -> mzero
             (True,False) -> mzero
             (False,True) -> return (a,b)
             (False,False) -> return (a,b)

example1 :: (Eq t, Monad m, Num t) => AmbT r m (t, t)
example1 = do x <- amb [1,2,3]
              y <- amb [4,5,6]
              if x*y == 10 then return (x,y) else amb []

example2 :: (Monad m, Num t, Ord t) => AmbT r m (t, t)
example2 = do x <- amb [1,2,3]
              y <- amb [4,5,6]
              if x*y > 10 then return (x,y) else amb []

pyTriple :: (Num t, Ord t) => t -> Amb r (t, t, t)
pyTriple n = do a <- anIntegerBetween 1 n
                b <- anIntegerBetween (a + 1) n
                c <- anIntegerBetween (b + 1) n
                when (a*a + b*b /= c*c) mzero
                return (a,b,c)
