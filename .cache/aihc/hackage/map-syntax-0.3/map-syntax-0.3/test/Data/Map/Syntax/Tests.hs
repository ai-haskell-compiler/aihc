{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Map.Syntax.Tests where

------------------------------------------------------------------------------
import qualified Data.List as L
import           Data.Function (on)
import qualified Data.Map as M
import           Data.Monoid (mempty, mappend)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.HUnit (assertEqual)

import           Data.Map.Syntax
import           Data.Map.Syntax.Util (mkMapABC, mkMapDEF,mkMapAEF,
                                       ArbMapSyntax(..))
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- |Simple tests for not-nested maps
insTests :: Spec
insTests = do
  it "Insert overwrite" overDup
  it "Insert over fail" failDup
  it "Reject duplicate" skipDup
  it "Trying dupFunc" dupFunc
  prop "Insert overwrite from list" prop_syntaxMatchesNubOver
  prop "Insert conditional from list" prop_syntaxMatchesNubCond
  prop "Insert error on dup from list" prop_syntaxMatchesNubErr

monoidLaws :: Spec
monoidLaws = do
  prop "Left identity"  prop_leftId
  prop "Right identity" prop_rightId
  prop "Associativity"  prop_assoc

------------------------------------------------------------------------------
-- |Simple tests of ##, #!, #?
overDup :: IO ()
overDup = assertEqual "Failed to overwrite duplicate entry"
          (Right $ M.fromList [("firstName","Egon") :: (String,String)])
          (runMap $ mkDupMap (##))

failDup :: IO ()
failDup = assertEqual "Failed to error on duplicate entry"
          (Left [("firstName" :: String)])
          (runMap $ mkDupMap (#!))

skipDup :: IO ()
skipDup = assertEqual "Failed to reject duplicate entry"
          (Right $ M.fromList [("firstName","Peter")])
          (runMap $ mkDupMap (#?))

dupFunc :: IO ()
dupFunc = assertEqual "Failed use dupFunc"
          (Right $ M.fromList [("firstName","firstNamePeterEgon")
            :: (String,String)])
          (runMapSyntax' f M.lookup M.insert $ mkDupMap (#!))
  where
    f k v v1 = Just (k `mappend` v1 `mappend` v)

mkDupMap :: (String -> String -> MapSyntax String String)
            -> MapSyntax String String
mkDupMap strat = do
  "firstName" `strat` "Peter"
  "firstName" `strat` "Egon"


------------------------------------------------------------------------------
prop_syntaxMatchesNubOver :: [(String,Int)] -> Bool
prop_syntaxMatchesNubOver pairs = Right revNubMap == (runMap mSyntax)
  where mSyntax   = mapM_ (\(k,v) -> (k ## v)) pairs
        revNubMap = M.fromList . L.nubBy ((==) `on` fst) . L.reverse $ pairs
        -- Nub keeps the first of each unique entry, so reverse list to
        -- simulate keeping the last

prop_syntaxMatchesNubCond :: [(String,Int)] -> Bool
prop_syntaxMatchesNubCond pairs = Right nubMap == (runMap mSyntax)
  where mSyntax = mapM_ (\(k,v) -> (k #? v)) pairs
        nubMap  = M.fromList . L.nubBy ((==) `on` fst) $ pairs

prop_syntaxMatchesNubErr :: [(String,Int)] -> Bool
prop_syntaxMatchesNubErr pairs =
  let mMap = runMap $ mapM_ (\(k,v) -> (k #! v)) pairs
  in if   pairs == L.nubBy ((==) `on` fst) pairs
     then mMap == (Right . M.fromList $ pairs)
     else case mMap of
       Right _ -> False  -- We expected (Left dupKeys)
       Left  _ -> True   -- Wasn't sure about semantics here
                         -- runMap ... ("a" #! 1) >> ("a" #! 2) >> ("a" #! 3)
                         -- should be (Left ["a"]), or (Left ["a","a"])?


------------------------------------------------------------------------------
-- |Tests for #! when do blocks are nested
nestingTests :: Spec
nestingTests = do
  it "Nested error dups"          nestedErr
  it "Nested error dups mapK"     nestedErrMapK
  it "Nester error dups mapV"     nestedErrMapV
  it "Nested overwrite dups"      nestedOver
  it "Nested overwrite dups mapK" nestedOverMapK
  it "Nested overwrite dups mapV" nestedOverMapV
  it "Nested ignore dups mixed"   nestedIgnoreMix
  it "Nested complex pass"        nestedComplex
  it "Nested complex error"       nestedComplexErr


nestedErr :: IO ()
nestedErr = assertEqual "Failed to error on duplicates across do blocks"
            (Left ['E','F'])
            (runMap $ do {mkMapDEF (#!); mkMapAEF (#!)})

nestedErrMapK :: IO ()
nestedErrMapK = assertEqual "Failed to error on mapK'ed dups across blocks"
                (Left ['B'])
                (runMap $ do
                    mapK succ $ mkMapABC (#!)
                    mapK succ $ mkMapAEF (#!)
                )

nestedErrMapV :: IO ()
nestedErrMapV = assertEqual "Failed to error on mapV'ed dups across blocks"
                (Left ['A'])
                (runMap $ do
                    mapV succ $ mkMapABC (#!)
                    mapV succ $ mkMapAEF (#!)
                    )

nestedOver :: IO ()
nestedOver = assertEqual "Failed to overwrite dup entries across blocks"
             (Right $ M.fromList
              [('A',100),('B',2),('C',3),('E',200),('F',300)])
             (runMap $ do
                 mkMapABC (##)
                 mkMapAEF (##)
             )

nestedOverMapK :: IO ()
nestedOverMapK = assertEqual "Failed to mapK in nested blocks"
                 (Right $ M.fromList
                  [('A',100),('E',200),('F',300),('C',10),('D',20),('B',2)])
                 (runMap $ do
                     mkMapABC (##)
                     mapK pred $ mkMapDEF (##)
                     mkMapAEF (##)
                 )

nestedOverMapV :: IO ()
nestedOverMapV = assertEqual "Failed to mapV in nested blocks"
                 (Right $ M.fromList
                  [('A',99),('B',2),('C',3),('E',199),('F',299)])
                 (runMap $ do
                     mkMapABC (##)
                     mapV pred $ mkMapAEF (##)
                 )

nestedIgnoreMix :: IO ()
nestedIgnoreMix = assertEqual "Failed to mapK/mapV in 'Ignore' do blocks"
                 (Right $ M.fromList
                  [('B',0),('C',1),('D',2),('E',31),('@',101)])
                 (runMap $ do
                     mapV pred . mapK succ $ mkMapABC (#?)
                     mapV succ . mapK pred $ mkMapDEF (#?)
                     mapK pred . mapV succ $ mkMapAEF (#?)
                 )

nestedComplex :: IO ()
nestedComplex = assertEqual "Failed a mix of dup strategies in nested block"
                (Right $ M.fromList
                  [('@',1),('A',2),('B',1000),('C',1000),('D',10),('E',20),('F',30),('G',300),('H',199),('I',299)])
                (runMap $ do
                    mapK succ . mapK succ $ mkMapABC (##)
                    mapK succ . mapK succ . mapK succ . mapV pred $
                      mkMapAEF (#?)
                    mapK succ ((mapV (const 1000) $ mkMapABC (##)) >>
                               mkMapAEF (#?))
                    mkMapDEF (##)
                    mapK pred $ mkMapABC (#?)
                )

nestedComplexErr :: IO ()
nestedComplexErr = assertEqual
                   "Failed to detect dup in complex nested block"
                   (Left ['B'])
                   (runMap $ do
                       mapK succ . mapK succ $ mkMapABC (##)
                       mapK succ . mapK succ . mapK succ . mapV pred $
                         mkMapAEF (#?)
                       mapK succ ((mapV (const 1000) $ mkMapABC (##)) >>
                                  mkMapAEF (#?))
                       mapK pred $ mkMapABC (#!)
                       mkMapDEF (##)
                       mapK pred $ mkMapABC (#?)
                   )


------------------------------------------------------------------------------
-- |Monoid Laws
prop_leftId :: ArbMapSyntax String Int -> Bool
prop_leftId a = runMap (mempty `mappend` m) == runMap m
  where m = unArbSyntax a

prop_rightId :: ArbMapSyntax String Int -> Bool
prop_rightId a = runMap (m `mappend` mempty) == runMap m
  where m = unArbSyntax a

prop_assoc :: ArbMapSyntax String Int
              -> ArbMapSyntax String Int
              -> ArbMapSyntax String Int
              -> Bool
prop_assoc a' b' c' =
    runMap ((a `mappend` b) `mappend` c) ==
    runMap (a `mappend` (b `mappend` c))
  where a = unArbSyntax a'
        b = unArbSyntax b'
        c = unArbSyntax c'

