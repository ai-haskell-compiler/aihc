{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtils where

import qualified Control.Exception as E
import qualified Data.List as L
import           Data.Maybe ( catMaybes )
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty as TT
import           Test.Tasty.HUnit
import           Text.Show.Pretty

import           Test.Tasty.Sugar


genCube :: MonadGen m => m CUBE
genCube = do inpDir <- someStr
             srcName <- someStr
             expSfx <- someStr
             seps <- Gen.filterT (\s -> length s == length (L.nub s)) $
                     Gen.string (Range.linear 0 3) Gen.alpha
             assocs <- Gen.list (Range.linear 0 10) assoc
             params <- Gen.list (Range.linear 0 5) param
             return $ mkCUBE { inputDirs = [ inpDir ]
                             , rootName = srcName
                             , expectedSuffix = expSfx
                             , separators = seps
                             , associatedNames = []
                             , validParams = []
                             }
 where
  assoc = (,) <$> someStr <*> someStr
  param = (,) <$> someStr <*> Gen.maybe (Gen.list (Range.linear 1 6) someStr)
  someStr = Gen.string (Range.linear 0 10) Gen.alpha


testWithFailInfo desc test testInp = E.catch (test testInp) (\(_e::E.SomeException) -> assertFailure (show desc))


eqTestWithFailInfo desc val = assertEqual (show desc) val


testArray name elemTests lst =
  let testElem (n,e,t) = TT.testGroup (name <> " elem#" <> show n) $ t e
      testEach = map testElem $ zip3 [0..] lst elemTests
  in testCase (name <> " count") (assertEqual "length" (length elemTests) (length lst)) : testEach

safeElem :: Int -> [a] -> Maybe a
safeElem idx lst = case drop idx lst of
                     [] -> Nothing
                     (e:_) -> Just e

compareBags name gotBag expBag =
  if gotBag `elem` L.permutations expBag
  then return ()
  else mismatchedColl name gotBag expBag


mismatchedColl name gotBag expBag =
       let expCnt = length expBag
           gotCnt = length gotBag
           uGot = L.nub gotBag
           expUCnt = length $ L.nub expBag
           gotUCnt = length $ uGot
           expUnique = L.filter (not . flip elem gotBag) expBag
           gotUnique = L.filter (not . flip elem expBag) gotBag
           nMatches = length $ L.filter (flip elem expBag) gotBag
           plural n sing plu = show n <> " " <> if n == 1 then sing else plu
           showEnt nm ent = Just $ unwords [ "Unmatched", nm, "entry:"
                                           , ppShow ent
                                           ]
       in assertFailure $
          unlines $ catMaybes $
          [ if expCnt == gotCnt
            then Just $
                 unwords [ "One or more mismatched entries in"
                         , plural expCnt "total entry" "total entries"
                         ]
            else Just $
                 unwords [ "Expected", plural expCnt "entry" "entries"
                         , "but got",  plural gotCnt "entry" "entries"
                         ]
          , if expCnt == expUCnt
            then Nothing
            else Just $
                 unwords [ "Expected results list has"
                         , plural (expCnt - expUCnt)
                           "duplicate entry"
                           "duplicate entries"
                         ]
          , if gotCnt == gotUCnt
            then Nothing
            else Just $
                 unwords ([ "Actual results list has"
                          , plural (gotCnt - gotUCnt)
                            "duplicate entry:"
                            "duplicate entries:"
                          ]
                          <>
                          let showDup e = unwords ([ "\n"
                                                   , show $ nCopies e
                                                   , "copies of"
                                                   , ppShow e
                                                   ])
                              nCopies e = length $ filter (== e) gotBag
                          in fmap showDup uGot)
          , if 0 == nMatches
            then Just "no common matches AT ALL"
            else Just $ show nMatches <> " matching common elements"
          ]
          <> (if null expUnique
              then [Nothing]
              else map (showEnt "expected") expUnique)
          <> (if null gotUnique
              then [Nothing]
              else map (showEnt "ACTUAL") gotUnique)

sugarTest name cube sample test =
  testCase name $ do (r,d) <- findSugarIn cube sample
                     testWithFailInfo d test r


sugarTestEq name cube sample expVal test =
  testCase name $ do (r,d) <- findSugarIn cube (sample cube)
                     eqTestWithFailInfo d expVal $ test r


checkCandidate cube files path subdirs sepSkip nm pm =
  testCase (nm <> " candidate")
  $ L.find (\c -> and [ nm == candidateFile c
                      , subdirs == candidateSubdirs c
                      ]) (files cube)
  @?= Just (CandidateFile { candidateDir = path
                          , candidateSubdirs = subdirs
                          , candidateFile = nm
                          , candidatePMatch = pm
                          , candidateMatchIdx = toEnum
                            $ let isSep = (`elem` (separators cube))
                                  sepsAt = L.findIndices isSep nm
                                  fstSep = case L.drop sepSkip sepsAt of
                                             (l:_) -> Just l
                                             [] -> Nothing
                                  lstSep = case reverse sepsAt of
                                             (l:_) -> Just l
                                             [] -> Nothing
                                  ln = length nm
                              in if null pm
                                 then maybe ln (1+) lstSep
                                 else if null pm
                                      then ln
                                      else maybe ln (+1) fstSep
                          })
