-- | Provides test identification by Search Using Golden Answer
-- References.  This is similar in principle to Tasty.KAT and
-- Tasty.Golden, but with different input selection processes.  The
-- intent is that there are multiple different test scenarios, which
-- may all originate with the same input, and that all scenarios are
-- specified by the presence of an "expected" result file along with
-- optional support files.
--
-- A 'Tasty.Sugar.CUBE' object is provided to the 'findSugar' function
-- which returns an array of 'Tasty.Sugar.Sweets' that describe test
-- configurations.
--
-- The 'sugarOptions' should be added to the tasty Options
-- specification, and the 'sugarIngredients' provides additional
-- ingredients for the sugar testing (e.g. the ability to use
-- --showsearch and see the scan and identification of tests).
--
-- The 'withSugarGroups' function can be used to drive the test
-- invocations and group the 'Sweets' by parameter values.
--
-- Example:
--
-- > import qualified Test.Tasty as T
-- > import Test.Tasty.Hunit ( testCase, (@?=) )
-- > import Test.Tasty.Sugar
-- > import Numeric.Natural
-- >
-- > sugarCube = mkCUBE { inputDirs = [ "test/samples", "test/expected" ]
-- >                    , rootName = "*.c"
-- >                    , associatedNames = [ ("inputs", "inp") ]
-- >                    , expectedSuffix = "exp"
-- >                    }
-- >
-- > ingredients = T.includingOptions sugarOptions :
-- >               sugarIngredients [sugarCube] <>
-- >               T.defaultIngredients
-- >
-- > main =
-- >   do testSweets <- findSugar sugarCube
-- >      T.defaultMainWithIngredients ingredients .
-- >        T.testGroup "sweet tests" =<<
-- >        withSugarGroups testSweets T.testGroup mkTest
-- >
-- > mkTest :: Sweets -> Natural -> Expectation -> IO [T.TestTree]
-- > mkTest s n e = do
-- >    exp <- reads <$> readFile $ expectedFile e
-- >    return [ testCase (rootMatchName s <> " #" <> show n) $ do
-- >               Just inpF <- lookup "inputs" $ associated e
-- >               result <- testSomething inpF
-- >               result @?= exp
-- >           ]
--
-- See the README for more information.

{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Test.Tasty.Sugar
  (
    -- * Tasty Options and Ingredients
    sugarOptions
  , sugarIngredients

    -- * Test Generation Functions
  , findSugar
  , findSugarIn
  , withSugarGroups

    -- * Types
    -- ** Input
  , CUBE(..)
  , Separators
  , ParameterPattern
  , mkCUBE
  , CandidateFile(..)
  , makeCandidate
  , findCandidates
  , candidateToPath
    -- ** Output
  , Sweets(..)
  , Expectation(..)
  , Association
  , NamedParamMatch
  , ParamMatch(..)
  , paramMatchVal
  , getParamVal

    -- * Helper and Optional functions
  , distinctResults
  , rangedParamAdjuster

    -- * Reporting
  , sweetsKVITable
  , sweetsTextTable
  )
where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logic
import           Data.Either ( lefts, rights )
import qualified Data.Foldable as F
import           Data.Function
import qualified Data.List as L
import qualified Data.Map as Map
import           Data.Maybe ( isJust, isNothing, fromJust, fromMaybe )
import           Data.Proxy
import qualified Data.Text as T
import           Data.Typeable ( Typeable )
import           Numeric.Natural ( Natural )
import           Prettyprinter
import           System.IO ( hPutStrLn, stderr )
import           Test.Tasty.Ingredients
import           Test.Tasty.Options

import Test.Tasty.Sugar.Analysis
import Test.Tasty.Sugar.Candidates
import Test.Tasty.Sugar.Ranged ( rangedParamAdjuster )
import Test.Tasty.Sugar.Report
import Test.Tasty.Sugar.Types

import Prelude hiding ( exp )


----------------------------------------------------------------------

data ShowSugarSearch = ShowSugarSearch Bool deriving (Eq, Ord, Typeable)

instance IsOption ShowSugarSearch where
  defaultValue = ShowSugarSearch False
  parseValue = fmap ShowSugarSearch . safeRead
  optionName = pure $ "showsearch"
  optionHelp = pure $ "Show details of the search for the set of\n\
                      \ sample-file driven tests that would be\n\
                      \ performed based on the search."
  optionCLParser = flagCLParser Nothing (ShowSugarSearch True)


-- | Specify the Sugar-specific Tasty command-line options
sugarOptions :: [OptionDescription]
sugarOptions = [ Option (Proxy :: Proxy ShowSugarSearch)
               ]

-- | Provides the Tasty Ingredients that can be used to inform the
-- testing process.
sugarIngredients :: [CUBE] -> [Ingredient]
sugarIngredients pats = [ searchResultsSugarReport pats ]


-- | This is a Tasty "Ingredient" (aka test runner) that can be used
-- to display the search process and results for generating the tests.
-- This output can be requested by the "--showsearch" argument to the
-- test executable.

searchResultsSugarReport :: [CUBE] -> Ingredient
searchResultsSugarReport pats = TestManager [] $ \opts _tests ->
  if lookupOption opts == ShowSugarSearch True
  then Just $ do searchinfo <- mapM findSugar' pats
                 mapM_ (putStrLn . show . pretty) pats
                 putStrLn ""
                 mapM_ (putStrLn . show . snd) searchinfo
                 putStrLn ""
                 let ttlNum = sum $ join
                              $ fmap (fmap (length . expected) . fst) searchinfo
                 putStrLn ("Final set of tests [" ++ show ttlNum ++ "]:")
                 putStrLn $ show $ vsep $ concatMap (map (("•" <+>) . align . pretty) . fst) searchinfo
                 putStrLn ""
                 putStrLn $ T.unpack $ sweetsTextTable pats $ reverse $
                   F.fold (fst <$> searchinfo)
                 putStrLn $ "Total: " <> show ttlNum <> " tests"
                 return True
  else Nothing


----------------------------------------------------------------------

-- | Returns a list of the discovered test configurations (Sweets)
-- that should be run.  This function is used to get the list of
-- possible test configurations that is passed with the
-- withSugarGroups function to generate the actual tests.

findSugar :: MonadIO m => CUBE -> m [Sweets]
findSugar cube = fst <$> findSugar' cube

findSugar' :: MonadIO m => CUBE -> m ([Sweets], Doc ann)
findSugar' pat = do
  candidates <- liftIO (concat
                        <$> (mapM (findCandidates pat)
                             $ L.filter (not . null)
                             $ L.nub
                             $ inputDir pat : inputDirs pat))
  mapM_ (liftIO . hPutStrLn stderr . ("WARNING: " <>)) $ lefts candidates
  findSugarIn pat $ rights candidates


-- | Given a list of filepaths and a CUBE, returns the list of matching
-- test Sweets that should be run, and an explanation of the search
-- process (describing unmatched possibilities as well as valid test
-- configurations).
--
-- This is a low-level function; the findSugar and withSugarGroups are the
-- recommended interface functions to use for writing tests.

findSugarIn :: MonadIO m => CUBE -> [CandidateFile] -> m ([Sweets], Doc ann)
findSugarIn pat allFiles = do
  let (swts, info) = findSugarIn' pat allFiles
  sweets <- sweetAdjuster pat pat swts
  return (sweets, info)

findSugarIn' :: CUBE -> [CandidateFile] -> ([Sweets], Doc ann)
findSugarIn' pat allFiles =
  let (nCandidates, sres, stats) = checkRoots pat allFiles
      inps = concat $ fst <$> sres
      expl = vsep $
             [ "Checking for test inputs in:" <+>
               pretty (L.nub $ inputDir pat : inputDirs pat)
             , indent 2 $
               vsep $ [ "# files in directories =" <+>
                        pretty (length allFiles)
                      , "# root candidates matching" <+>
                        dquotes (pretty (rootName pat)) <+> equals <+>
                        pretty nCandidates
                      , "# valid roots" <+> equals <+>
                        pretty (length sres)
                      , "parameters = " <+> pretty (validParams pat)
                      ] <> ((("--?" <+>) . pretty) <$> (concatMap snd sres))
                      <> if null stats
                         then []
                         else [ "", "Stats:" ]
                              <> ((\(k,v) -> "  #" <+> pretty k <+> " = " <> pretty v)
                                  <$> Map.toList stats)
             ]
  in case cubeIsValid pat of
       Right _ -> (L.sortBy (compare `on` rootFile) inps, expl)
       Left e -> error e  -- this is just testing code, so error is fine

  where

    cubeIsValid :: CUBE -> Either String CUBE
    cubeIsValid cube = cube
                       <$ separatorsAreValid (separators cube)
                       <* paramsAreValid (separators cube) (validParams cube)

    separatorsAreValid :: Separators -> Either String [()]
    separatorsAreValid seps = sequence $ observeAll $
      do (s1,s2) <- choose2 seps
         let globChars = "[*](|)\\" :: String
         return $ do when (s1 == s2) $
                       Left "Duplicate separator characters"
                     when (s1 `elem` globChars) $
                       Left "Separator contains glob wildcard"
                     when (s2 `elem` globChars) $
                       Left "Separator contains glob wildcard"
                     pure ()

    paramsAreValid :: Separators
                   -> [ParameterPattern]
                   -> Either String [ParameterPattern]
    paramsAreValid seps p =
      let existential = filter (isNothing . snd) p
          blankVals = filter (or . (fmap null) . snd) p
          emptyVal = filter (or . maybe [] (fmap null) . snd) $ filter (isJust . snd) p
          dupVals = rmvOrderSwapped $ observeAll duplicatedValues
          duplicatedValues =
            do p1 <- choose p
               p2 <- choose p
               guard (isJust $ snd p1)
               guard (isJust $ snd p2)
               pv <- if (fst p1 == fst p2)
                     then do (p1v, p2v) <- choose2 $ fromJust $ snd p1
                             guard (p1v == p2v)
                             return p1v
                     else do p1v <- choose $ fromJust $ snd p1
                             p2v <- choose $ fromJust $ snd p2
                             guard (p1v == p2v)
                             return p1v
               return ((fst p1, fst p2), pv)
          sepVals = observeAll $
                    do (n,vl) <- choose p
                       guard (isJust vl)
                       v <- choose $ maybe [] id vl
                       s <- choose seps
                       guard (s `elem` v)
                       return n
          rmvOrderSwapped [] = []
          rmvOrderSwapped (e@((a,b),_):es) =
            let notSwapped ((a',b'),_) = not $ or [ a == a' && b == b'
                                                  , a == b' && b == a' ]
            in e : rmvOrderSwapped (filter notSwapped es)
      in do when (length existential > 1) $
              Left "Only one parameter can have unconstrained values (i.e. Nothing)"
            unless (null blankVals) $
              Left ("Blank validParams values are not allowed (" <>
                    (L.intercalate ", " (fst <$> blankVals)) <> ")")
            unless (null emptyVal) $
              Left ("Parameter values cannot be blank (" <>
                    (L.intercalate ", " (fst <$> emptyVal)) <> ")")
            unless (null dupVals) $
              Left ("Parameter values cannot be duplicated " <> show dupVals)
            unless (null sepVals) $
              Left ("Parameter values cannot contain separators " <>
                    show sepVals)
            return p

    choose = foldr (mplus . return) mzero

    choose2 lst = let ll = length lst
                  in do guard (ll > 1)
                        i1 <- choose [0..ll-1]
                        i2 <- choose [0..ll-1]
                        guard (i1 /= i2)
                        return (lst !! i1, lst !! i2)


-- | Removes any sweets results where the expected file matches the rootFile.
-- This is expected to be registered in the 'sweetAdjuster' field of the 'CUBE'
-- if it is used.
--
-- This is a convenience function for client code that wants to ensure that the
-- rootFile is distinct from the expected file, which could not happen prior to
-- release 2.1.0.0 but can happen from that release onward when a rootName allows
-- the expectedSuffix.

distinctResults :: [Sweets] -> [Sweets]
distinctResults sweets =
  let isDistinct s e = rootFile s /= expectedFile e
      removeRootExp s = s { expected = filter (isDistinct s) (expected s) }
  in filter (not . null . expected) $ fmap removeRootExp sweets


-- | The 'withSugarGroups' is the primary function used to run tests.
-- Given a list of 'Sweets' returned by 'findSugar', a function to
-- mark a group of tests (usually @Tasty.testGroup@), and a function
-- to generate a number of tests from a 'Sweets' and a specific
-- 'Expectation', this will iterate over the supplied 'Sweets' and
-- call the test generator for each valid test configuration.
--
-- Note that 'Sweets' contains all expectations (@[Expectation]@), but
-- the passed 'Expectation' is the only one that should be tested for
-- this set of generated tests.
--
-- > withSugarGroups sweets groupFun mkTestFun
--
-- where
--
--  * @groupFun@ is the function to group a set of tests with a
--    specific name.  Typically this can just be 'tasty.testGroup'
--
--  * @mkTestFun@ is the function to create any specific tests for the
--    specified expectation.  The output type is usually a
--    @['Tasty.TestTree']@.  This is passed the general 'Sweets', the
--    specific 'Expectation' for the tests that should be created, and
--    a numeric iteration indicating the 'Expectation' number within
--    this group.  The iteration number can be used for
--    differentiation against the other tests, but there is no
--    determinate relationship to elements of the 'Sweets' (such as
--    parameters or associated sets).  It is also possible to suppress
--    the generation of any tests for a particular 'Expectation' by
--    returning an empty list from the @mkTestFun@.

withSugarGroups :: MonadIO m
                => [Sweets]
                -> (String -> [a] -> a)
                   --  Given a name and list of tests (aka
                   -- 'TestTree'), group them (usually 'testGroup')
                -> (Sweets -> Natural -> Expectation -> m [a])
                   -- Generate any tests for this 'Expectation' (usually
                   -- @a ~ TestTree@)
                -> m [a]
withSugarGroups sweets mkGroup mkLeaf =
  let mkSweetTests sweet =
        mkGroup (rootMatchName sweet) <$>
        (mkParams sweet (expected sweet) $ cubeParams sweet)

      -- mkParams iterates through the declared expected values to
      -- create a group for each actual value per expectation, calling
      -- the user-supplied mkLeaf at the leaf of each path.

      mkParams sweet exp [] = concat <$> (mapM (uncurry $ mkLeaf sweet)
                                          $ zip [1..] exp)
      mkParams sweet exp ((name,vspec):ps) =
        case vspec of
          Nothing ->
            let pVal = lookup name . expParamsMatch
                expSrt = L.sortBy (compare `on` pVal) exp
                expGrps = L.groupBy ((==) `on` pVal) expSrt
                f es@(eh:_) =
                  let gn = fromMaybe (name <> " not specified")
                           $ (getParamVal =<<
                              (lookup name $ expParamsMatch eh)
                             )
                  in mkGroup gn <$> mkParams sweet es ps
                f [] = mkGroup (name <> " not specified") <$> mkParams sweet [] ps
            in sequence (f <$> expGrps)
          Just vs -> let f v = mkGroup (name <> "=" <> v)
                               <$> mkParams sweet (subExp v) ps
                         subExp v = expMatching name v exp
                     in sequence $ f <$> L.sort vs

      expMatching :: String -> String -> [Expectation] -> [Expectation]
      expMatching p v exp =
        filter (\e -> maybe False (paramMatchVal v) (lookup p (expParamsMatch e))) exp

  in mapM mkSweetTests $ L.sortBy (compare `on` rootMatchName) sweets
