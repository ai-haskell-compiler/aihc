{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  RunW3CTests
--  Copyright   :  (c) 2013 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  OverloadedStrings, RecordWildCards
--
--  Run the W3C Turtle tests using the supplied manifest file (Turtle format).
--  It requires that the tests are installed locally (i.e. it will /not/ 
--  download from the Turtle test suite at <http://www.w3.org/2013/TurtleTests/>).
--
--  Possible improvements:
--
--   - add an @--enable-roundtrip@ flag which would do a round-trip test
--     on all the valid files, to check the formatter as well as the parser
--     (or just for those tests with a NTriples version, since that can be
--      used to create the output graph, but may miss a few edge cases).
--
--   - create an EARL report (<http://www.w3.org/TR/EARL10-Schema/>), for
--     <https://dvcs.w3.org/hg/rdf/raw-file/default/rdf-turtle/reports/index.html>.
--     See also <http://lists.w3.org/Archives/Public/public-rdf-comments/2013Aug/0013.html>.
--
--   - option to download the tests from the W3C site.
--
--------------------------------------------------------------------------------

module Main where

import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

import qualified Swish.RDF.Parser.Turtle as TTL
import qualified Swish.RDF.Parser.NTriples as NT

import Control.Monad (forM_)

import Data.Maybe (catMaybes)
import Data.Version (showVersion)

import Network.URI (URI, parseURI, parseURIReference, relativeTo, uriPath, uriScheme)

import Swish.RDF.Graph
import Swish.Namespace (ScopedName, getScopeURI)
import Swish.RDF.Query
import Swish.RDF.Vocabulary.RDF (rdfType)
import Swish.RDF.Vocabulary.XSD (xsdString)

import System.Directory (canonicalizePath)
import System.Environment
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (splitFileName)
import System.IO (hFlush, hPutStr, hPutStrLn, stderr, stdout)

import Paths_swish (version)

-- | The base URI for the tests.
base :: Maybe URI
base = parseURI "http://www.w3.org/2013/TurtleTests/"

-- Could include the language type for the Parse version.

-- | I have decided to treat @rdf:type rdft:TestTurtleNegativeEval@
--   tests the same as @rdf:TestTurtleNegativeSyntax@.
data TestType =
  NTriplesParse Bool -- ^ Should the NTriples file parse successfully?
  | TurtleParse Bool -- ^ Should the Turtle file parse successfully?
  | TurtleCompare    -- ^ The Turtle and NTriples files should match.
  
_showBool :: Bool -> String
_showBool True = "pass"
_showBool _ = "fail"

instance Show TestType where
  show (NTriplesParse a) = "<NTriples parse " ++ _showBool a ++ ">"
  show (TurtleParse a) = "<Turtle parse " ++ _showBool a ++ ">"
  show TurtleCompare = "<Turtle compare>"
                             
data Test =
  Test
  {
    _tName :: String
  , _tAction :: IO (Maybe String)
    -- ^ If the test fails a string reporting the error is returned.
  } 

-- | Returns the name of the test if it failed.
runTest :: Test -> IO (Maybe String)
runTest Test {..} = _tAction >>= \r -> hFlush stdout >> return r

runTests :: [Test] -> IO ()
runTests ts = do
  putStrLn $ "Running " ++ show (length ts) ++ " tests"
  hFlush stdout
  fails <- catMaybes `fmap` mapM runTest ts
  putStrLn ""
  case fails of
    [] -> putStrLn "All tests passed." >> exitSuccess
    [f] -> hPutStrLn stderr ("One test failed: " ++ f) >> exitFailure
    _ -> do
      let nf = show $ length fails
      hPutStrLn stderr $ "There were " ++ nf ++ " failures:"
      forM_ (zip [(1::Int)..] fails) $ \(n,m) -> do
        hPutStr stderr $ "# [" ++ show n ++ "/" ++ nf ++ "] "
        hPutStrLn stderr m
      exitFailure

mfEntries, mfName :: ScopedName
mfEntries = "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entries"
mfName = "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name"

mfAction, mfResult :: ScopedName
mfAction = "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action"
mfResult = "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#result"

rdftTestTurtleEval, rdftTestTurtleNegativeEval, rdftTestTurtlePositiveSyntax, rdftTestTurtleNegativeSyntax :: ScopedName
rdftTestTurtleEval = "http://www.w3.org/ns/rdftest#TestTurtleEval"
rdftTestTurtleNegativeEval = "http://www.w3.org/ns/rdftest#TestTurtleNegativeEval"

rdftTestTurtlePositiveSyntax ="http://www.w3.org/ns/rdftest#TestTurtlePositiveSyntax"
rdftTestTurtleNegativeSyntax = "http://www.w3.org/ns/rdftest#TestTurtleNegativeSyntax"

rdftTestNTriplesPositiveSyntax, rdftTestNTriplesNegativeSyntax :: ScopedName
rdftTestNTriplesPositiveSyntax = "http://www.w3.org/ns/rdftest#rdftTestNTriplesPositiveSyntax"
rdftTestNTriplesNegativeSyntax = "http://www.w3.org/ns/rdftest#rdftTestNTriplesNegativeSyntax"

{-
rdftApproval :: RDFLabel
rdftApproval = u2L
               "http://www.w3.org/ns/rdftest#approval"
-}

-- | Extract out the object from the list of triples,
--   erroring out if there is not a single match.
getVal ::
  ScopedName  -- ^ predicate to search for
  -> [RDFTriple]
  -> Either String RDFLabel -- ^ object, if found
getVal p ts = 
  let ns = filter ((== Res p) . arcPred) ts
  in case ns of
    [n] -> Right $ arcObj n
      
    [] -> Left $ "No " ++ show p ++ " predicate found"
    _ -> Left $ "Found multiple " ++ show p ++ " attributes"

-- | Note: assuming that the literals are untyped at the moment.
toString :: RDFLabel -> Either String String
toString (Lit s) = Right $ T.unpack s
toString (LangLit s _) = Right $ T.unpack s
toString (TypedLit s dt) | dt == xsdString = Right $ T.unpack s
                         | otherwise = Left $ "Not a string, but " ++ show dt
toString v = Left $ "Not a string literal, but " ++ show v                                       

toTestType :: RDFLabel -> Either String TestType
toTestType (Res r) | r == rdftTestTurtleEval = Right TurtleCompare
                   | r == rdftTestTurtlePositiveSyntax = Right $ TurtleParse True
                   | r == rdftTestTurtleNegativeSyntax = Right $ TurtleParse False
                   | r == rdftTestTurtleNegativeEval = Right $ TurtleParse False
                   | r == rdftTestNTriplesPositiveSyntax = Right $ NTriplesParse True
                   | r == rdftTestNTriplesNegativeSyntax = Right $ NTriplesParse False
                   | otherwise = Left $ "Unrecognized test type: " ++ show r
toTestType x = Left $ "Not a resource, but " ++ show x

getScheme, getPath :: ScopedName -> String
getScheme = uriScheme . getScopeURI
getPath = uriPath . getScopeURI

toFilePath :: RDFLabel -> Either String FilePath
toFilePath (Res r) | getScheme r == "file:" = Right $ getPath r
                   | otherwise = Left $ "Not a file URL: " ++ show r
toFilePath x = Left $ "Not a resource, but " ++ show x

-- | Indicates that the details of the test in the manifest graph
--   do not contain the required details.
failedAction :: String -> IO (Maybe String)
failedAction = return . Just

pass :: IO (Maybe String)
pass = putStrLn "[PASS]" >> return Nothing

nopass :: String -> IO (Maybe String)
nopass e = putStrLn "[FAIL]" >> failedAction e

-- Ensure that the string ends in a space; it may exceed
-- 60 characters (ASCII) wide.
ljust :: String -> IO ()
ljust m =
  putStr $ m ++ replicate (59 - length m) ' ' ++ " "

-- | Compare the two files.
evalAction ::
  String       -- ^ test name
  -> FilePath  -- ^ turtle file (to test)
  -> FilePath  -- ^ NTriples file (to compare against)
  -> IO (Maybe String)
  -- ^ If the test fpasses return @Nothing@, otherwise
  --   a string descibing the error
evalAction name tFile nFile = do
  ljust $ "*** <COMPARE> " ++ name
  cts1 <- L.readFile tFile
  cts2 <- L.readFile nFile
  let filename = snd $ splitFileName tFile
      Just frag = parseURIReference filename
      nbase = (frag `relativeTo`) `fmap` base
  let res = do
        tgr <- TTL.parseTurtle cts1 nbase
        ngr <- NT.parseNT cts2
        return $ if tgr == ngr
                 then Nothing
                 else
                   -- should look at Swish.Commands.swishOutputDiffs
                   -- but that is quite involved, so just dump the
                   -- two graphs, which should be small
                   let f = concatMap show . S.toList . getArcs
                   in Just $ name ++ "\nDoes not compare equal.\nExpected:\n" ++
                      f ngr ++ "\nTurtle:\n" ++ f tgr
        
  case res of
    Left e -> nopass (name ++ "\nParse failure:\n" ++ e)
    Right Nothing -> pass
    Right (Just e) -> nopass e
    
-- | Does the file parse?
--
--   TODO: should we ensure the graph is evaluated to make sure
--         that laziness does not catch us out here?
evalSyntaxPass ::
  (L.Text -> Either String a) -- ^ parser to test
  -> String    -- ^ test name
  -> FilePath  -- ^ turtle file (to test)
  -> IO (Maybe String)
evalSyntaxPass parser name tFile = do
  ljust $ "*** <PASS> " ++ name
  cts <- L.readFile tFile
  case parser cts of
    Left e -> nopass (name ++ "\n" ++ e)
    Right _ -> pass
    
-- | Does the file fail to parse?
--
--   TODO: should we ensure the graph is evaluated to make sure
--         that laziness does not catch us out here?
evalSyntaxFail ::
  (L.Text -> Either String a) -- ^ parser to test
  -> String    -- ^ test name
  -> FilePath  -- ^ turtle file (to test)
  -> IO (Maybe String)
evalSyntaxFail parser name tFile = do
  ljust $ "*** <FAIL> " ++ name
  cts <- L.readFile tFile
  case parser cts of
    Left _ -> pass
    Right _ -> nopass (name ++ "\nShould not have parsed, but it did!")
    
-- | Create a test for the given label. For now ignore the
--   approved field.
makeTest :: RDFGraph -> RDFLabel -> Test
makeTest gr lbl =
  let arcs = rdfFindArcs (rdfSubjEq lbl) gr

      getMetaData = do
        testName <- getVal mfName arcs >>= toString
        testType <- getVal rdfType arcs >>= toTestType
        return (testName, testType)

      getAction name (NTriplesParse b) = do
        inFile <- getVal mfAction arcs >>= toFilePath
        return $ if b
                 then evalSyntaxPass NT.parseNT name inFile
                 else evalSyntaxFail NT.parseNT name inFile
      
      getAction name (TurtleParse b) = do
        inFile <- getVal mfAction arcs >>= toFilePath
        return $ if b
                 then evalSyntaxPass TTL.parseTurtlefromText name inFile
                 else evalSyntaxFail TTL.parseTurtlefromText name inFile
      
      getAction name TurtleCompare = do
        inFile <- getVal mfAction arcs >>= toFilePath
        outFile <- getVal mfResult arcs >>= toFilePath
        return $ evalAction name inFile outFile
        
  in case getMetaData of
    Left e -> Test "Failed to build test" $ failedAction $ "No test data found: " ++ e 
    Right (n,t) -> case getAction n t of
      Left e -> Test n $ failedAction $ "Failed to build test " ++ n ++ ": " ++ e
      Right a -> Test n a

makeTests :: RDFGraph -> [Test]
makeTests gr =
  let [Arc _ _ ehead] = rdfFindArcs (rdfPredEq (Res mfEntries)) gr
  in map (makeTest gr) $ rdfFindList gr ehead

readManifest :: FilePath -> IO [Test]
readManifest fname = do
  putStrLn $ "Reading manifest: " ++ fname
  cts <- L.readFile fname
  path <- canonicalizePath fname
  let (dName, _) = splitFileName path
      baseName = parseURI $ "file://" ++ dName
  case baseName of
    Just bn -> putStrLn $ "Using as base: " ++ show bn
    _ -> hPutStrLn stderr ("Unable to convert " ++ dName ++ " to a base URI!")
         >> exitFailure
  case TTL.parseTurtle cts baseName of
    Left e -> hPutStrLn stderr ("Error parsing " ++ fname)
              >> hPutStrLn stderr ("--> " ++ e)
              >> exitFailure
    Right gr -> return $ makeTests gr
      
      
main :: IO ()
main = do
  -- As there's no command-line options, always display the version
  putStrLn $ "Swish library: " ++ showVersion version
  args <- getArgs
  case args of
    [fname] -> readManifest fname >>= runTests
    _ -> do
      pName <- getProgName
      hPutStrLn stderr $ "Usage: " ++ pName ++ " <manifest file>"
      exitFailure

      