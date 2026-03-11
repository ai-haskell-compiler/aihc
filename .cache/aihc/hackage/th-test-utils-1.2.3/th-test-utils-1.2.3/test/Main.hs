{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextL
import qualified Data.Text.Lazy.Encoding as TextL
import Language.Haskell.TH (AnnLookup (..), Q, Type (..), runIO)
import Language.Haskell.TH.Syntax (Extension (..), ForeignSrcLang (..), Quasi (..))
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

import Language.Haskell.TH.TestUtils
import Language.Haskell.TH.TestUtils.QMode (IsMockedMode, TestQResult)
import TH
import TestLib

main :: IO ()
main =
  defaultMain $
    testGroup
      "th-test-utils"
      [ testAllowQ
      , testMockQ
      , testMockQAllowIO
      ]

testAllowQ :: TestTree
testAllowQ =
  testGroup
    "AllowQ"
    [ testRunners
    , testMethods
    ]
  where
    testRunners =
      testGroup
        "tryTestQ, runTestQ, runTestQErr"
        [ $( testCaseTH "tryTestQ - success" $ do
               let x = 1
               result <- tryTestQ unmockedState (return x :: Q Int)
               return $ result @?= Right x
           )
        , $( testCaseTH "tryTestQ - error" $ do
               let msg = "Error message"
               result <- tryTestQ unmockedState (fail msg :: Q Int)
               return $ result @?= Left msg
           )
        , $( testCaseTH "fmap Right . runTestQ === tryTestQ" $ do
               actual <- Right <$> runTestQ unmockedState basicSuccess
               expected <- tryTestQ unmockedState basicSuccess
               return $ actual @?= expected
           )
        , $( testCaseTH "runTestQ errors on failure" $ do
               let msg = "Error message"
               result <- tryQ $ runTestQ unmockedState (fail msg :: Q ())
               return $ result @?= Left msg
           )
        , $( testCaseTH "fmap Left . runTestQErr === tryTestQ" $ do
               actual <- Left <$> runTestQErr unmockedState basicFailure
               expected <- tryTestQ unmockedState basicFailure
               return $ actual @?= expected
           )
        , $( testCaseTH "runTestQErr errors on success" $ do
               result <- tryQ $ runTestQErr unmockedState (return 1 :: Q Int)
               return $ result @?= Left "Unexpected success: 1"
           )
        ]
    testMethods =
      testGroup
        "Quasi methods"
        [ $( testCaseTH "qNewName" $
               isSuccess $
                 runTestQ unmockedState $
                   qNewName "foo"
           )
        , $( testCaseTH "qReport" $
               -- not testing 'qReport False' because it fails with '-Werror'
               -- isSuccess $ runTestQ unmockedState $ qReport False "A warning message"
               isSuccess $
                 runTestQ unmockedState $
                   qReport True "An error message"
           )
        , $( testCaseTH "qRecover" $ do
               let x = "Success"
               result <- runTestQ unmockedState $ qRecover (return x) basicFailure
               return $ result @?= x
           )
        , $( testCaseTH "qLookupName" $
               checkUnmockedMatches $
                 sequence
                   [ qLookupName True "Show"
                   , qLookupName True "Right"
                   , qLookupName False "Right"
                   , qLookupName False "Show"
                   ]
           )
        , $( testCaseTH "qReify" $
               checkUnmockedMatches $
                 qReify ''Maybe
           )
        , $( testCaseTH "qReifyFixity" $
               checkUnmockedMatches $
                 qReifyFixity '($)
           )
        , $( testCaseTH "qReifyType" $
               checkUnmockedMatches $
                 qReifyType ''Maybe
           )
        , $( testCaseTH "qReifyInstances" $
               checkUnmockedMatches $
                 qReifyInstances ''Show [ConT ''Int]
           )
        , $( testCaseTH "qReifyRoles" $
               checkUnmockedMatches $
                 qReifyRoles ''Maybe
           )
        , $( testCaseTH "qReifyAnnotations" $
               checkUnmockedMatches $
                 qReifyAnnotations @_ @[String] (AnnLookupName 'basicSuccess)
           )
        , $( testCaseTH "qReifyModule" $
               checkUnmockedMatches $
                 qReifyModule $(thisModule)
           )
        , $( testCaseTH "qReifyConStrictness" $
               checkUnmockedMatches $
                 qReifyConStrictness 'Just
           )
        , $( testCaseTH "qLocation" $
               checkUnmockedMatches qLocation
           )
        , $( testCaseTH "qRunIO, qAddDependentFile" $
               checkUnmockedMatches $ do
                 readmeContents <- qRunIO $ readFile "README.md"
                 qAddDependentFile "README.md"
                 return readmeContents
           )
        , $( testCaseTH "qAddTopDecls" $ do
               decs <- [d|{-# ANN module "AllowQ - qAddTopDecls" #-}|]
               isSuccess $ runTestQ unmockedState $ qAddTopDecls decs
           )
        , $( testCaseTH "qAddTempFile, qAddForeignFilePath" $ isSuccess $ do
               fp <- runTestQ unmockedState $ qAddTempFile "c"
               qRunIO $ writeFile fp "#include <stdio.h>"
               runTestQ unmockedState $ qAddForeignFilePath LangC fp
           )
        , $( testCaseTH "qAddModFinalizer" $
               isSuccess $
                 runTestQ unmockedState $
                   qAddModFinalizer $
                     return ()
           )
        , -- Not testing qAddCorePlugin because I don't want to actually register plugins here

          $( testCaseTH "qGetQ, qPutQ" $
               checkUnmockedMatches $ do
                 qPutQ "Hello"
                 qGetQ @_ @String
           )
        , $( testCaseTH "qIsExtEnabled" $
               checkUnmockedMatches $
                 sequence
                   [ qIsExtEnabled JavaScriptFFI
                   , qIsExtEnabled TemplateHaskell
                   ]
           )
        , $( testCaseTH "qExtsEnabled" $
               checkUnmockedMatches qExtsEnabled
           )
        ]

testMockQ :: TestTree
testMockQ =
  testMockQ'
    MockQTests
      { qMode = MockQ
      , toIO = pure
      , qRunIOResult = \_ -> Left "IO actions not allowed"
      }

testMockQAllowIO :: TestTree
testMockQAllowIO =
  testMockQ'
    MockQTests
      { qMode = MockQAllowIO
      , toIO = id
      , qRunIOResult = Right
      }

{- Helpers -}

data MockQTests mode = MockQTests
  { qMode :: QMode mode
  , toIO :: forall a. TestQResult mode a -> IO a
  , qRunIOResult :: forall a. a -> Either String a
  }

-- | Tests for both MockQ and MockQAllowIO, since they should behave exactly the same except for qRunIO.
testMockQ' :: forall mode. (IsMockedMode mode) => MockQTests mode -> TestTree
testMockQ' MockQTests{..} =
  testGroup
    (show qMode)
    [ testRunners
    , testMethods
    ]
  where
    mockedState =
      QState
        { mode = qMode
        , knownNames = []
        , reifyInfo = []
        }

    -- Call runTestQ, converting the result to IO and ensuring that the result is evaluated
    runTestQ' :: QState mode -> Q a -> IO a
    runTestQ' state = forceM . toIO . runTestQ state

    -- Call runTestQ', capturing any 'error' calls that occur in evaluating the result
    runTestQWithErrors :: QState mode -> Q a -> IO (Either String a)
    runTestQWithErrors state = tryIO . runTestQ' state

    testRunners =
      testGroup
        "tryTestQ, runTestQ, runTestQErr"
        [ testCase "tryTestQ - success" $ do
            let x = 1
            result <- toIO $ tryTestQ mockedState (return x :: Q Int)
            (result :: Either String Int) @?= Right x
        , testCase "tryTestQ - error" $ do
            let msg = "Error message"
            result <- toIO $ tryTestQ mockedState (fail msg :: Q Int)
            (result :: Either String Int) @?= Left msg
        , testCase "Right . runTestQ === tryTestQ" $ do
            actual <- fmap Right $ toIO $ runTestQ mockedState basicSuccess
            expected <- toIO $ tryTestQ mockedState basicSuccess
            (actual :: Either String String) @?= (expected :: Either String String)
        , testCase "runTestQ errors on failure" $ do
            let msg = "Error message"
            result <- tryIO $ forceM $ toIO $ runTestQ mockedState (fail msg :: Q ())
            (result :: Either String ()) @?= Left msg
        , testCase "Left . runTestQErr === tryTestQ" $ do
            actual <- fmap Left $ toIO $ runTestQErr mockedState basicFailure
            expected <- toIO $ tryTestQ mockedState basicFailure
            (actual :: Either String String) @?= (expected :: Either String String)
        , testCase "runTestQErr errors on success" $ do
            result <- tryIO $ forceM $ toIO $ runTestQErr mockedState (return 1 :: Q Int)
            (result :: Either String String) @?= Left "Unexpected success: 1"
        ]

    testMethods =
      testGroup
        "Quasi methods"
        [ golden "qNewName" $
            join $
              runTestQ' mockedState $ do
                name1 <- qNewName "foo"
                name2 <- qNewName "foo"
                name3 <- qNewName "bar"
                return $
                  labelled
                    [ ("Name 1", pure name1)
                    , ("Name 2", pure name2)
                    , ("Name 3", pure name3)
                    ]
        , testCase "qReport" $ do
            warningResult <- runTestQ' mockedState $ qReport False "A warning message"
            warningResult @?= ()

            errorResult <- runTestQ' mockedState $ qReport True "An error message"
            errorResult @?= ()
        , testCase "qRecover" $ do
            let x = "Success"
            result <- runTestQ' mockedState $ qRecover (return x) basicFailure
            result @?= x
        , testCase "qLookupName" $ do
            let state = mockedState{knownNames = [("Show", ''Show), ("Right", 'Right)]}

            nameShow <- runTestQ' state $ qLookupName True "Show"
            nameShow @?= Just ''Show

            nameEq <- runTestQ' state $ qLookupName True "Eq"
            nameEq @?= Nothing

            nameRight <- runTestQ' state $ qLookupName False "Right"
            nameRight @?= Just 'Right

            nameLeft <- runTestQ' state $ qLookupName False "Left"
            nameLeft @?= Nothing
        , golden "qReify" $ do
            let state = mockedState{reifyInfo = $(loadNames ['putStrLn])}
            labelled
              [ ("Found", runTestQWithErrors state $ qReify 'putStrLn)
              , ("Missing", runTestQWithErrors state $ qReify ''Show)
              ]
        , golden "qReifyFixity" $ do
            let state = mockedState{reifyInfo = $(loadNames ['($)])}
            labelled
              [ ("Found", runTestQWithErrors state $ qReifyFixity '($))
              , ("Missing", runTestQWithErrors state $ qReifyFixity '(+))
              ]
        , golden "qReifyType" $ do
            let state = mockedState{reifyInfo = $(loadNames [''Maybe])}
            labelled
              [ ("Found", runTestQWithErrors state $ qReifyType ''Maybe)
              , ("Missing", runTestQWithErrors state $ qReifyType ''Show)
              ]
        , golden "qReifyInstances" $
            runUnsupported mockedState $
              qReifyInstances ''Show [ConT ''Int]
        , golden "qReifyRoles" $ do
            let state = mockedState{reifyInfo = $(loadNames [''Maybe, 'map])}
            labelled
              [ ("Found", runTestQWithErrors state $ qReifyRoles ''Maybe)
              , -- reifyRoles errors if name is not of a type constructor
                ("Invalid", runTestQWithErrors state $ qReifyRoles 'map)
              , ("Missing", runTestQWithErrors state $ qReifyRoles ''Show)
              ]
        , golden "qReifyAnnotations" $
            runUnsupported mockedState $
              qReifyAnnotations @_ @[String] (AnnLookupName 'basicSuccess)
        , golden "qReifyModule" $
            runUnsupported mockedState $
              qReifyModule $(thisModule)
        , golden "qReifyConStrictness" $
            runUnsupported mockedState $
              qReifyConStrictness 'Just
        , golden "qLocation" $
            runUnsupported mockedState qLocation
        , testCase "qRunIO" $ do
            let x = 1 :: Int
                io = return x

            qRunIOResult' <- runTestQWithErrors mockedState $ qRunIO io
            qRunIOResult' @?= qRunIOResult x

            runIOResult <- runTestQWithErrors mockedState $ runIO io
            runIOResult @?= qRunIOResult x

            liftIOResult <- runTestQWithErrors mockedState $ liftIO io
            liftIOResult @?= qRunIOResult x
        , golden "qAddDependentFile" $
            runUnsupported mockedState $
              qAddDependentFile "README.md"
        , golden "qAddTopDecls" $
            runUnsupported mockedState $
              qAddTopDecls []
        , golden "qAddTempFile" $
            runUnsupported mockedState $
              qAddTempFile "c"
        , golden "qAddForeignFilePath" $
            runUnsupported mockedState $
              qAddForeignFilePath LangC "foo.c"
        , golden "qAddModFinalizer" $
            runUnsupported mockedState $
              qAddModFinalizer $
                return ()
        , golden "qAddCorePlugin" $
            runUnsupported mockedState $
              qAddCorePlugin "MyPlugin"
        , golden "qGetQ" $
            runUnsupported mockedState $
              qGetQ @_ @String
        , golden "qPutQ" $
            runUnsupported mockedState $
              qPutQ "Hello"
        , golden "qIsExtEnabled" $
            runUnsupported mockedState $
              qIsExtEnabled TemplateHaskell
        , golden "qExtsEnabled" $
            runUnsupported mockedState qExtsEnabled
        ]

    -- force both MockQ and MockQAllowIO to resolve the same goldens
    golden name =
      goldenVsString name ("test/goldens/MockQ_" ++ name ++ ".golden")
        . fmap (TextL.encodeUtf8 . TextL.fromStrict . normalizeModules)

    labelled :: (Show a) => [(String, IO a)] -> IO Text
    labelled vals = do
      let mkLine (label, getVal) = do
            val <- getVal
            return $ label ++ ": " ++ show val
      Text.pack . intercalate "\n" . map (++ "\n") <$> mapM mkLine vals

    runUnsupported :: (Show a) => QState mode -> Q a -> IO Text
    runUnsupported state q = labelled [("Unsupported", runTestQWithErrors state q)]

normalizeModules :: Text -> Text
normalizeModules s0 = foldr go s0 $ changes910 <> changes914
  where
    go (old, new) = Text.replace (Text.pack old) (Text.pack new)

#if __GLASGOW_HASKELL__ < 910
    changes910 =
      [ ("GHC.Show", "GHC.Internal.Show")
      , ("GHC.Base", "GHC.Internal.Base")
      , ("GHC.Num", "GHC.Internal.Num")
      , ("System.IO", "GHC.Internal.System.IO")
      ]
#else
    changes910 = []
#endif

#if __GLASGOW_HASKELL__ < 914
    changes914 =
      [ ("GHC.Types.IO", "GHC.Internal.Types.IO")
      ]
#else
    changes914 = []
#endif
