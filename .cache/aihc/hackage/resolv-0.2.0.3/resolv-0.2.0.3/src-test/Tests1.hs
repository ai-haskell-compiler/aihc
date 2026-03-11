{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ >= 900
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
#endif

module Main where

import           Control.Applicative as A
import qualified Control.Exception   as E
import           Control.Monad
import qualified Data.ByteString     as BS
import           System.Directory    (getDirectoryContents, removeFile)
import           System.FilePath     (dropExtension, takeExtension, (<.>),
                                      (</>))

import qualified Test.Tasty          as T
import qualified Test.Tasty.HUnit    as T

import qualified Network.DNS         as DNS

main :: IO ()
main = do
    msgfiles <- filter ((== ".bin") . takeExtension) <$> getDirectoryContents "testdata/msg"

    let tests1 = [ msgFileTest1 (dropExtension fn) | fn <- msgfiles ]
        tests2 = [ msgFileTest2 (dropExtension fn) | fn <- msgfiles ]

    T.defaultMain (T.testGroup "" [ T.testGroup "decode" tests1
                                  , T.testGroup "enc/dec" tests2
                                  , T.testGroup "Type/TypeSym"
                                    [ testTypeToFromSym1, testTypeToFromSym2 ]
                                  , T.testGroup "mkQueryRaw" [ mkQueryRawText1 ]
                                  ])

testTypeToFromSym1 :: T.TestTree
testTypeToFromSym1 = T.testCase "testTypeToFromSym1" $ do
    forM_ [minBound..maxBound] $ \sym -> do
        T.assertEqual "" (Just sym) (DNS.typeToSym . DNS.typeFromSym $ sym)

testTypeToFromSym2 :: T.TestTree
testTypeToFromSym2 = T.testCase "testTypeToFromSym2" $ do
    forM_ (map DNS.Type [minBound..maxBound]) $ \ty ->
        case DNS.typeToSym ty of
          Nothing  -> pure ()
          Just sym -> T.assertEqual "" (DNS.typeFromSym sym) ty

msgFileTest1 :: FilePath -> T.TestTree
msgFileTest1 fn = T.testCase fn $ do
    bs <- BS.readFile ("testdata" </> "msg" </> fn <.> "bin")
    msg1 <- assertJust "failed to decode message" $ DNS.decodeMessage bs

    -- load reference value
    let refFn = "testdata" </> "msg" </> fn <.> "show"
    writeFile (refFn ++ "~") (show (msg1 :: DNS.Msg DNS.Name))
    msg0 <- read <$> readFile refFn

    assertEqShow (pure ()) msg0 msg1
    removeFile (refFn ++ "~")

msgFileTest2 :: FilePath -> T.TestTree
msgFileTest2 fn = T.testCase fn $ do
    -- use this as reference message
    bs <- BS.readFile ("testdata" </> "msg" </> fn <.> "bin")
    msg0 <- assertJust "failed to decode stored message" $ DNS.decodeMessage bs

--    print msg0

    -- encode it now again
    let Just msg0bin = DNS.encodeMessage (msg0 :: DNS.Msg DNS.Labels)

    msg1 <- assertJust "failed to decode re-encoded message" $ DNS.decodeMessage msg0bin

    assertEqShow (pure ()) msg0 msg1

mkQueryRawText1 :: T.TestTree
mkQueryRawText1 = T.testCase "mkQueryRawText1" $ do
  msgraw <- DNS.mkQueryRaw DNS.classIN (DNS.Name "www.google.com") (DNS.typeFromSym DNS.TypeA)

  let Just msg = DNS.decodeMessage msgraw

  assertEqShow (pure ()) (head (DNS.msgQD msg)) (DNS.MsgQuestion (DNS.Name "www.google.com.") (DNS.Type 1) (DNS.Class 1))

assertJust :: String -> Maybe a -> IO a
assertJust msg Nothing  = T.assertFailure msg
assertJust _   (Just v) = A.pure v

assertEqShow :: Show a => IO () -> a -> a -> T.Assertion
assertEqShow onFail ref cur
  | show ref /= show cur  = do
        onFail
        T.assertFailure ("expected: " ++ show ref ++ "\n but got: " ++ show cur)
  | otherwise = A.pure ()
