{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad          (forM_, when)
import           Data.Either            (isRight)
import qualified Data.Map               as M
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

import qualified Data.ByteString        as S
import qualified Data.ByteString.Base16 as SB16

import           Test.Hspec
import qualified Test.Tasty             as Tasty
import           Test.Tasty.Hspec

import           Data.RLP
import           GenericRLPTest
import           RLPTest

main :: IO ()
main = do
    putStrLn "" -- cabal doesn't put a \n before running the test and that makes me D:<
    test <- testSpec "ReLaPse" (genericSpec >> specsFromJSON)
    Tasty.defaultMain test

specsFromJSON :: Spec
specsFromJSON = do
    it "should be able to load the embedded tests" $
        officialRLPTests `shouldSatisfy` isRight

    case officialRLPTests of
        Left err -> runIO . putStrLn $ "Could not load official tests: " ++ err
        Right tests ->
            describe "Official RLP Tests" $ forM_ tests makeTest

makeTest :: (T.Text, RLPTest) -> SpecWith ()
makeTest (name, test) = describe (T.unpack name) $ do
    let theInput  = input test
        preOutput = output test
        decOutput = SB16.decode (TE.encodeUtf8 preOutput)

    it "should fully read the base-16 encoded expected output in the test case" $
        decOutput `shouldSatisfy` didDecodeFully

    when (didDecodeFully decOutput) $ do
        let Right theOutput = decOutput
        describe "the input" $ do
            let theInput  = input test
                theEncode = rlpEncode theInput
                theDecode = rlpDecode theEncode

            it "should decode to something after being encoded" $
                theDecode `shouldSatisfy` isRight

            case theDecode of
                Left err -> return ()
                Right decoded -> do
                    it "should maintain (decode . encode) == id" $
                        decoded `shouldBe` theInput

                    it "should serialize to the expected output" $ do
                        let packed = packRLP theEncode
                        packed `shouldBe` theOutput

        describe "the output" $ do
            let unpackedOutput = unpackRLPFully theOutput
            it "should unpack completely to an RLP object" $
                unpackedOutput `shouldSatisfy` isRight

            case unpackedOutput of
                Left err -> return ()
                Right unpackedRlpObj -> do
                    let decodedUnpack = rlpDecode unpackedRlpObj
                    it "should decode to something that can satisfy the input's type" $
                        decodedUnpack `shouldSatisfy` isRight

                    case decodedUnpack of
                        Left err -> return ()
                        Right du' ->
                            it "should decode back to the input" $
                                du' `shouldBe` theInput


didDecodeFully :: Either String S.ByteString -> Bool
didDecodeFully = isRight
