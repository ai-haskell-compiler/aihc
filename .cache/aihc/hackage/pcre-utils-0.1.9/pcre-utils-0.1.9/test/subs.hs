{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils
import qualified Data.ByteString as BS

tests :: [(BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString)]
tests = [ ("l", "x", "lap\\nin", "xap\\nin")
        , ("log(\\d+)truc", "x\\1x", "log186truc", "x186x")
        , ("'", "'\\''", "# This file is managed by Puppet. DO NOT EDIT.", "# This file is managed by Puppet. DO NOT EDIT.")
        , ("test", "x\\0x", "long test replacement but test", "long xtestx replacement but xtestx")
        , ("(([0-9]+))", "<\\1>", "1.2.3.4", "<1>.<2>.<3>.<4>")
        ]

toTest :: (BS.ByteString, BS.ByteString, BS.ByteString, BS.ByteString) -> IO Test
toTest cas@(regexp, replacement, string, result) = do
    let failtest :: String -> IO Test
        failtest rr = return $ TestCase $ assertFailure $ rr ++ " " ++ show cas
    x <- compile compBlank execBlank regexp
    case x of
        Left  rr -> failtest ("Failure when compiling regexp: " ++ show rr)
        Right rregexp -> do
            res <- substitute rregexp string replacement
            case res of
                Right rs -> return (rs ~?= result)
                Left err -> failtest ("Could not split regexp: " ++ err)

main :: IO ()
main = do
    (Counts _ _ e f) <- fmap TestList (mapM toTest tests) >>= runTestTT
    if (e > 0) || (f > 0)
        then error "Tests failed"
        else return ()
