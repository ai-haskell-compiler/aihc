{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import Text.Regex.PCRE.ByteString
import Text.Regex.PCRE.ByteString.Utils
import qualified Data.ByteString as BS

tests :: [(BS.ByteString, BS.ByteString, [BS.ByteString])]
tests = [ ("a"   , "lala"       , ["l","l"])
        , ("l"   , "lala"       , ["", "a", "a"])
        , ("x"   , "lala"       , ["lala"])
        , ("[la]", "lalaz"      , ["", "", "", "", "z"])
        , ("[la]", "lalazoula"  , ["", "", "", "", "zou"])
        , ("[la]", "lalazoulaz" , ["", "", "", "", "zou", "", "z"])
        , ("[la]", "lala"       , [])
        , ("l"   , "l\nl"       , ["", "\n"])
        , (""    , "abc"        , ["a", "b", "c"])
        , ("(ha)", "lahachac"      , ["la","c","c"])
        ]

toTest :: (BS.ByteString, BS.ByteString, [BS.ByteString]) -> IO Test
toTest cas@(regexp, string, result) = do
    let failtest :: String -> IO Test
        failtest rr = return $ TestCase $ assertFailure $ rr ++ " " ++ show cas
    x <- compile compBlank execBlank regexp
    case x of
        Left  rr -> failtest ("Failure when compiling regexp: " ++ show rr)
        Right rregexp -> do
            res <- split rregexp string
            case res of
                Right rs -> return (rs ~?= result)
                Left err -> failtest ("Could not split regexp: " ++ err)

main :: IO ()
main = do
    (Counts _ _ e f) <- fmap TestList (mapM toTest tests) >>= runTestTT
    if (e > 0) || (f > 0)
        then error "Tests failed"
        else return ()
