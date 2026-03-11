module Main where

import Test.Tasty
import Test.Tasty.KAT
import Test.Tasty.KAT.FileLoader

import qualified Data.ByteString.Char8 as BC

main = do
    kat <- testKatLoad "tests/KAT" katLoaderSimple
    defaultMain $ testGroup "tasty-kat"
        [ testKatDetailed "kat-detailed" kat myTest
        , testKatGrouped "kat-grouped" kat myTest
        ]
  where myTest group kvs =
            case group of
                "add" -> case sequence $ map (flip lookup kvs) ["a","b","r"] of
                            Just [as,bs,rs] ->
                                let a = read as :: Int
                                    b = read bs :: Int
                                    r = read rs :: Int
                                 in return (a + b == r)
                            _ -> error ("invalid vector " ++ show kvs)
                "sub" -> case sequence $ map (flip lookup kvs) ["a","b","r"] of
                            Just [as,bs,rs] ->
                                let a = read as :: Int
                                    b = read bs :: Int
                                    r = read rs :: Int
                                 in return (a - b == r)
                            _ -> error "invalid vector"
                "base64" ->
                    case sequence $ map (flip lookup kvs) ["a","r"] of
                            Just [as,rs] ->
                                return ((valueUnbase64 as) == (BC.pack rs))
                            _ -> error "invalid vector"
                
                _ -> error ("unknown group: " ++ group)
