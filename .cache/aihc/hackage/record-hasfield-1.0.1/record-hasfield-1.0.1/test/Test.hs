{-# LANGUAGE MultiParamTypeClasses
           , TypeApplications
           , DataKinds
           , FlexibleInstances
  #-}

module Main(main) where

import GHC.Records.Extra

newtype Foo = Foo {foo :: Int} deriving (Show,Eq)
instance HasField "foo" Foo Int where
    hasField r = (\x -> r{foo=x}, foo r)

newtype Bar = Bar {bar :: Foo} deriving (Show,Eq)
instance HasField "bar" Bar Foo where
    hasField r = (\x -> r{bar=x}, bar r)

main :: IO ()
main = do
    let a === b = if a == b then putStr "." else fail $ "Mismatch: " ++ show a ++ " /= " ++ show b
    modifyField @'("bar","foo") (Bar $ Foo 12) (*2) === Bar {bar = Foo {foo = 24}}
    getField @"foo" (Foo 3) === 3
    setField @"_1" (1, True) 8 === (8, True)
    modifyField @'("_1","_2") ((1,2),3,4,5) negate === ((1,-2),3,4,5)
    putStrLn " success"
