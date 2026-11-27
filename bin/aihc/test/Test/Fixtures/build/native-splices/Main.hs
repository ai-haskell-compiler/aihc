{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.Bits (finiteBitSize)
import Data.String (IsString (..))
import Language.Haskell.TH.Syntax (lift, runIO)

newtype Label = Label String
  deriving (Show)

instance IsString Label where
  fromString = Label

main :: IO ()
main = do
  -- A native splice has the word size of the host. A splice in the
  -- WebAssembly engine has 32 bits.
  print $(lift (finiteBitSize (0 :: Int)))
  -- A native splice can write to its standard output. The output must not
  -- go into the protocol of the engine.
  print $(runIO (putStrLn "output of a splice") >> lift (3 :: Int))
  -- The lift of a String is a string literal, as in GHC. Thus,
  -- OverloadedStrings gives it a type that is not String.
  print ($(lift ("text" :: String)) :: Label)
