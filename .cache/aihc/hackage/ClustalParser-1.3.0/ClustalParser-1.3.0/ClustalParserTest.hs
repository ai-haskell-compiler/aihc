--runghc -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d  ClustalParserTest.hs struct-multiline.mlocarna
module Main where

import System.Environment (getArgs)
import Biobase.Clustal.Import
--import Data.Either.Unwrap

main :: IO ()
main = do
  args <- getArgs
  let input_file = (head args)
  -- read Clustal outputfile
  parsedinput <- readStructuralClustalAlignment input_file
  print parsedinput
  --print $ structuralAlignmentEntries (fromRight parsedinput)
