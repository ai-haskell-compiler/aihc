module TestLib where

import Language.Haskell.TH

{-# ANN basicSuccess "This is a test annotation" #-}
basicSuccess :: Q String
basicSuccess = return "Success"

basicFailure :: Q String
basicFailure = fail "Failure"
