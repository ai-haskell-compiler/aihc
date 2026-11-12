{-# LANGUAGE TemplateHaskell #-}

-- | A module below "Helper". It has a splice of its own, so the splice
-- package of "Main" compiles a module with a splice.
module Lower (Mode (..), lowerValue) where

import Language.Haskell.TH

data Mode = Fast | Slow

$( pure
     [ SigD (mkName "lowerValue") (ConT ''Int),
       ValD (VarP (mkName "lowerValue")) (NormalB (LitE (IntegerL 3))) []
     ]
 )
