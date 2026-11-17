{-# LANGUAGE TemplateHaskell #-}

-- | A module below "Helper". It has a splice of its own, so the splice
-- package of "Main" compiles a module with a splice.
module Lower (Mode (..), lowerFile, lowerValue) where

import Language.Haskell.TH

data Mode = Fast | Slow

$( pure
     [ SigD (mkName "lowerValue") (ConT ''Int),
       ValD (VarP (mkName "lowerValue")) (NormalB (LitE (IntegerL 3))) []
     ]
 )

-- The splice package of "Main" uses the result of this splice from the
-- build of the program. Its copy of the module has a different file name.
$( do
     loc <- location
     pure
       [ SigD (mkName "lowerFile") (ConT ''String),
         ValD (VarP (mkName "lowerFile")) (NormalB (LitE (StringL (loc_filename loc)))) []
       ]
 )
