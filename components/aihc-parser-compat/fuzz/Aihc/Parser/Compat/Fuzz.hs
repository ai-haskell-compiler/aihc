-- | Continuously runnable QuickCheck properties owned by @aihc-parser-compat@.
module Aihc.Parser.Compat.Fuzz
  ( parserCompatFuzzProperties,
  )
where

import Test.Compat.Decl (prop_declCompat)
import Test.Compat.Expr (prop_exprCompat)
import Test.QuickCheck (Property, Testable, property)

parserCompatFuzzProperties :: [(String, Property)]
parserCompatFuzzProperties =
  [ named "generated expr converts to normalized GHC parsed AST" prop_exprCompat,
    named "generated decl converts to normalized GHC parsed AST" prop_declCompat
  ]
  where
    named :: (Testable prop) => String -> prop -> (String, Property)
    named name value = (name, property value)
