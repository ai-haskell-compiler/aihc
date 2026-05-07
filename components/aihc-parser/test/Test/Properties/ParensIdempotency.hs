module Test.Properties.ParensIdempotency
  ( prop_declParensIdempotent,
    prop_exprParensIdempotent,
    prop_moduleParensIdempotent,
    prop_patternParensIdempotent,
    prop_typeParensIdempotent,
  )
where

import Aihc.Parser.Parens
  ( addDeclParens,
    addExprParens,
    addModuleParens,
    addPatternParens,
    addTypeParens,
  )
import Aihc.Parser.Syntax
import Data.Data (Data)
import Test.Properties.Arb.Decl ()
import Test.Properties.Arb.Expr ()
import Test.Properties.Arb.Module ()
import Test.Properties.Arb.Pattern ()
import Test.Properties.Arb.Type ()
import Test.QuickCheck

prop_moduleParensIdempotent :: Module -> Property
prop_moduleParensIdempotent = parensIdempotent addModuleParens

prop_declParensIdempotent :: Decl -> Property
prop_declParensIdempotent = parensIdempotent addDeclParens

prop_exprParensIdempotent :: Expr -> Property
prop_exprParensIdempotent = parensIdempotent addExprParens

prop_patternParensIdempotent :: Pattern -> Property
prop_patternParensIdempotent = parensIdempotent addPatternParens

prop_typeParensIdempotent :: Type -> Property
prop_typeParensIdempotent = parensIdempotent addTypeParens

parensIdempotent :: (Data a, Eq a, Show a) => (a -> a) -> a -> Property
parensIdempotent addParens value =
  let onceParenthesized = stripAnnotations (addParens value)
      twice = stripAnnotations (addParens (addParens value))
   in counterexample ("once:\n" <> show onceParenthesized <> "\n\ntwice:\n" <> show twice) (twice == onceParenthesized)
