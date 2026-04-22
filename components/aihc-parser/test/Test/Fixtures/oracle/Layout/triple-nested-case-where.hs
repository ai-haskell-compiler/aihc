{- ORACLE_TEST pass -}
module TripleNestedCaseWhere where

-- where clause after triply nested case expressions.
-- Tests that closeBeforeWhere correctly closes all enclosing
-- LayoutCaseAlternative contexts.
f x y z =
  case x of
    True ->
      case y of
        True ->
          case z of
            True -> x
            False -> y
    where
        g = x
