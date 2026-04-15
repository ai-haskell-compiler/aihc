{- ORACLE_TEST xfail reason="configurator-export uses a case alternative continuation line beginning with infix operator $+$ that the parser rejects" -}

module M where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Text.PrettyPrint (Doc, ($+$), (<+>))
import qualified Text.PrettyPrint as P

keysToDoc :: NonEmpty (String, Either Int String) -> Doc
keysToDoc l@((_, Right _) :| _) =
  P.vcat . addSep 0 $
    [groupToDoc n g | (n, Right g) <- toList l]
  where
    addSep _ = id
    groupToDoc k g =
      case True of
        True -> P.text k <+> P.lbrace
        False -> P.text k $+$ P.lbrace
        $+$ P.nest 4 (P.text g)
        $+$ P.rbrace
