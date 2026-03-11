module TestImport
  ( testParseMode
  , testPrintMode
  , Name(..)
  , ValueExpr(..)
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Language.Egison.Syntax.Pattern.Expr
                                               as X
import           Language.Egison.Parser.Pattern
                                               as X
                                                ( Parsable(..) )
import           Language.Egison.Pretty.Pattern
                                               as X
                                                ( prettyExpr )

-- main
import           Data.Text                      ( pack )
import           Data.Functor                   ( void )
import           Data.Void                      ( Void )
import           Control.Applicative            ( (<|>)
                                                , some
                                                )

import           Text.Megaparsec                ( Parsec )
import qualified Text.Megaparsec               as Parsec
                                                ( chunk
                                                , single
                                                , parse
                                                )
import qualified Text.Megaparsec.Char          as Parsec
                                                ( letterChar )
import qualified Text.Megaparsec.Char.Lexer    as Parsec
                                                ( decimal )

import           Language.Egison.Syntax.Pattern.Expr
                                                ( Expr(..) )
import           Language.Egison.Parser.Pattern ( ParseMode(..)
                                                , ParseFixity(..)
                                                , Fixity(..)
                                                , Precedence(..)
                                                , Associativity(..)
                                                )
import           Language.Egison.Pretty.Pattern ( PrintMode(..)
                                                , PrintFixity(..)
                                                )


newtype Name = Name String
  deriving (Show, Eq, Ord)

newtype ValueExpr = ValueExprInt Int
  deriving newtype Num
  deriving stock (Show, Eq)

unParsec :: Parsec Void String a -> (String -> Either String a)
unParsec p input = case Parsec.parse p "test" input of
  Left  e -> Left (show e)
  Right x -> Right x

testFixities :: [Fixity Name]
testFixities =
  [ Fixity AssocRight (Precedence 5) (Name "++")
  , Fixity AssocRight (Precedence 5) (Name ":")
  , Fixity AssocLeft  (Precedence 4) (Name "|>")
  , Fixity AssocRight (Precedence 4) (Name "<|")
  ]

toParseFixity :: Fixity Name -> ParseFixity Name String
toParseFixity fixity@(Fixity _ _ (Name name)) = ParseFixity fixity
  $ unParsec parser
  where parser = void $ Parsec.chunk name

toPrintFixity :: Fixity Name -> PrintFixity Name
toPrintFixity fixity@(Fixity _ _ (Name name)) = PrintFixity fixity $ pack name

testParseName :: Parsec Void String Name
testParseName = withParens <|> name
 where
  name       = Name <$> some Parsec.letterChar
  ops        = Parsec.chunk "++"
  withParens = do
    op <- Parsec.single '(' *> ops <* Parsec.single ')'
    pure $ Name op

testParseValueExpr :: Parsec Void String ValueExpr
testParseValueExpr = withParens <|> dec
 where
  dec        = ValueExprInt <$> Parsec.decimal
  withParens = do
    d <- Parsec.chunk "(-" *> dec <* Parsec.single ')'
    pure $ negate d

testParseMode :: ParseMode Name Name ValueExpr String
testParseMode = ParseMode { filename        = "test"
                          , fixities        = map toParseFixity testFixities
                          , blockComment    = Just ("{-", "-}")
                          , lineComment     = Just "--"
                          , varNameParser   = unParsec testParseName
                          , nameParser      = unParsec testParseName
                          , valueExprParser = unParsec testParseValueExpr
                          }

testPrintMode :: PrintMode Name Name ValueExpr
testPrintMode = PrintMode { fixities         = map toPrintFixity testFixities
                          , varNamePrinter   = namePrinter
                          , namePrinter
                          , valueExprPrinter = valueExprPrinter
                          , pageMode         = Nothing
                          }
 where
  namePrinter (Name name) = pack name
  valueExprPrinter (ValueExprInt i) = pack $ show i
