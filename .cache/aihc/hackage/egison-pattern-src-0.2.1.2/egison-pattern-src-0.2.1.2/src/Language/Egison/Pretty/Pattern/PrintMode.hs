-- |
--
-- Module:      Language.Egison.Pretty.Pattern.PrintMode
-- Description: Printer configuration
-- Stability:   experimental
--
-- A printer configuration type, that contains a set of external printers

module Language.Egison.Pretty.Pattern.PrintMode
  ( ExtPrinter
  , PrintFixity(..)
  , PrintMode(..)
  , PageMode(..)
  , Fixity(..)
  )
where

import           Data.Text                      ( Text )

import           Language.Egison.Syntax.Pattern.Fixity
                                                ( Fixity(..) )


-- | @'ExtPrinter' a@ is a type for externally provided printer of @a@.
type ExtPrinter a = a -> Text

-- | Fixity of infix operators.
data PrintFixity n =
  PrintFixity { fixity  :: Fixity n
              , printed :: Text
              }

-- | Rendering style configuration.
data PageMode =
  PageMode { lineLength :: Int
           , ribbonsPerLine :: Double
           }

-- | Printer configuration.
data PrintMode n v e
  = PrintMode { fixities         :: [PrintFixity n]
              , varNamePrinter   :: ExtPrinter v
              , namePrinter      :: ExtPrinter n
              , valueExprPrinter :: ExtPrinter e
              , pageMode         :: Maybe PageMode
              }
