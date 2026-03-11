-- |
--
-- Module:      Language.Egison.Pretty.Pattern.Prim
-- Description: Primitive printers
-- Stability:   experimental
--
-- Primitive printers.
--
-- Note that all dependencies on printer library are in this module.

module Language.Egison.Pretty.Pattern.Prim
  ( Doc
  , renderDoc
  , text
  -- * Re-exports
  , module X
  )
where

-- re-exports
import           Data.Text.Prettyprint.Doc     as X
                                                ( parens
                                                , hsep
                                                , list
                                                , tupled
                                                , (<+>)
                                                )

-- main
import           Data.Void                      ( Void )
import           Data.Text                      ( Text )

import qualified Data.Text.Prettyprint.Doc     as PP
                                                ( Doc
                                                , PageWidth(..)
                                                , LayoutOptions(..)
                                                , pretty
                                                , layoutPretty
                                                )
import qualified Data.Text.Prettyprint.Doc.Render.Text
                                               as PP
                                                ( renderStrict )

import           Language.Egison.Pretty.Pattern.PrintMode
                                                ( PrintMode(..)
                                                , PageMode(..)
                                                )


type Doc = PP.Doc Void

text :: Text -> Doc
text = PP.pretty

renderDoc :: PrintMode n v e -> Doc -> Text
renderDoc PrintMode { pageMode } doc = PP.renderStrict stream
 where
  stream          = PP.layoutPretty layoutOptions doc
  layoutOptions   = PP.LayoutOptions { PP.layoutPageWidth }
  layoutPageWidth = case pageMode of
    Just PageMode { lineLength, ribbonsPerLine } ->
      PP.AvailablePerLine lineLength ribbonsPerLine
    Nothing -> PP.Unbounded
