{-# OPTIONS_GHC -fno-warn-partial-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Data.Diff.Types where

import Data.Text


data Edit = EditDelete { deleteFrom :: Int
                       , deleteTo :: Int }
          | EditInsert { insertPos :: Int
                       , insertFrom :: Int
                       , insertTo :: Int }
  deriving (Show, Eq)


-- * Types to mimic TextDocumentContentChangeEvent from the lsp-types package

data Position = Position { positionLine :: Int
                         , positionCh :: Int }
  deriving (Show, Eq)

data Range = Range { rangeStart :: Position
                   , rangeEnd :: Position }
  deriving (Show, Eq)

data ChangeEvent = ChangeEvent { range :: Range
                               , text :: Text }
  deriving (Show, Eq)
