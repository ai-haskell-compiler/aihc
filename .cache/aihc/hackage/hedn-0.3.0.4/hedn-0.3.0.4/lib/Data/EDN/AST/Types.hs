module Data.EDN.AST.Types
  ( Parser
  , module Data.EDN.AST.Types.Value
  , module Data.EDN.AST.Types.Tagged
  ) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

import Data.EDN.AST.Types.Tagged
import Data.EDN.AST.Types.Value

type Parser = Parsec Void Text
