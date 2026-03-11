{-# language DeriveDataTypeable #-}
{-# language LambdaCase #-}
module Control.Exception.Common where

import Control.Exception
-- import Control.Monad.Catch (MonadThrow (..))
import Data.Typeable -- (TypeRep, Typeable, typeRep)

-- | Exceptions related to loading/importing data
data ImportError = FileParseError String String deriving (Eq, Typeable)
instance Show ImportError where
  show = \case
    FileParseError s s2 -> unwords [s, ": File parse error:", s2]
instance Exception ImportError

-- | Exceptions related to serializing/storing data
data ExportError = FormatExportNotSupported String String deriving (Eq, Typeable)
instance Show ExportError where
  show = \case
    FormatExportNotSupported s sty -> unwords [s, ":", sty, "format not supported"]
instance Exception ExportError
