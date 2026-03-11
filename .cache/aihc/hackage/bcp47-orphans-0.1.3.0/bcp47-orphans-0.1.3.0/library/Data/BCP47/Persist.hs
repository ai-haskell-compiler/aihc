{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Persist () where

import Control.Monad ((<=<))
import Data.BCP47 (BCP47, fromText, toText)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Database.Persist.Class (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))

instance PersistField BCP47 where
  toPersistValue = toPersistValue . toText
  fromPersistValue = fromText <=< fromPersistValue

instance PersistFieldSql BCP47 where
  -- sqlType for Text should be SqlString, but we don't hardcode in the
  -- unlikely case that changes
  sqlType _ = sqlType @Text Proxy
