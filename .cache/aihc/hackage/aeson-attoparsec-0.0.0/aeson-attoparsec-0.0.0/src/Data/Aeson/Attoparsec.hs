module Data.Aeson.Attoparsec where

import Data.Aeson (FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Attoparsec.Text as Attoparsec



attoAeson :: Attoparsec.Parser a -> Value -> Aeson.Parser a
attoAeson parser (String x) = case parseOnly parser x of
  Left e -> fail e
  Right x -> pure x
attoAeson _ x = typeMismatch "Attoparsec" x
