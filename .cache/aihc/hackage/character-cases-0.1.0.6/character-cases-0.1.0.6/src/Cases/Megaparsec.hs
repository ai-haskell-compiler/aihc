module Cases.Megaparsec where

import Data.Void
import Text.Megaparsec (MonadParsec)

type CodeParsing m = (MonadParsec Void String m, MonadFail m)
