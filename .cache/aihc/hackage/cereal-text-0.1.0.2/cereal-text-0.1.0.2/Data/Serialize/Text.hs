{-|
    Data.Text instances for cereal.

    Provides instances for Text and lazy Text
-}
module Data.Serialize.Text () where

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Serialize
import Control.Applicative ((<$>))

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get 

instance Serialize LT.Text where
    put = put . TLE.encodeUtf8
    get = TLE.decodeUtf8 <$> get
