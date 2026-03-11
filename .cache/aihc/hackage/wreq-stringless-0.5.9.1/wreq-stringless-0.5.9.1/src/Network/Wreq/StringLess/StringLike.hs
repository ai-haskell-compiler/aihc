-- |
-- Simple Type-Class to use string-like datatypes instead of 'String'.
--
{-# LANGUAGE FlexibleInstances #-}
module Network.Wreq.StringLess.StringLike
    (StringLike (..)) where


import qualified Data.ByteString.Lazy.UTF8 as BSL
import qualified Data.ByteString.UTF8      as BS
import           Data.String               (IsString)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL


class IsString s => StringLike s where
    toString :: s -> String


instance StringLike T.Text where
    toString = T.unpack

instance StringLike TL.Text where
    toString = TL.unpack

instance StringLike BS.ByteString where
    toString = BS.toString

instance StringLike BSL.ByteString where
    toString = BSL.toString

