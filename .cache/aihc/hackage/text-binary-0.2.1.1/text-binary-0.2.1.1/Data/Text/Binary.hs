{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Text.Binary where

#if !(MIN_VERSION_text(1,2,1))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import Data.Binary

instance Binary T.Text where
    put = put . T.encodeUtf8
    get   = do
      bs <- get
      case T.decodeUtf8' bs of
        Left exn -> fail (show exn)
        Right a -> return a

instance Binary L.Text where
    put = put . L.encodeUtf8
    get   = do
      bs <- get
      case L.decodeUtf8' bs of
        Left exn -> fail (show exn)
        Right a -> return a
#endif
