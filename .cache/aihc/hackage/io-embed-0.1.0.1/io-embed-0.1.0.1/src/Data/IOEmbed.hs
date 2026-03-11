{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.IOEmbed (embedIO, embedIOLit, toLitE) where

import Language.Haskell.TH
import Data.Typeable (Typeable, Proxy (..), typeRep, cast)
import qualified Data.ByteString as B8
import Data.ByteString(ByteString)
import Data.ByteString.Internal (ByteString(..))
import Data.Maybe(fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Unsafe (unsafePackAddressLen)

-- | Embeds an `IO` a value - as long as `a` is `Char`, `String`,  'Integer', `Rational`, or `ByteString`.
-- 
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > fileContent = $(embedIO $ readFile "./README.md")
-- 
embedIO :: forall a.  Typeable a => IO a -> Q Exp
embedIO = runIO . fmap toLitE

-- | If you want to embed something else, you can manually generate an `IO` `Lit` and use this function. 
-- 
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > fileContentL = $(embedIOLit $ StringL <$> readFile "./README.md")
-- 
embedIOLit :: IO Lit -> Q Exp
embedIOLit = runIO . fmap LitE

-- | Converts `Char`, `String`,  'Integer', `Rational`, or `ByteString` to a literal expression.
toLitE :: forall a. Typeable a => a -> Exp
toLitE x
  | typeRep' == typeRep (Proxy :: Proxy Char)       = LitE $ CharL     $ fromJust $ cast x
  | typeRep' == typeRep (Proxy :: Proxy String)     = LitE $ StringL   $ fromJust $ cast x
  | typeRep' == typeRep (Proxy :: Proxy Integer)    = LitE $ IntegerL  $ fromJust $ cast x
  | typeRep' == typeRep (Proxy :: Proxy Rational)   = LitE $ RationalL $ fromJust $ cast x
  -- Special types
  | typeRep' == typeRep (Proxy :: Proxy ByteString) = let
                                                        bs = fromJust $ cast x 
                                                        PS ptr off sz = bs
                                                      in VarE 'unsafePerformIO
                                                      `AppE`  (VarE 'unsafePackAddressLen `AppE` LitE (IntegerL $ fromIntegral $ B8.length bs)
                                                      `AppE` LitE (bytesPrimL $ mkBytes ptr (fromIntegral off) (fromIntegral sz)))
                                                        

  | otherwise = error $ "Type " <> show typeRep' <> " is not embeddable" <>
                        "\nEmbeddable types are: Char, String, Integer, Rational, ByteString"
  where typeRep' = typeRep (Proxy :: Proxy a)


