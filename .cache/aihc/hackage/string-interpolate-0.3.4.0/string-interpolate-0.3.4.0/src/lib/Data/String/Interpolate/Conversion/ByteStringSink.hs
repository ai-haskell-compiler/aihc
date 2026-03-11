{-# OPTIONS -Wno-orphans           #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.String.Interpolate.Conversion.ByteStringSink
  ()
where

import Data.Text.Conversions

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as LB
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT hiding ( singleton )
import qualified Data.Text.Lazy.Builder  as LT

import Data.String.Interpolate.Conversion.Classes
import Data.String.Interpolate.Conversion.Encoding ( encodeCharUTF8 )

--------------------
-- SINK DEFINITIONS
--------------------

#ifdef BYTESTRING_BUILDER

instance InterpSink 'True B.ByteString where
  type Builder 'True B.ByteString = LB.Builder

  ofString _ = B . LB.byteString . unUTF8 . convertText
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = LB.toStrict . LB.toLazyByteString . unB

instance InterpSink 'True LB.ByteString where
  type Builder 'True LB.ByteString = LB.Builder

  ofString _ = B . LB.lazyByteString . unUTF8 . convertText
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = LB.toLazyByteString . unB

#else

instance InterpSink 'True B.ByteString where
  type Builder 'True B.ByteString = B.ByteString

  ofString _ = B . unUTF8 . convertText
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = unB

instance InterpSink 'True LB.ByteString where
  type Builder 'True LB.ByteString = LB.ByteString

  ofString _ = B . unUTF8 . convertText
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = unB

#endif

instance InterpSink 'True LB.Builder where
  type Builder 'True LB.Builder = LB.Builder

  ofString _ = B . LB.lazyByteString . unUTF8 . convertText
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = unB

--------------------
-- INTERPOLATION INSTANCES
--------------------

#ifdef BYTESTRING_BUILDER

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src B.ByteString where
  interpolate _ = B . LB.byteString . unUTF8 . convertText . show
instance {-# OVERLAPS #-} Interpolatable 'True Char B.ByteString where
  interpolate _ = B . encodeCharUTF8
instance {-# OVERLAPS #-} Interpolatable 'True String B.ByteString where
  interpolate _ = B . LB.byteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True T.Text B.ByteString where
  interpolate _ = B . LB.byteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text B.ByteString where
  interpolate _ = B . LB.byteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder B.ByteString where
  interpolate _ = B . LB.byteString . unUTF8 . convertText . LT.toLazyText
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString B.ByteString where
  interpolate _ = B . LB.byteString
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString B.ByteString where
  interpolate _ = B . LB.lazyByteString
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder B.ByteString where
  interpolate _ = B

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src LB.ByteString where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText . show
instance {-# OVERLAPS #-} Interpolatable 'True Char LB.ByteString where
  interpolate _ = B . encodeCharUTF8
instance {-# OVERLAPS #-} Interpolatable 'True String LB.ByteString where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True T.Text LB.ByteString where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text LB.ByteString where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder LB.ByteString where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText . LT.toLazyText
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString LB.ByteString where
  interpolate _ = B . LB.byteString
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString LB.ByteString where
  interpolate _ = B . LB.lazyByteString
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder LB.ByteString where
  interpolate _ = B

#else

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src B.ByteString where
  interpolate _ = B . unUTF8 . convertText . show
instance {-# OVERLAPS #-} Interpolatable 'True Char B.ByteString where
  interpolate _ = B . LB.toStrict . LB.toLazyByteString . encodeCharUTF8
instance {-# OVERLAPS #-} Interpolatable 'True String B.ByteString where
  interpolate _ = B . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True T.Text B.ByteString where
  interpolate _ = B . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text B.ByteString where
  interpolate _ = B . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder B.ByteString where
  interpolate _ = B . unUTF8 . convertText . LT.toLazyText
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString B.ByteString where
  interpolate _ = B
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString B.ByteString where
  interpolate _ = B . LB.toStrict
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder B.ByteString where
  interpolate _ = B . LB.toStrict . LB.toLazyByteString

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src LB.ByteString where
  interpolate _ = B . unUTF8 . convertText . show
instance {-# OVERLAPS #-} Interpolatable 'True Char LB.ByteString where
  interpolate _ = B . LB.toLazyByteString . encodeCharUTF8
instance {-# OVERLAPS #-} Interpolatable 'True String LB.ByteString where
  interpolate _ = B . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True T.Text LB.ByteString where
  interpolate _ = B . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text LB.ByteString where
  interpolate _ = B . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder LB.ByteString where
  interpolate _ = B . unUTF8 . convertText . LT.toLazyText
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString LB.ByteString where
  interpolate _ = B . LB.fromStrict
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString LB.ByteString where
  interpolate _ = B
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder LB.ByteString where
  interpolate _ = B . LB.toLazyByteString

#endif

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src LB.Builder where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText . show
instance {-# OVERLAPS #-} Interpolatable 'True Char LB.Builder where
  interpolate _ = B . encodeCharUTF8
instance {-# OVERLAPS #-} Interpolatable 'True String LB.Builder where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True T.Text LB.Builder where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text LB.Builder where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder LB.Builder where
  interpolate _ = B . LB.lazyByteString . unUTF8 . convertText . LT.toLazyText
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString LB.Builder where
  interpolate _ = B . LB.byteString
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString LB.Builder where
  interpolate _ = B . LB.lazyByteString
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder LB.Builder where
  interpolate _ = B
