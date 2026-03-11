{-# OPTIONS -Wno-orphans           #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.String.Interpolate.Conversion.TextSink
  ()
where

import qualified Data.ByteString         as B
import qualified Data.ByteString.Builder as LB
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as LT hiding ( singleton )
import qualified Data.Text.Lazy.Builder  as LT

import Data.String.Interpolate.Conversion.Classes
import Data.String.Interpolate.Conversion.Encoding ( bsToTextBuilder, lbsToTextBuilder )

#ifdef TEXT_BUILDER
#else
import qualified Data.Text.Lazy
#endif

--------------------
-- SINK DEFINITIONS
--------------------

#ifdef TEXT_BUILDER

instance InterpSink 'True T.Text where
  type Builder 'True T.Text = LT.Builder

  ofString _ = B . LT.fromString
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = LT.toStrict . LT.toLazyText . unB

instance InterpSink 'True LT.Text where
  type Builder 'True LT.Text = LT.Builder

  ofString _ = B . LT.fromString
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = LT.toLazyText . unB

#else

instance InterpSink 'True T.Text where
  type Builder 'True T.Text = T.Text

  ofString _ = B . T.pack
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = unB

instance InterpSink 'True LT.Text where
  type Builder 'True LT.Text = LT.Text

  ofString _ = B . LT.pack
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = unB

#endif

instance InterpSink 'True LT.Builder where
  type Builder 'True LT.Builder = LT.Builder

  ofString _ = B . LT.fromString
  build _ (B l) (B r) = B $ l `mappend` r
  finalize _ = unB

--------------------
-- INTERPOLATION INSTANCES
--------------------

#ifdef TEXT_BUILDER

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src T.Text where
  interpolate _ = B . LT.fromString . show
instance {-# OVERLAPS #-} Interpolatable 'True Char T.Text where
  interpolate _ = B . LT.singleton
instance {-# OVERLAPS #-} Interpolatable 'True String T.Text where
  interpolate _ = B . LT.fromString
instance {-# OVERLAPS #-} Interpolatable 'True T.Text T.Text where
  interpolate _ = B . LT.fromText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text T.Text where
  interpolate _ = B . LT.fromLazyText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder T.Text where
  interpolate _ = B
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString T.Text where
  interpolate _ = B . bsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString T.Text where
  interpolate _ = B . lbsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder T.Text where
  interpolate _ = B . lbsToTextBuilder . LB.toLazyByteString

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src LT.Text where
  interpolate _ = B . LT.fromString . show
instance {-# OVERLAPS #-} Interpolatable 'True Char LT.Text where
  interpolate _ = B . LT.singleton
instance {-# OVERLAPS #-} Interpolatable 'True String LT.Text where
  interpolate _ = B . LT.fromString
instance {-# OVERLAPS #-} Interpolatable 'True T.Text LT.Text where
  interpolate _ = B . LT.fromText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text LT.Text where
  interpolate _ = B . LT.fromLazyText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder LT.Text where
  interpolate _ = B
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString LT.Text where
  interpolate _ = B . bsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString LT.Text where
  interpolate _ = B . lbsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder LT.Text where
  interpolate _ = B . lbsToTextBuilder . LB.toLazyByteString

#else

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src T.Text where
  interpolate _ = B . T.pack . show
instance {-# OVERLAPS #-} Interpolatable 'True Char T.Text where
  interpolate _ = B . T.singleton
instance {-# OVERLAPS #-} Interpolatable 'True String T.Text where
  interpolate _ = B . T.pack
instance {-# OVERLAPS #-} Interpolatable 'True T.Text T.Text where
  interpolate _ = B
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text T.Text where
  interpolate _ = B . LT.toStrict
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder T.Text where
  interpolate _ = B . LT.toStrict . LT.toLazyText
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString T.Text where
  interpolate _ = B . LT.toStrict . LT.toLazyText . bsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString T.Text where
  interpolate _ = B . LT.toStrict . LT.toLazyText . lbsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder T.Text where
  interpolate _ = B . LT.toStrict . LT.toLazyText . lbsToTextBuilder . LB.toLazyByteString

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src LT.Text where
  interpolate _ = B . LT.pack . show
instance {-# OVERLAPS #-} Interpolatable 'True Char LT.Text where
  interpolate _ = B . Data.Text.Lazy.singleton
instance {-# OVERLAPS #-} Interpolatable 'True String LT.Text where
  interpolate _ = B . LT.pack
instance {-# OVERLAPS #-} Interpolatable 'True T.Text LT.Text where
  interpolate _ = B . LT.fromStrict
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text LT.Text where
  interpolate _ = B
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder LT.Text where
  interpolate _ = B . LT.toLazyText
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString LT.Text where
  interpolate _ = B . LT.toLazyText . bsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString LT.Text where
  interpolate _ = B . LT.toLazyText . lbsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder LT.Text where
  interpolate _ = B . LT.toLazyText . lbsToTextBuilder . LB.toLazyByteString

#endif

instance {-# OVERLAPPABLE #-} Show src => Interpolatable 'True src LT.Builder where
  interpolate _ = B . LT.fromString . show
instance {-# OVERLAPS #-} Interpolatable 'True Char LT.Builder where
  interpolate _ = B . LT.singleton
instance {-# OVERLAPS #-} Interpolatable 'True String LT.Builder where
  interpolate _ = B . LT.fromString
instance {-# OVERLAPS #-} Interpolatable 'True T.Text LT.Builder where
  interpolate _ = B . LT.fromText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Text LT.Builder where
  interpolate _ = B . LT.fromLazyText
instance {-# OVERLAPS #-} Interpolatable 'True LT.Builder LT.Builder where
  interpolate _ = B
instance {-# OVERLAPS #-} Interpolatable 'True B.ByteString LT.Builder where
  interpolate _ = B . bsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.ByteString LT.Builder where
  interpolate _ = B . lbsToTextBuilder
instance {-# OVERLAPS #-} Interpolatable 'True LB.Builder LT.Builder where
  interpolate _ = B . lbsToTextBuilder . LB.toLazyByteString
