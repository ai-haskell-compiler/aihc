{-# LANGUAGE CPP
            ,MultiParamTypeClasses
            ,TypeFamilies
            ,FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}

module Data.ListLike.Text.Builder

where

import           Control.DeepSeq (NFData(rnf))
import qualified Data.Text.Lazy.Builder as Builder
import           Data.ListLike.Base as LL
import           Data.ListLike.FoldableLL as LL
import           Data.ListLike.IO
import           Data.ListLike.String as LL
import           Data.ListLike.Text.TextLazy ()
--import           Data.String (IsString(fromString))
import           GHC.Exts (IsList(..))

instance FoldableLL Builder.Builder Char where
    foldl f r0 = LL.foldl f r0 . Builder.toLazyText
    foldr f r0 = LL.foldr f r0 . Builder.toLazyText

instance IsList Builder.Builder where
    type Item Builder.Builder = Char
    -- Can we do better?
    toList = LL.toList'
    fromList = LL.fromList'

instance ListLike Builder.Builder Char where
    singleton = Builder.singleton
    uncons b = case LL.uncons (Builder.toLazyText b) of
                 Nothing -> Nothing
                 Just (c, s) -> Just (c, Builder.fromLazyText s)
    null = LL.null . Builder.toLazyText

instance ListLikeIO Builder.Builder Char where
    hGetLine h = Builder.fromLazyText <$> hGetLine h
    hGetContents h = Builder.fromLazyText <$> hGetContents h
    hGet h n = Builder.fromLazyText <$> hGet h n
    hGetNonBlocking h n = Builder.fromLazyText <$> hGetNonBlocking h n
    hPutStr h = hPutStr h . Builder.toLazyText

instance StringLike Builder.Builder where
    toString = toString . Builder.toLazyText
    fromText = Builder.fromText
    fromLazyText = Builder.fromLazyText

instance NFData Builder.Builder where
    rnf = rnf . Builder.toLazyText
