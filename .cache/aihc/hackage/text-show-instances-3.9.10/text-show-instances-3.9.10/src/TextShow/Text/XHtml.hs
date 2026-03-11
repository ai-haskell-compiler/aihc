{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Text.XHtml
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @xhtml@ library.

/Since: 2/
-}
module TextShow.Text.XHtml () where

import Prelude ()
import Prelude.Compat

import Text.XHtml.Frameset (Html, HtmlAttr, HotLink,
                            htmlAttrPair, renderHtmlFragment)
import Text.XHtml.Table (HtmlTable)

import TextShow (TextShow(..), FromStringShow(..), fromString, singleton)
import TextShow.TH (deriveTextShow)

-- | /Since: 2/
instance TextShow Html where
    showb = fromString . renderHtmlFragment
    {-# INLINE showb #-}

    showbList = mconcat . map showb
    {-# INLINE showbList #-}

-- | /Since: 2/
instance TextShow HtmlAttr where
    showb ha = case htmlAttrPair ha of
       (str, val) -> fromString str <> singleton '=' <> showb val
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow ''HotLink)

-- | /Since: 2/
instance TextShow HtmlTable where
    showb = showb . FromStringShow
    {-# INLINE showb #-}
