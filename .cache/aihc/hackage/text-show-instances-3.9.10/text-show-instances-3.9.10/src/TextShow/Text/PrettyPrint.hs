{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Text.PrettyPrint
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'TextShow' instances for data types in the @pretty@ library.

/Since: 2/
-}
module TextShow.Text.PrettyPrint (
      renderB
    , renderStyleB
    , renderAnnotB
    , renderStyleAnnotB
    ) where

import           Prelude ()
import           Prelude.Compat

import           Text.PrettyPrint.HughesPJ (Doc, Mode, Style(..), TextDetails(..),
                                            fullRender, style)
import           Text.PrettyPrint.HughesPJClass (PrettyLevel)
import qualified Text.PrettyPrint.Annotated.HughesPJ as Annot (Doc, fullRender, style)
import           Text.PrettyPrint.Annotated.HughesPJ (AnnotDetails, Span)
import qualified Text.PrettyPrint.Annotated.HughesPJClass as Annot (PrettyLevel)

import           TextShow (TextShow(..), TextShow1(..), Builder, fromString, singleton)
import           TextShow.TH (deriveTextShow, deriveTextShow1)

-- | Renders a 'Doc' to a 'Builder' using the default 'style'.
--
-- /Since: 2/
renderB :: Doc -> Builder
renderB = renderStyleB style
{-# INLINE renderB #-}

-- | Renders a 'Doc' to a 'Builder' using the given 'Style'.
--
-- /Since: 2/
renderStyleB :: Style -> Doc -> Builder
renderStyleB sty doc = fullRender (mode sty)
                                  (lineLength sty)
                                  (ribbonsPerLine sty)
                                  txtPrinter
                                  mempty
                                  doc
{-# INLINE renderStyleB #-}

txtPrinter :: TextDetails -> Builder -> Builder
txtPrinter (Chr c)   b = singleton c <> b
txtPrinter (Str s')  b = fromString s' <> b
txtPrinter (PStr s') b = fromString s' <> b
{-# INLINE txtPrinter #-}

-- | Renders an annotated 'Doc' to a 'Builder' using the default 'Annot.style'.
--
-- /Since: 3/
renderAnnotB :: Annot.Doc a -> Builder
renderAnnotB = renderStyleAnnotB Annot.style
{-# INLINE renderAnnotB #-}

-- | Renders an annotated 'Doc' to a 'Builder' using the given 'Style'.
--
-- /Since: 3/
renderStyleAnnotB :: Style -> Annot.Doc a -> Builder
renderStyleAnnotB sty doc =
    Annot.fullRender (mode sty)
                     (lineLength sty)
                     (ribbonsPerLine sty)
                     txtPrinter
                     mempty
                     doc

-- | /Since: 2/
instance TextShow Doc where
    showb = renderB
    {-# INLINE showb #-}

-- | /Since: 2/
$(deriveTextShow ''Mode)
-- | /Since: 2/
$(deriveTextShow ''Style)
-- | /Since: 2/
$(deriveTextShow ''TextDetails)
-- | /Since: 2/
$(deriveTextShow ''PrettyLevel)

-- | /Since: 3/
$(deriveTextShow  ''AnnotDetails)
-- | /Since: 3/
$(deriveTextShow1 ''AnnotDetails)

-- | /Since: 3/
instance TextShow (Annot.Doc a) where
    showb = renderAnnotB
    {-# INLINE showb #-}
-- | /Since: 3/
instance TextShow1 Annot.Doc where
    liftShowbPrec _ _ = showbPrec
    {-# INLINE liftShowbPrec #-}

-- | /Since: 3/
$(deriveTextShow ''Annot.PrettyLevel)

-- | /Since: 3/
$(deriveTextShow  ''Span)
-- | /Since: 3/
$(deriveTextShow1 ''Span)
