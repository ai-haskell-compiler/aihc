{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      TextShow.Data.Bifunctor
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @bifunctors@ library.

/Since: 2/
-}
module TextShow.Data.Bifunctor () where

import Data.Bifunctor.Biff (Biff)
import Data.Bifunctor.Clown (Clown)
import Data.Bifunctor.Fix (Fix(..))
import Data.Bifunctor.Flip (Flip)
import Data.Bifunctor.Join (Join(..))
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Product (Product)
import Data.Bifunctor.Sum (Sum)
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Wrapped (WrappedBifunctor)

import GHC.Show (appPrec)

import Prelude ()
import Prelude.Compat

import TextShow (TextShow(..), TextShow1(..), TextShow2(..),
                 fromString, showbParen, singleton)
import TextShow.TH (deriveTextShow2, makeShowbPrec, makeLiftShowbPrec)

-- | /Since: 2/
$(deriveTextShow2 ''Biff)
-- | /Since: 2/
instance TextShow (p (f a) (g b)) => TextShow (Biff p f g a b) where
    showbPrec = $(makeShowbPrec ''Biff)
-- | /Since: 2/
instance (TextShow2 p, TextShow1 f, TextShow1 g, TextShow a) => TextShow1 (Biff p f g a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList

-- | /Since: 2/
instance TextShow (f a) => TextShow (Clown f a b) where
    showbPrec = $(makeShowbPrec ''Clown)
-- | /Since: 2/
instance TextShow (f a) => TextShow1 (Clown f a) where
    liftShowbPrec = $(makeLiftShowbPrec ''Clown)
-- | /Since: 2/
$(deriveTextShow2 ''Clown)

-- | /Since: 2/
instance TextShow (p (Fix p a) a) => TextShow (Fix p a) where
    showbPrec = $(makeShowbPrec ''Fix)
-- | /Since: 2/
instance TextShow2 p => TextShow1 (Fix p) where
    liftShowbPrec sp sl p (In x) = showbParen (p > appPrec) $
        fromString "In {out = "
     <> liftShowbPrec2 (liftShowbPrec sp sl) (liftShowbList sp sl) sp sl 0 x
     <> singleton '}'

-- | /Since: 2/
$(deriveTextShow2 ''Flip)
-- | /Since: 2/
instance TextShow (p b a) => TextShow (Flip p a b) where
    showbPrec = $(makeShowbPrec ''Flip)
-- | /Since: 2/
instance (TextShow2 p, TextShow a) => TextShow1 (Flip p a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList

-- | /Since: 2/
instance TextShow (p a a) => TextShow (Join p a) where
    showbPrec = $(makeShowbPrec ''Join)
-- | /Since: 2/
instance TextShow2 p => TextShow1 (Join p) where
    liftShowbPrec sp sl p (Join x) = showbParen (p > appPrec) $
        fromString "Join {runJoin = "
     <> liftShowbPrec2 sp sl sp sl 0 x
     <> singleton '}'

-- | /Since: 2/
instance TextShow (g b) => TextShow (Joker g a b) where
    showbPrec = $(makeShowbPrec ''Joker)
-- | /Since: 2/
instance TextShow1 g => TextShow1 (Joker g a) where
    liftShowbPrec = $(makeLiftShowbPrec ''Joker)
-- | /Since: 2/
$(deriveTextShow2 ''Joker)

-- | /Since: 2/
$(deriveTextShow2 ''Product)
-- | /Since: 2/
instance (TextShow (f a b), TextShow (g a b)) => TextShow (Product f g a b) where
    showbPrec = $(makeShowbPrec ''Product)
-- | /Since: 2/
instance (TextShow2 f, TextShow2 g, TextShow a) => TextShow1 (Product f g a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList

-- | /Since: 2/
$(deriveTextShow2 ''Sum)
-- | /Since: 2/
instance (TextShow (f a b), TextShow (g a b)) => TextShow (Sum f g a b) where
    showbPrec = $(makeShowbPrec ''Sum)
-- | /Since: 2/
instance (TextShow2 f, TextShow2 g, TextShow a) => TextShow1 (Sum f g a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList

-- | /Since: 2/
$(deriveTextShow2 ''Tannen)
-- | /Since: 2/
instance TextShow (f (p a b)) => TextShow (Tannen f p a b) where
    showbPrec = $(makeShowbPrec ''Tannen)
-- | /Since: 2/
instance (TextShow1 f, TextShow2 p, TextShow a) => TextShow1 (Tannen f p a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList

-- | /Since: 2/
$(deriveTextShow2 ''WrappedBifunctor)
-- | /Since: 2/
instance TextShow (p a b) => TextShow (WrappedBifunctor p a b) where
    showbPrec = $(makeShowbPrec ''WrappedBifunctor)
-- | /Since: 2/
instance (TextShow2 p, TextShow a) => TextShow1 (WrappedBifunctor p a) where
    liftShowbPrec = liftShowbPrec2 showbPrec showbList
