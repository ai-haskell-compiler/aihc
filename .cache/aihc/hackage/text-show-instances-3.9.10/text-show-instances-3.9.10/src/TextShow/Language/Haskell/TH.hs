{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      TextShow.Language.Haskell.TH
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

'TextShow' instances for data types in the @template-haskell@ library.

/Since: 2/
-}
module TextShow.Language.Haskell.TH (showbName, showbName') where

import           Data.Char (isAlpha)
import           Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL (Text, dropWhile, null, tail)
import           Data.Text.Lazy (uncons)

import           Prelude ()
import           Prelude.Compat

import           Language.Haskell.TH.PprLib (Doc, to_HPJ_Doc)
import           Language.Haskell.TH.Syntax

import           TextShow (TextShow(..), Builder,
                           fromString, singleton, toLazyText)
import           TextShow.Text.PrettyPrint (renderB)
import           TextShow.TH (deriveTextShow)

#if MIN_VERSION_base(4,15,0)
import qualified Data.Text.Foreign as TS (peekCStringLen)
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (plusPtr)
import           System.IO.Unsafe (unsafePerformIO)
import           TextShow (showtToShowb)
#endif

-- | Convert a 'Name' to a 'Builder'.
--
-- /Since: 2/
showbName :: Name -> Builder
showbName = showbName' Alone

-- | Convert a 'Name' to a 'Builder' with the given 'NameIs' settings.
--
-- /Since: 2/
showbName' :: NameIs -> Name -> Builder
showbName' ni nm = case ni of
    Alone           -> nms
    Applied
        | pnam      -> nms
        | otherwise -> singleton '(' <> nms <> singleton ')'
    Infix
        | pnam      -> singleton '`' <> nms <> singleton '`'
        | otherwise -> nms
  where
    -- For now, we make the NameQ and NameG print the same, even though
    -- NameQ is a qualified name (so what it means depends on what the
    -- current scope is), and NameG is an original name (so its meaning
    -- should be independent of what's in scope.
    -- We may well want to distinguish them in the end.
    -- Ditto NameU and NameL
    nms :: Builder
    nms = case nm of
               Name occ NameS         -> occB occ
               Name occ (NameQ m)     -> modB m   <> singleton '.' <> occB occ
               Name occ (NameG _ _ m) -> modB m   <> singleton '.' <> occB occ
               Name occ (NameU u)     -> occB occ <> singleton '_' <> showb u
               Name occ (NameL u)     -> occB occ <> singleton '_' <> showb u

    occB :: OccName -> Builder
    occB = fromString . occString

    modB :: ModName -> Builder
    modB = fromString . modString

    pnam :: Bool
    pnam = classify $ toLazyText nms

    -- True if we are function style, e.g. f, [], (,)
    -- False if we are operator style, e.g. +, :+
    classify :: TL.Text -> Bool
    classify t
        | TL.null t  = False -- shouldn't happen; . operator is handled below
        | otherwise = case fromJust $ uncons t of
              (x, xs) -> if isAlpha x || (x `elem` "_[]()")
                            then let t' = TL.dropWhile (/= '.') xs
                                 in if TL.null t'
                                       then True
                                       else classify $ TL.tail t'
                            else False

-- | /Since: 2/
instance TextShow Name where
    showb = showbName

-- | /Since: 2/
instance TextShow Doc where
    showb = renderB . to_HPJ_Doc

#if MIN_VERSION_template_haskell(2,17,0)
instance TextShow Bytes where
   showb = showtToShowb showt
   showt b = unsafePerformIO $ withForeignPtr (bytesPtr b) $ \ptr ->
                TS.peekCStringLen ( ptr `plusPtr` fromIntegral (bytesOffset b)
                                  , fromIntegral (bytesSize b)
                                  )
#endif

-- A significant chunk of these data types are mutually recursive, which makes
-- it impossible to derive TextShow instances for everything individually using
-- Template Haskell. As a workaround, we splice everything together in a single
-- ungodly large splice. One unfortunate consequence of this is that we cannot
-- give Haddocks to each instance :(
$(concat <$> traverse deriveTextShow
  [ ''AnnLookup
  , ''AnnTarget
  , ''Body
  , ''Callconv
  , ''Clause
  , ''Con
  , ''Dec
  , ''Exp
#if !(MIN_VERSION_template_haskell(2,13,0))
  , ''FamFlavour
#endif
  , ''Fixity
  , ''FixityDirection
  , ''Foreign
  , ''FunDep
  , ''Guard
  , ''Info
  , ''Inline
  , ''Lit
  , ''Loc
  , ''Match
  , ''ModName
  , ''Module
  , ''ModuleInfo
  , ''NameFlavour
  , ''NameSpace
#if MIN_VERSION_template_haskell(2,22,0)
  , ''NamespaceSpecifier
#endif
  , ''OccName
  , ''Pat
  , ''Phases
  , ''PkgName
  , ''Pragma
  , ''Range
  , ''Role
  , ''RuleBndr
  , ''RuleMatch
  , ''Safety
  , ''Stmt
  , ''TyLit
  , ''Type
  , ''TySynEqn
  , ''TyVarBndr

  , ''Bang
  , ''DecidedStrictness
  , ''FamilyResultSig
  , ''InjectivityAnn
  , ''Overlap
  , ''SourceStrictness
  , ''SourceUnpackedness
  , ''TypeFamilyHead

  , ''DerivClause
  , ''DerivStrategy
  , ''PatSynArgs
  , ''PatSynDir

#if MIN_VERSION_template_haskell(2,16,0) && !(MIN_VERSION_template_haskell(2,17,0))
  , ''Bytes
#endif

#if MIN_VERSION_template_haskell(2,17,0)
  , ''Specificity
#endif

#if MIN_VERSION_template_haskell(2,21,0)
  , ''BndrVis
#endif
  ])
