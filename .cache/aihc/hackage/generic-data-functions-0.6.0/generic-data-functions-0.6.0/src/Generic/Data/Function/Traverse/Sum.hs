{-# LANGUAGE UndecidableInstances #-} -- due to type hell
{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to generic typeclass design

{- | 'traverse' over generic sum types.

Disambiguates constructors by prepending sum tags.

Note that the sum tag approach has efficiency limitations.
You may design a constructor disambiguation schema which permits "incrementally"
parsing, rather than parsing some whole thing then comparing to each option,
which will be faster. If you wish to perform such sum tag handling yourself, but
still want the free generics, "Generic.Data.FOnCstr" can do this for you.
-}

module Generic.Data.Function.Traverse.Sum where

import GHC.Generics
import Generic.Data.Function.Traverse.Constructor
import Generic.Data.MetaParse.Cstr
import GHC.Exts ( proxy# )

import Control.Applicative qualified as Applicative
import Control.Applicative ( Alternative((<|>)) )

import Generic.Data.Function.Common.TypeLits ( symbolVal'' )
import GHC.TypeLits ( Symbol, KnownSymbol )

class GTraverseSum tag sumtag gf where
    gTraverseSum
        :: ParseCstrTo sumtag pt
        -> (String -> GenericTraverseF tag pt)
        -> (forall a. String -> GenericTraverseF tag a)
        -> (pt -> pt -> Bool)
        -> GenericTraverseF tag (gf p)

instance GenericTraverse tag => GTraverseSum tag sumtag V1 where
    gTraverseSum _parseCstr _ptGet _fNoMatch _ptEq =
        genericTraverseV1 @tag

instance
  ( f ~ GenericTraverseF tag
  , Alternative f
  , Monad f
  , KnownSymbol dtName
  , GTraverseCSum tag sumtag dtName gf
  ) => GTraverseSum tag sumtag (D1 (MetaData dtName _md2 _md3 _md4) gf) where
    gTraverseSum parseCstr ptGet fNoMatch ptEq = do
        pt <- ptGet dtName
        M1 <$>
            (     gTraverseCSum @tag @sumtag @dtName parseCstr ptEq pt
              <|> fNoMatch dtName)
      where
        dtName = symbolVal'' @dtName

class GTraverseCSum tag sumtag (dtName :: Symbol) gf where
    gTraverseCSum
        :: ParseCstrTo sumtag pt
        -> (pt -> pt -> Bool)
        -> pt
        -> GenericTraverseF tag (gf p)

-- | Combine constructor options with '(<|>)' ("or").
instance
  ( Alternative (GenericTraverseF tag)
  , GTraverseCSum tag sumtag dtName l
  , GTraverseCSum tag sumtag dtName r
  ) => GTraverseCSum tag sumtag dtName (l :+: r) where
    gTraverseCSum parseCstr ptEq pt = l <|> r
      where
        l = L1 <$> gTraverseCSum @tag @sumtag @dtName parseCstr ptEq pt
        r = R1 <$> gTraverseCSum @tag @sumtag @dtName parseCstr ptEq pt

instance
  ( Alternative (GenericTraverseF tag)
  , GTraverseC tag dtName cstrName 0 gf
  , ReifyCstrParseResult sumtag cstrParsed
  , ForceGCParse dtName cstr (ParseCstr sumtag cstrName) ~ cstrParsed
  ) => GTraverseCSum tag sumtag dtName (C1 (MetaCons cstrName _mc2 _mc3) gf) where
    gTraverseCSum parseCstr ptEq pt = do
        if   ptEq pt ptCstr
        then M1 <$> gTraverseC @tag @dtName @cstrName @0
        else Applicative.empty
      where
        ptCstr = parseCstr (proxy# @cstrParsed)
