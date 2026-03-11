----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.Combinators
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Combinators
  ( Pretty(..)
  , PP.Doc
  , putDocLn
  , render
  , renderLazy
  , renderString
  , docFromString
  , docFromText
  , PP.viaShow

  , renderWith
  , renderLazyWith
  , renderStringWith
  , PP.defaultLayoutOptions
  , PP.LayoutOptions(..)
  , PP.PageWidth(..)

  , MapEntry(..)
  , (-->)
  , ppMapEntryWith
  , ppMapEntryWithSep
  , ppList
  , (##)
  , ppDictHeader
  , ppDictAssocList

  , ppTuple
  , ppTupleWith
  , ppListWith
  , ppFoldableHeader
  , ppFoldableHeaderWith
  , ppNE
  , ppNEWith
  , ppMap
  , ppMapWith
  , ppSet
  , ppSetWith
  , ppBimap
  , ppBimapWith
  , ppIntSet
  , ppIntSetWith
  , ppIntMap
  , ppIntMapWith
  , ppHashSet
  , ppHashSetWith
  , ppHashMap
  , ppHashMapWith
  , ppVector
  , ppVectorWith
  , ppDList
  , ppDListWith
  , ppListWithDelim
  , ppAssocList
  , ppAssocListWith
  , ppAssocListWithSep
  , ppByteString
  , ppByteStringLazy
  , ppShortByteString
  , ppCallStack
  , ppCallStackGHC

#ifdef HAVE_ENUMMAPSET
  , ppEnumSet
  , ppEnumSetWith
  , ppEnumMap
  , ppEnumMapWith
#endif
  ) where

import Data.Bimap (Bimap)
import Data.Bimap qualified as BM
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as CL8
import Data.ByteString.Short qualified as ShortBS
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector.Generic qualified as G
import GHC.Stack (CallStack, SrcLoc(..), getCallStack, prettySrcLoc)
import Prettyprinter (Pretty(..), Doc, (<+>))
import Prettyprinter qualified as PP
import Prettyprinter.Combinators.Basic
import Prettyprinter.MetaDoc
import Prettyprinter.Render.Text qualified as PP.Render

#ifdef HAVE_ENUMMAPSET
import Data.EnumMap (EnumMap)
import Data.EnumMap qualified as EM
import Data.EnumSet (EnumSet)
import Data.EnumSet qualified as ES
#endif

putDocLn :: Doc ann -> IO ()
putDocLn x = do
  PP.Render.putDoc x
  putStrLn ""

render :: Doc ann -> T.Text
render = renderWith PP.defaultLayoutOptions

renderLazy :: Doc ann -> TL.Text
renderLazy = renderLazyWith PP.defaultLayoutOptions

renderString :: Doc ann -> String
renderString = renderStringWith PP.defaultLayoutOptions

renderWith :: PP.LayoutOptions -> Doc ann -> T.Text
renderWith opt = TL.toStrict . renderLazyWith opt

renderLazyWith :: PP.LayoutOptions -> Doc ann -> TL.Text
renderLazyWith opt = PP.Render.renderLazy . PP.layoutPretty opt

renderStringWith :: PP.LayoutOptions -> Doc ann -> String
renderStringWith opt = TL.unpack . renderLazyWith opt


docFromString :: String -> Doc ann
docFromString = pretty . TL.pack

docFromText :: T.Text -> Doc ann
docFromText = pretty


infixr 0 :->

data MapEntry k v = k :-> v
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

ppMapEntryWith
  :: (k -> Doc ann) -> (v -> Doc ann) -> MapEntry k v -> Doc ann
ppMapEntryWith = ppMapEntryWithSep "->"

ppMapEntryWithSep
  :: Doc ann -> (k -> Doc ann) -> (v -> Doc ann) -> MapEntry k v -> Doc ann
ppMapEntryWithSep sep f g (x :-> y) =
  PP.group $ f x <+> PP.nest 4 (sep <> PP.line <> PP.align (g y))

instance (Pretty k, Pretty v) => Pretty (MapEntry k v) where
  pretty = ppMapEntryWith pretty pretty

infixr 0 -->

-- | Helper to make constructing 'MapEntry' pairs easier by calling
-- pretty on value.
(-->) :: Pretty v => T.Text -> v -> MapEntry T.Text (Doc ann)
(-->) k v = k :-> pretty v

ppDictHeader :: Doc ann -> [MapEntry T.Text (Doc ann)] -> Doc ann
ppDictHeader header entries =
  header ## ppDictAssocList entries

ppDictAssocList :: [MapEntry T.Text (Doc ann)] -> Doc ann
ppDictAssocList entries = ppListWithDelim PP.lbrace PP.rbrace entries'
  where
    entries' = flip map entries $ \entry ->
      ppMapEntryWith (PP.fillBreak maxWidth . pretty) id entry `PP.flatAlt`
      ppMapEntryWith pretty id entry
    maxWidth = maximum $ map (\(k :-> _) -> T.length k) entries

ppList :: Pretty a => [a] -> Doc ann
ppList = ppListWith pretty

ppListWith :: (a -> Doc ann) -> [a] -> Doc ann
ppListWith f = ppListWithDelim PP.lbracket PP.rbracket . map f

ppTuple :: Pretty a => [a] -> Doc ann
ppTuple = ppTupleWith pretty

ppTupleWith :: (a -> Doc ann) -> [a] -> Doc ann
ppTupleWith f = ppListWithDelim PP.lparen PP.rparen . map f

ppFoldableHeader :: (Pretty a, Foldable f) => Doc ann -> f a -> Doc ann
ppFoldableHeader = ppFoldableHeaderWith pretty

ppFoldableHeaderWith
  :: Foldable f
  => (a -> Doc ann) -> Doc ann -> f a -> Doc ann
ppFoldableHeaderWith f header entries =
  PP.nest 2 $
  header <> PP.line <> PP.vsep (map (("-" PP.<+>) . PP.align . f) $ toList entries)

ppNE :: Pretty a => NonEmpty a -> Doc ann
ppNE = ppNEWith pretty

ppNEWith :: (a -> Doc ann) -> NonEmpty a -> Doc ann
ppNEWith f = ppListWithDelim PP.lbracket PP.rbracket . map f . toList

ppMap :: (Pretty a, Pretty b) => Map a b -> Doc ann
ppMap = ppMapWith pretty pretty

ppMapWith :: (k -> Doc ann) -> (v -> Doc ann) -> Map k v -> Doc ann
ppMapWith f g = ppAssocListWith f g . M.toList

ppSet :: Pretty a => Set a -> Doc ann
ppSet = ppSetWith pretty

ppSetWith :: (a -> Doc ann) -> Set a -> Doc ann
ppSetWith f = ppListWithDelim PP.lbrace PP.rbrace . map f . toList

ppBimap :: (Pretty k, Pretty v) => Bimap k v -> Doc ann
ppBimap = ppBimapWith pretty pretty

ppBimapWith :: (k -> Doc ann) -> (v -> Doc ann) -> Bimap k v -> Doc ann
ppBimapWith f g = ppAssocListWithSep "<->" f g . BM.toList

ppIntSet :: IntSet -> Doc ann
ppIntSet = ppIntSetWith pretty

ppIntSetWith :: (Int -> Doc ann) -> IntSet -> Doc ann
ppIntSetWith f = ppListWithDelim PP.lbrace PP.rbrace . map f . IS.toList

ppIntMap :: Pretty a => IntMap a -> Doc ann
ppIntMap = ppIntMapWith pretty pretty

ppIntMapWith :: (Int -> Doc ann) -> (a -> Doc ann) -> IntMap a -> Doc ann
ppIntMapWith f g = ppAssocListWith f g . IM.toList

#ifdef HAVE_ENUMMAPSET
ppEnumSet :: (Enum a, Pretty a) => EnumSet a -> Doc ann
ppEnumSet = ppEnumSetWith pretty

ppEnumSetWith :: Enum a => (a -> Doc ann) -> EnumSet a -> Doc ann
ppEnumSetWith f = ppListWithDelim PP.lbrace PP.rbrace . map f . ES.toList

ppEnumMap :: (Enum k, Pretty k, Pretty v) => EnumMap k v -> Doc ann
ppEnumMap = ppEnumMapWith pretty pretty

ppEnumMapWith :: Enum k => (k -> Doc ann) -> (v -> Doc ann) -> EnumMap k v -> Doc ann
ppEnumMapWith f g = ppAssocListWith f g . EM.toList
#endif

ppHashSet :: Pretty a => HashSet a -> Doc ann
ppHashSet = ppHashSetWith pretty

ppHashSetWith :: (a -> Doc ann) -> HashSet a -> Doc ann
ppHashSetWith f = ppListWithDelim PP.lbrace PP.rbrace . map f . HS.toList

ppHashMap :: (Pretty k, Pretty v) => HashMap k v -> Doc ann
ppHashMap = ppHashMapWith pretty pretty

ppHashMapWith :: (k -> Doc ann) -> (v -> Doc ann) -> HashMap k v -> Doc ann
ppHashMapWith f g = ppAssocListWith f g . HM.toList

ppVector :: (G.Vector v a, Pretty a) => v a -> Doc ann
ppVector = ppVectorWith pretty

ppVectorWith :: G.Vector v a => (a -> Doc ann) -> v a -> Doc ann
ppVectorWith f = ppListWith f . G.toList

ppDList :: Pretty a => DList a -> Doc ann
ppDList = ppDListWith pretty

ppDListWith :: (a -> Doc ann) -> DList a -> Doc ann
ppDListWith f = ppListWith f . DL.toList

ppListWithDelim
  :: forall f ann. Foldable f
  => Doc ann
  -> Doc ann
  -> f (Doc ann)
  -> Doc ann
ppListWithDelim = ppListWithDelimSep separator
  where
    separator :: Doc ann
    separator = ","

ppAssocList :: (Pretty k, Pretty v) => [(k, v)] -> Doc ann
ppAssocList =
  ppAssocListWith pretty pretty

ppAssocListWith :: (k -> Doc ann) -> (v -> Doc ann) -> [(k, v)] -> Doc ann
ppAssocListWith =
  ppAssocListWithSep "->"

ppAssocListWithSep
  :: Doc ann
  -> (k -> Doc ann)
  -> (v -> Doc ann)
  -> [(k, v)]
  -> Doc ann
ppAssocListWithSep sep f g =
  ppListWithDelim PP.lbrace PP.rbrace . map (ppMapEntryWithSep sep f g . uncurry (:->))

ppByteString :: C8.ByteString -> Doc ann
ppByteString = mdPayload . strictByteStringMetaDoc

ppByteStringLazy :: CL8.ByteString -> Doc ann
ppByteStringLazy = mdPayload . lazyByteStringMetaDoc

ppShortByteString :: ShortBS.ShortByteString -> Doc ann
ppShortByteString = mdPayload . shortByteStringMetaDoc

ppCallStack :: CallStack -> Doc ann
ppCallStack =
  PP.vcat .
  map (\(name, loc) -> PP.hcat
        [ docFromString (srcLocModule loc)
        , "."
        , docFromString name
        , ":"
        , pretty (srcLocStartLine loc)
        , ":"
        , pretty (srcLocStartCol loc)
        ]
        ) .
  getCallStack

-- | Pretty-print a CallStack just as GHC would.
ppCallStackGHC :: CallStack -> Doc ann
ppCallStackGHC =
  PP.vcat .
  map (\(name, loc) ->
        docFromString name <> ", called at" <+> docFromString (prettySrcLoc loc)) .
  getCallStack
