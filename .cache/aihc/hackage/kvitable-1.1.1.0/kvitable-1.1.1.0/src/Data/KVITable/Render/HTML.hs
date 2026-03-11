{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides the 'KVITable' 'render' function for
-- rendering the table in a HTML table format.  The various HTML table
-- entries have class designators that allow the user to provide CSS
-- to adjust the appearance of the table.

module Data.KVITable.Render.HTML
  (
    render
    -- re-export Render definitions to save the caller an additional import
  , RenderConfig(..)
  , defaultRenderConfig
  )
where

import qualified Data.Foldable as F
import qualified Data.List as L
import           Data.List.NonEmpty ( NonEmpty( (:|) ) )
import qualified Data.List.NonEmpty as NEL
import           Data.Maybe ( isNothing )
import           Data.Name ( Named, HTMLStyle, UTF8, convertName
                           , convertStyle, nameText )
import           Data.String ( fromString )
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lens.Micro ( (^.) )
import           Lucid
import           Numeric.Natural
import           Text.Sayable

import           Data.KVITable as KVIT
import           Data.KVITable.Internal.Helpers
import           Data.KVITable.Render
import           Data.KVITable.Render.Internal

import           Prelude hiding ( lookup )


-- | Renders the specified table in HTML format, using the specified
-- 'RenderConfig' controls.  The output is only the @<table>@
-- definition; it is intended to be embedded in a larger HTML
-- document.

render :: Sayable "html" v => RenderConfig -> KVITable v -> Text
render cfg t =
  let kmap = renderingKeyVals cfg $ t ^. keyVals
      (fmt, hdr) = renderHdrs cfg kmap t
      bdy = renderSeq cfg fmt kmap t
  in TL.toStrict $ renderText $
     table_ [ class_ "kvitable" ] $
     do maybe mempty (caption_ . toHtml . convertStyle @UTF8 @HTMLStyle)
          $ Data.KVITable.Render.caption cfg
        thead_ [ class_ "kvitable_head" ] hdr
        tbody_ [ class_ "kvitable_body" ] bdy


instance ToHtml (Named HTMLStyle nameOf) where
  toHtml = toHtmlRaw . nameText
  -- Note: toHtml uses toHtmlRaw because Named HTMLStyle is already escaped
  toHtmlRaw = toHtmlRaw . nameText

----------------------------------------------------------------------

data FmtLine = FmtLine [Natural]  -- colspans, length is # columns

instance Semigroup FmtLine where
  (FmtLine c1) <> (FmtLine c2) = FmtLine $ c1 <> c2

instance Monoid FmtLine where
  mempty = FmtLine mempty

fmtAddColLeft :: Natural -> FmtLine -> FmtLine
fmtAddColLeft lspan (FmtLine col) = FmtLine $ lspan : col

data FmtVal = Val Span LastInGroup Bool Text
            | Hdr Span LastInGroup (Named HTMLStyle "column header")
data Span = Singular | Cols Width | Rows Height
type Height = Natural
type Width = Natural
type LastInGroup = Bool
type RightLabel = Named HTMLStyle "column header"

fmtRender :: FmtLine -> Maybe RightLabel -> [FmtVal] -> Html ()
fmtRender (FmtLine cols) mbRLabel vals = do
  tr_ [ class_ "kvitable_tr" ] $
    let excessColCnt = length cols - length vals
        cell (w,Hdr h l v) =
          let a = [ [ class_ "kvitable_th" ]
                  , case h of
                      Singular -> mempty
                      Cols n -> if w == 1 && n == 1
                                then mempty
                                else [ colspan_ $ T.pack $ show (n*w)
                                     , class_ " multicol"
                                     ]
                      Rows 1 -> mempty
                      Rows n -> [ rowspan_ $ T.pack $ show n ]
                  , if w == 1
                       || case h of
                            Cols n | n > 1 -> True
                            _ -> False
                    then mempty
                    else [ colspan_ $ T.pack $ show w
                         , class_ " multicol" ]
                  , if l then [ class_ " last_in_group" ] else mempty
                  ]
          in th_ (concat $ reverse a) (div_ $ span_ $ toHtml v)
        cell (w,Val h l i v) =
          let a = [ [ class_ "kvitable_td" ]
                  , case h of
                      Singular -> mempty
                      Cols 1 -> mempty
                      Cols n -> [ colspan_ $ T.pack $ show n ]
                      Rows 1 -> mempty
                      Rows n -> [ rowspan_ $ T.pack $ show n ]
                  , if w == 1 then mempty
                    else [ colspan_ $ T.pack $ show w
                         , class_ " multicol" ]
                  , if l then [ class_ " last_in_group" ] else mempty
                  ]
          in td_ (concat $ reverse a) $ if i then i_ (toHtml v) else toHtml v
        labelMark = toHtmlRaw ("&nbsp;&larr;" :: Text)
        labelHtml = th_ [ class_ "rightlabel kvitable_th" ] .
                    (labelMark <>) .
                    toHtml
    in do mapM_ cell $ L.filter ((/= 0) . fst) $
            zip (drop excessColCnt cols) vals
          maybe mempty labelHtml mbRLabel


----------------------------------------------------------------------

data HeaderLine = HdrLine FmtLine HdrVals Trailer
type HdrVals = [FmtVal]
type Trailer = Maybe RightLabel

instance Semigroup HeaderLine where
  (HdrLine fmt1 hv1 t1) <> (HdrLine fmt2 hv2 _) =
    HdrLine (fmt1 <> fmt2) (hv1 <> hv2) t1

hdrFmt :: HeaderLine -> FmtLine
hdrFmt (HdrLine fmt _ _) = fmt

renderHdrs :: Sayable "html" v
           => RenderConfig
           -> (TblHdrs, TblHdrs)
           -> KVITable v
           -> ( FmtLine, Html () )
renderHdrs cfg kmap t = ( rowfmt, sequence_ hdrs )
  where
    hdrs = fmap renderHdr hrows
    (hrows, rowfmt) = hdrstep cfg t kmap
    renderHdr (HdrLine fmt hdrvals trailer) = fmtRender fmt trailer hdrvals

hdrstep :: Sayable "html" v
        => RenderConfig
        -> KVITable v
        -> (TblHdrs, TblHdrs)
        -> (NEL.NonEmpty HeaderLine, FmtLine)
hdrstep _cfg t ([], []) =
  let hdr = Hdr Singular False $ t ^. valueColName
      one = single 1
  in ( HdrLine (FmtLine one) (single hdr) Nothing :| mempty
     , FmtLine one
     )
hdrstep cfg t ([], colKeys) =
  hdrvalstep cfg t colKeys mempty -- switch to column stacking mode
hdrstep cfg t ((key,_) : keys, colKeys) =
  let (nexthdr0 :| nexthdrs, lowestfmt) = hdrstep cfg t (keys, colKeys)
      (HdrLine fmt vals tr) = nexthdr0
      fmt' = fmtAddColLeft 1 fmt
      val = Hdr (Rows $ nLength nexthdrs + 1) False
            $ convertStyle $ convertName key
  in ( (HdrLine fmt' (val : vals) tr) :| nexthdrs
     , fmtAddColLeft 1 lowestfmt
     )

hdrvalstep :: Sayable "html" v
           => RenderConfig
           -> KVITable v
           -> TblHdrs
           -> KeySpec
           -> (NEL.NonEmpty HeaderLine, FmtLine)
hdrvalstep _ _ [] _ = error "HTML hdrvalstep with empty keys after matching colStackAt -- impossible"
hdrvalstep cfg t ((key, titles) : []) steppath =
  let cvalWidths kv = fmap (length . sez @"html" . snd) $
                      L.filter ((L.isSuffixOf (snoc steppath (key, kv))) . fst)
                      $ KVIT.toList t
      cwidth = \case
        V c -> if hideBlankCols cfg && 0 == (sum $ cvalWidths c) then 0 else 1
        AndMore _ -> 1
      fmt = FmtLine (cwidth <$> titles)
      hdr = Hdr Singular False . toHdrText <$> titles
      k = convertStyle @UTF8 @HTMLStyle $ convertName key
  in ( HdrLine fmt hdr (Just k) :| mempty, fmt)
hdrvalstep _cfg _t ((_key, []) : _keys) _steppath = error "cannot happen"
hdrvalstep cfg t ((key, ttl:ttls) : keys) steppath =
  let
    titles = ttl :| ttls
    subhdrsV v = hdrvalstep cfg t keys (case v of
                                          V kv -> snoc steppath (key,kv)
                                          _ -> steppath
                                       )
    subTtlHdrs :: NEL.NonEmpty (NEL.NonEmpty HeaderLine, FmtLine)
    subTtlHdrs = subhdrsV <$> titles
    subhdrs :: NEL.NonEmpty (NEL.NonEmpty HeaderLine, FmtLine)
    subhdrs = if hideBlankCols cfg
              then subTtlHdrs
              else
                -- Want to repeat the first element of subTtlHdrs to get a
                -- NonEmpty the same length as titles.  Both titles and
                -- subTtlHdrs are NonEmpty, but NonEmpty has no replicate
                -- function.
                let n = length titles -- >= 1 because titles is NonEmpty
                    e = NEL.head subTtlHdrs
                    tail' = NEL.take (n-1) $ NEL.repeat e
                in e :| tail'
    subhdr_rollup = joinHdrs <$> NEL.transpose (fst <$> subhdrs)
    joinHdrs :: NEL.NonEmpty HeaderLine -> HeaderLine
    joinHdrs (hl0 :| hls) = foldl (<>) hl0 hls
    superFmt :: (NEL.NonEmpty HeaderLine, FmtLine) -> Natural
    superFmt sub = let FmtLine subcols = hdrFmt $ NEL.last $ fst sub
                   in if sum subcols == 0
                      then 0
                      else nLength $ L.filter (/= 0) subcols
    topfmt = FmtLine $ NEL.toList (superFmt <$> subhdrs)
    tophdr = let h = Hdr Singular False . toHdrText <$> titles
                 tr = convertStyle @UTF8 @HTMLStyle $ convertName key
             in HdrLine topfmt (NEL.toList h) $ Just tr
  in ( NEL.cons tophdr subhdr_rollup, F.fold (snd <$> subTtlHdrs))


toHdrText :: TblHdr -> Named HTMLStyle "column header"
toHdrText th = case toHdrText' th of
                 Right t -> t
                 Left n -> fromString $ sez @"html" $ t'"{+" &+ n &+ '}'

toHdrText' :: TblHdr -> Either Natural (Named HTMLStyle "column header")
toHdrText' = \case
  V kv -> Right $ convertStyle @UTF8 @HTMLStyle $ convertName kv
  AndMore n -> Left n

----------------------------------------------------------------------

renderSeq :: Sayable "html" v
          => RenderConfig -> FmtLine
          -> (TblHdrs, TblHdrs)
          -> KVITable v
          -> Html ()
renderSeq cfg fmt kmap t =
  let lst = htmlRows kmap mempty
      rndr = fmtRender fmt Nothing
  in sequence_ (each rndr lst)
  where
    each = map

    filterBlank = if hideBlankRows cfg
                  then L.filter (not . all isNothing)
                  else id

    mkVal = Val Singular False False . T.pack . sez @"html"

    htmlRows :: (TblHdrs, TblHdrs) -> KeySpec -> [ [FmtVal] ]
    htmlRows ([], []) path =
      let v = lookup' path t
          skip = case v of
            Nothing -> hideBlankRows cfg
            Just _ -> False
          row = maybe (Val Singular False False "") mkVal v
      in if skip then mempty else single $ single row
    htmlRows ([], colKeyMap) path =
          let filterOrDefaultBlankRows =
                fmap (fmap (maybe (Val Singular False False "") id)) . filterBlank
          in filterOrDefaultBlankRows $ single $ multivalRows colKeyMap path
    htmlRows ((key,keyvals) : kseq, colKeyMap) path =
      let subrows = \case
            V keyval -> htmlRows (kseq, colKeyMap) $ snoc path (key,keyval)
            _ -> [ [Val (Cols nCols) True True "more"] ]
          nCols = product ( nLength . snd <$> colKeyMap )
          nHdrs = nLength kseq + 1
          endOfGroup = key `elem` rowGroup cfg
          genSubrows keyval =
            let sr = subrows keyval
                kv = toHdrText' keyval
            in fst
               $ foldl (leftAdd (nLength sr)) (mempty, Just kv)
               $ reverse
               $ zip (endOfGroup : L.repeat False)
               $ reverse sr
          leftAdd nrows (acc,mb'kv) (endGrp, subrow) =
            let sr = setValGrouping endGrp <$> subrow
                setValGrouping g (Val h g' i v) = Val h (g || g') i v
                setValGrouping g (Hdr h g' v) = Hdr h (g || g') v
            in ( snoc acc
                 (case mb'kv of
                     Nothing -> sr
                     Just (Right kv) ->
                       let w = if rowRepeat cfg then 1 else nrows
                       in Hdr (Rows w) endOfGroup kv : sr
                     Just (Left n) ->
                       let m = fromString $ sez @"html" $ t'"{+" &+ n &+ '}'
                       in Hdr (Cols nHdrs) True m : sr
                 )
               , if rowRepeat cfg then mb'kv else Nothing)
      in concat $ each genSubrows keyvals

    multivalRows [] _ = error "HTML multivalRows cannot be called with no keys!"
    multivalRows ((key, keyvals) : []) path =
      (\case
          V v -> mkVal <$> lookup' (snoc path (key,v)) t
          _ -> Just $ Val Singular False True "... -->"
      ) <$> keyvals
    multivalRows ((key, keyvals) : kseq) path =
      concatMap (\case
                   V v -> multivalRows kseq (snoc path (key,v))
                   _ -> mempty
                ) keyvals
