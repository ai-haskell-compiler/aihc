{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module provides the 'KVITable' 'render' function for
-- rendering the table in a plain ASCII format.

module Data.KVITable.Render.ASCII
  (
    render
    -- re-export Render definitions to save the caller an additional import
  , RenderConfig(..)
  , defaultRenderConfig
  )
where

import qualified Data.List as L
import           Data.Maybe ( isNothing )
import           Data.Name
import           Data.String ( fromString )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (^.) )
import           Numeric.Natural
import           Text.Sayable

import           Data.KVITable ( KVITable, KeySpec, keyVals )
import qualified Data.KVITable as KVIT
import           Data.KVITable.Internal.Helpers
import           Data.KVITable.Render
import           Data.KVITable.Render.Internal

import           Prelude hiding ( lookup )


-- | Renders the specified table in ASCII format, using the specified
-- 'RenderConfig' controls.

render :: Sayable "normal" v => RenderConfig -> KVITable v -> Text
render cfg t =
  let kmap = renderingKeyVals cfg $ t ^. keyVals
      (fmt, hdr) = renderHdrs cfg t kmap
      bdy = renderSeq cfg fmt kmap t
  in T.unlines $ hdr <> bdy

----------------------------------------------------------------------

data FmtLine = FmtLine [Natural] Sigils Sigils  -- last is for sepline
data Sigils = Sigils { sep :: Text, pad :: Text, cap :: Text }

fmtLine :: [Natural] -> FmtLine
fmtLine cols = FmtLine cols
               Sigils { sep = "|", pad = " ", cap = "_" }
               Sigils { sep = "+", pad = "-", cap = "_" }

fmtColCnt :: FmtLine -> Natural
fmtColCnt (FmtLine cols _ _) = nLength cols

perColOvhd :: Natural
perColOvhd = 2 -- pad chars on either side of each column's entry

-- | Formatted width of output, including pad on either side of each
-- column's value (but not the outer set), and a separator between columns.
--
-- Note that a column size of 0 indicates that hideBlankCols is active
-- and the column was found to be empty of values, so it should not be
-- counted.
fmtWidth :: FmtLine -> Natural
fmtWidth (FmtLine cols _ _) =
  let cols' = L.filter (/= 0) cols
  in sum cols' + ((perColOvhd + 1) * (nLength cols' - 1))

fmtEmptyCols :: FmtLine -> Bool
fmtEmptyCols (FmtLine cols _ _) = sum cols == 0

fmtAddColLeft :: Natural -> FmtLine -> FmtLine
fmtAddColLeft leftCol (FmtLine cols s s') = FmtLine (leftCol : cols) s s'

data FmtVal = Separator | TxtVal Text | CenterVal Text
            | Overage  -- cells for the overflow row/column

fmtRender :: FmtLine -> [FmtVal] -> Text
fmtRender (FmtLine _cols _sigils _sepsigils) [] = ""
fmtRender (FmtLine cols sigils sepsigils) vals@(val:_) =
  if length cols == length vals
  then let sig f o = case o of
                       Separator   -> f sepsigils
                       TxtVal _    -> f sigils
                       CenterVal _ -> f sigils
                       Overage     -> f sigils
           l = sig sep val
           charRepeat n c = T.pack (replicate (fromEnum n) c)
           rightAlign n t = let tl = toEnum $ T.length t
                                rt = charRepeat (n - tl) ' ' <> t
                            in if tl >= n then t else rt
           centerIn n t = let tl = toEnum $ T.length t
                              (w,e) = (n - tl - 2) `divMod` 2
                              m = cap sigils
                              ls = T.replicate (fromEnum $ w + 0) m
                              rs = T.replicate (fromEnum $ w + e) m
                          in if tl + 2 >= n
                             then rightAlign n t
                             else ls <> " " <> t <> " " <> rs
       in l <>
          T.concat
          [ sig pad fld <>
            (case fld of
                Separator   -> charRepeat sz '-'
                TxtVal v    -> rightAlign sz v
                CenterVal t -> centerIn sz t
                Overage     -> centerIn sz "+"
            ) <>
            sig pad fld <>
            sig sep fld  -- KWQ or if next fld is Nothing
          | (sz,fld) <- zip cols vals, sz /= 0
          ]
  else error ("Insufficient arguments (" <>
              show (length vals) <> ")" <>
              " for FmtLine " <> show (length cols))

----------------------------------------------------------------------

data HeaderLine = HdrLine FmtLine HdrVals Trailer
type HdrVals = [FmtVal]
type Trailer = Name "column header"

hdrFmt :: HeaderLine -> FmtLine
hdrFmt (HdrLine fmt _ _) = fmt

renderHdrs :: Sayable "normal" v
           => RenderConfig -> KVITable v -> (TblHdrs, TblHdrs)
           -> (FmtLine, [Text])
renderHdrs cfg t kmap =
  ( lastFmt
  , [ fmtRender fmt hdrvals
      <> (if nullName trailer then "" else (" <- " <> nameText trailer))
    | (HdrLine fmt hdrvals trailer) <- hrows
    ] <>
    (single $ let sz = fromEnum $ fmtColCnt lastFmt
              in fmtRender lastFmt (replicate sz Separator)) )
  where
    hrows = hdrstep cfg t kmap
    lastFmt = case reverse hrows of
                [] -> fmtLine mempty
                (hrow:_) -> hdrFmt hrow

hdrstep :: Sayable "normal" v
        => RenderConfig
        -> KVITable v
        -> (TblHdrs, TblHdrs)
        -> [HeaderLine]
hdrstep _cfg t ([], []) =
  -- colStackAt wasn't recognized, so devolve into a non-colstack table
  let valcoltxt = t ^. KVIT.valueColName
      valcoltsz = nameLength valcoltxt
      valsizes  = nLength . sez @"normal" . snd <$> KVIT.toList t
      valwidth  = maxOf 0 $ valcoltsz : valsizes
      hdrVal = TxtVal $ nameText valcoltxt
  in single $ HdrLine (fmtLine $ single valwidth) (single hdrVal) ""
hdrstep cfg t ([], colKeyMap) =
  hdrvalstep cfg t colKeyMap mempty  -- switch to column-stacking mode
hdrstep cfg t ((key,keyvals) : keys, colKeyMap) =
  let keyw = max (nameLength key)
             $ (maxOf 0 . fmap (nameLength . toHdrText)) keyvals
      mkhdr (hs, v) (HdrLine fmt hdrvals trailer) =
        ( HdrLine (fmtAddColLeft keyw fmt) (TxtVal (nameText v) : hdrvals) trailer : hs , "")
  in reverse $ fst $ foldl mkhdr (mempty, key) $ hdrstep cfg t (keys, colKeyMap)
  -- first line shows hdrval for non-colstack'd columns, others are blank

hdrvalstep :: Sayable "normal" v
           => RenderConfig -> KVITable v -> TblHdrs -> KeySpec
           -> [HeaderLine]
hdrvalstep cfg t ((key,titles) : []) steppath =
  let cvalWidths = \case
        V kv -> fmap (nLength . sez @"normal" . snd)
                $ filter ((L.isSuffixOf (snoc steppath (key, kv))) . fst)
                $ KVIT.toList t
        AndMore _ -> [1] -- always show this column, although it has no contents
      colWidth kv = let cvw = cvalWidths kv
                    in if hideBlankCols cfg && sum cvw == 0
                       then 0
                       else maxOf (nameLength $ toHdrText kv) cvw
      cwidths = fmap colWidth titles
      fmtcols = if equisizedCols cfg
                then (replicate (length cwidths) (maxOf 0 cwidths))
                else cwidths
      tr = convertName key
      hdrTxt = nameText . toHdrText
  in single $ HdrLine (fmtLine $ fmtcols) (TxtVal . hdrTxt <$> titles) tr
hdrvalstep cfg t ((key,vals) : keys) steppath =
  let subhdrsV = \case
        V v -> hdrvalstep cfg t keys (snoc steppath (key,v))
        _ -> mempty
      subTtlHdrs = let subAtVal v = (nameLength $ toHdrText v, subhdrsV v)
                   in fmap subAtVal vals
      szexts = let subW (hl,sh) =
                     case sh of
                       [] -> (0, 0)  -- should never be the case
                       (sh0:_) ->
                         let sv = fmtWidth $ hdrFmt sh0
                         in if hideBlankCols cfg && (fmtEmptyCols $ hdrFmt sh0)
                            then (0, 0)
                            else (hl, sv)
               in fmap (uncurry max . subW) subTtlHdrs
      rsz_extsubhdrs = fmap hdrJoin $
                       L.transpose $
                       fmap (uncurry rsz_hdrstack) $
                       zip szhdrs $ fmap snd subTtlHdrs
      largest = maxOf 0 szexts
      szhdrs = if equisizedCols cfg && not (hideBlankCols cfg)
               then replicate (length vals) largest
               else szexts
      rsz_hdrstack s vhs = fmap (rsz_hdrs s) vhs
      rsz_hdrs hw (HdrLine (FmtLine c s j) v r) =
        let nzCols = L.filter (/= 0) c
            numNZCols = nLength nzCols
            pcw = sum nzCols + ((perColOvhd + 1) * (numNZCols - 1))
            (ew,w0) = let l = length nzCols
                      in if l == 0 then (0,0)
                         else max 0 (hw - pcw) `divMod` numNZCols
            c' = fst $ foldl (\(c'',n) w -> (snoc c'' $ n+w, ew)) (mempty,ew+w0) c
        in HdrLine (FmtLine c' s j) v r
      hdrJoin hl = foldl hlJoin (HdrLine (fmtLine mempty) mempty "") hl
      hlJoin (HdrLine (FmtLine c s j) v _) (HdrLine (FmtLine c' _ _) v' r) =
        HdrLine (FmtLine (c<>c') s j) (v<>v') r
      tvals = CenterVal . nameText . toHdrText <$> vals
  in HdrLine (fmtLine szhdrs) tvals (convertName key) : rsz_extsubhdrs
hdrvalstep _ _ [] _ = error "ASCII hdrvalstep with empty keys after matching colStackAt -- impossible"

toHdrText :: TblHdr -> Name "column header"
toHdrText = \case
  V kv -> convertName kv
  AndMore n -> fromString $ sez @"normal" $ t'"{+" &+ n &+ '}'

----------------------------------------------------------------------

renderSeq :: Sayable "normal" v
          => RenderConfig
          -> FmtLine
          -> (TblHdrs, TblHdrs)
          -> KVITable v
          -> [Text]
renderSeq cfg fmt kmap kvitbl = fmtRender fmt . snd <$> asciiRows kmap mempty
  where
    filterBlank = if hideBlankRows cfg
                  then L.filter (not . all isNothing . snd)
                  else id
    asciiRows :: (TblHdrs, TblHdrs)
              -> KeySpec
              -> [ (Bool, [FmtVal]) ]
    asciiRows ([], []) path =
      let v = KVIT.lookup' path kvitbl
          skip = case v of
                   Nothing -> hideBlankRows cfg
                   Just _  -> False
      in if skip then mempty
         else single $ (False, single $ maybe (TxtVal "") TxtVal (T.pack . sez @"normal" <$> v) )
    asciiRows ([], colKeyMap) path =
      let filterOrDefaultBlankRows = fmap (fmap defaultBlanks) . filterBlank
          defaultBlanks = fmap (\v -> maybe (TxtVal "") TxtVal v)
      in filterOrDefaultBlankRows $ single $ (False, multivalRows colKeyMap path)
    asciiRows ((key, keyvals) : kseq, colKeyMap) path =
      let subrows = \case
            V keyval -> asciiRows (kseq, colKeyMap) $ snoc path (key, keyval)
            _ ->
              let ttlcols = product ( length . snd <$> colKeyMap)
              in [ (False , replicate (ttlcols + length kseq) $ Overage) ]
          grprow = \case
            subs@(sub0:_) | key `elem` rowGroup cfg ->
              let subl = single (True, replicate (length $ snd sub0) Separator)
              in if fst (last subs) then init subs <> subl else subs <> subl
            subs -> subs
          genSubRow keyval = grprow $ fst
                             $ foldl leftAdd (mempty, toHdrText keyval) $ subrows keyval
          leftAdd (acc,kv) (b,subrow) =
            (snoc acc (b, TxtVal (nameText kv) : subrow)
            , if rowRepeat cfg then kv else ""
            )
      in concat (genSubRow <$> keyvals)

    multivalRows :: TblHdrs -> KeySpec -> [ Maybe Text ]
    multivalRows ((key, keyvals) : []) path =
      let showEnt = T.pack . sez @"normal"
      in (\case
             V v -> (showEnt <$> (KVIT.lookup' (snoc path (key,v)) kvitbl))
             _ -> Nothing
         ) <$> keyvals
    multivalRows ((key, keyvals) : kseq) path =
      concatMap (\case
                    V v -> multivalRows kseq (snoc path (key,v))
                    _ -> mempty
                ) keyvals
    multivalRows [] _ = error "multivalRows cannot be called with no keys!"
