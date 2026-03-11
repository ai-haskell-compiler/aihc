{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.KVITable.Render.Internal where

import qualified Data.List as L
import           Data.Name ( ConvertName, UTF8 )
import           Data.String ( fromString )
import           Numeric.Natural

import           Data.KVITable
import           Data.KVITable.Internal.Helpers
import           Data.KVITable.Render


-- TODO: to allow for hideBlankCols, the KVITable should keep track of what the
-- pre-declared vals for each key are v.s. what additional vals may be set by
-- setting a value in that region.  Then there is enough information here
-- (without re-scanning the table) to properly compute the keyvals that should be
-- displayed (which would also mean that the hideCols/hideRows determinations in
-- the rendering functions below are no longer needed.

data TblHdr = V (KeyVal) | AndMore Natural

type TblHdrs = [ (Key, [TblHdr]) ]

-- | Returns the rows and columns KeyVals, with appropriate application of
-- RenderConfig specifications: colStackAt, maxCells, maxCols.  Does not collapse
-- empty rows or columns.

renderingKeyVals :: RenderConfig
                 -> KeyVals
                 -> (TblHdrs, TblHdrs)
renderingKeyVals cfg inpKvs =
  let maxNumKeys = maxCells cfg
      origNumKeys = toEnum $ length inpKvs
      maxNumCols = min (maxCells cfg) (maxCols cfg)

  in case colStackAt cfg of
       Nothing ->
         -- width is just keys, height is combination of keys and values
         let okKvs = if origNumKeys > maxNumKeys
                     then snoc (take (fromEnum maxNumKeys) (fst kvs))
                          (fromString
                           $ "{+ " <> show (origNumKeys - maxNumKeys) <> " MORE}"
                          , mempty
                          )
                     else (fst kvs)
             -- n.b. maxCols is not really useful here, since all but the last
             -- column are headers and values are only shown in that last column.
         in (snd $ trimStacked True 1 maxNumKeys okKvs, [])
       Just _c ->
         let (kvsRows, kvsCols) = kvs
             numRegularColKvs = let v = length inpKvs - length kvsCols
                                in if v < 0 then error "BAD1" else toEnum v
             numStackedCols = countStacked kvsCols
             origNumCols = numRegularColKvs + numStackedCols
             allowedNumCols = subOrDef 1 maxNumCols numRegularColKvs
             okKvsCols = if origNumCols > maxNumCols
                      then if numStackedCols <= maxNumCols
                           then kvsCols
                           else snd $ trimStacked False 1 allowedNumCols kvsCols
                      else kvsCols
             allowedNumRows = subOrDef 1 (maxCells cfg)
                           (if origNumCols > maxNumCols
                             then if numStackedCols <= maxNumCols
                                  then numStackedCols
                                  else allowedNumCols
                             else numStackedCols
                           )
             eachRowCols = min maxNumCols numStackedCols
             okKvsRows = snd $ trimStacked False eachRowCols allowedNumRows kvsRows
         in (okKvsRows, okKvsCols)

  where

    -- subtracting Naturals must be done carefully to not allow the result to be
    -- < 0; a post-subtraction max is not enough to protect against the initial
    -- value.
    subOrDef d a b = if a < b then d else a - b

    kvs = let kvs' = case colStackAt cfg of
                       Nothing -> (inpKvs, mempty)
                       Just c -> span ((/= c) . fst) inpKvs
              ksrt = case sortKeyVals cfg of
                       Nothing -> kvs'
                       Just fn -> fn kvs'
          in ( fmap (fmap V) <$> fst ksrt
             , fmap (fmap V) <$> snd ksrt
             )

    countStacked = \case -- does not allow for hiddenCols
      [] -> 1
      ((_,vs):r) -> toEnum (L.length vs) * countStacked r
    trimStacked _ each n [] = ((n,each), [])
    trimStacked _mulSubs each n ((k,vs):[]) =
      let lvs = toEnum $ length vs
          mvs = foldl (\a b -> if b * each < n then b else a) 1 $ [0..lvs]
          tvs = snoc (take (fromEnum mvs) vs) $ AndMore $ lvs - mvs
          rvs = if mvs < lvs then tvs else vs
      in ((subOrDef 0 n mvs, mvs * each), [(k,rvs)])
    trimStacked mulSubs each n ((k,vs):rkvs) =
      let lvs = toEnum $ length vs
          ((n',w), kvs') = trimStacked mulSubs each n rkvs
          mvs = foldl (\a b -> if b * w < n then b else a) 1 $ [0..lvs]
          tvs = snoc (take (fromEnum mvs) vs) $ AndMore $ remcnt (lvs - mvs) rkvs
          rvs = if mvs < lvs then tvs else vs
      in ((subOrDef 0 n' mvs, mvs * w), (k,rvs):kvs')

    remcnt n [] = n
    remcnt n (rkv:rkvs) = n * remcnt (toEnum $ length (snd rkv)) rkvs


instance ConvertName UTF8 "Key" "column header"
instance ConvertName UTF8 "KeyVal" "column header"


nLength :: Foldable t => t a -> Natural
nLength = toEnum . length
