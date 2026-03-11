{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module HTMLRenderTests where

import           Control.Monad ( unless )
import qualified Data.List as L
import           Data.String ( fromString )
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Lens.Micro ( (^.), (.~), (%~), (&) )
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.HTML.Parser ( parseTokens, renderToken
                                  , canonicalizeTokens
                                  , Token( TagOpen, TagSelfClose ) )

import           SampleTables
import           TestQQDefs

import qualified Data.KVITable as KVI
import qualified Data.KVITable.Render as KTR
import qualified Data.KVITable.Render.HTML as KTRH


cmpTables :: Text -> Text -> Text -> IO ()
cmpTables nm actual expected = do
  let expH = normalize $ parseTokens $ T.concat $ fmap T.strip $ T.lines expected
      actH = normalize $ parseTokens actual
      normalize = fmap sortAttrs . canonicalizeTokens
      sortAttrs (TagOpen n a) = TagOpen n $ L.sort a
      sortAttrs (TagSelfClose n a) = TagSelfClose n $ L.sort a
      sortAttrs t = t
  unless (expH == actH) $ do
    let dl (e,a) = if e == a then db e else de " ↱" e <> "\n    " <> da " ↳" a
        db b = "|        > " <> b
        de m e = "|" <> m <> "expect> " <> e
        da m a = "|" <> m <> "actual> " <> a
        el = fmap (TL.toStrict . renderToken) expH
        al = fmap (TL.toStrict . renderToken) actH
        addnum n l = let nt = T.pack (show n)
                         nl = T.length nt
                     in T.take (4 - nl) "    " <> nt <> l
    let details = ("MISMATCH between " <>
                   T.pack (show $ length el) <> " expected and " <>
                   T.pack (show $ length al) <> " actual for " <> nm) :
                  (fmap (uncurry addnum) $
                   zip [1..] $ concat $
                   -- Highly simplistic "diff" output assumes
                   -- correlated lines: added or removed lines just
                   -- cause everything to shown as different from that
                   -- point forward.
                   [ fmap dl $ zip el al
                   , fmap (de "∌ ") $ drop (length al) el
                   , fmap (da "∹ ") $ drop (length el) al
                   ])
    -- writeFile "test.html" $ T.unpack $ T.unlines al
    assertFailure $ T.unpack $ T.unlines details


testHTMLRendering =
  testGroup "HTML rendering" $
  let kvi0 = mempty :: KVI.KVITable Text
      cfg0 = KTR.defaultRenderConfig
      cfgWBlankRows = cfg0 { KTR.hideBlankRows = False }
  in
    [
      testCase "empty table, hide blank" $
      cmpTables "empty table, hide blank"
      (KTRH.render cfg0 kvi0) [sq|
****
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr"><th class="kvitable_th"><div><span>Value</span></div></th></tr>
  </thead>
  <tbody class="kvitable_body">
  </tbody>
</table>
****
|]

    , testCase "empty table, show blank" $
      cmpTables "empty table, show blank"
      (KTRH.render cfgWBlankRows kvi0) [sq|
****
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr"><th class="kvitable_th"><div><span>Value</span></div></th></tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <td class="kvitable_td"></td>
    </tr>
  </tbody>
</table>
****
|]

    , testCase "empty table with labels" $
      let kvi = mempty & KVI.keyVals @Float .~ [ ("foo", []), ("dog", []) ]
      in cmpTables "empty table with labels" (KTRH.render cfg0 kvi) [sq|
****
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>foo</span></div></th>
      <th class="kvitable_th"><div><span>dog</span></div></th>
      <th class="kvitable_th"><div><span>Value</span></div></th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
  </tbody>
</table>
****
|]

    , testCase "nested table hideBlank=rows,cols, fitted, colstack=hundreds" $
      cmpTables "nested table hideBlank=rows,cols, fitted, colstack=hundreds"
      (KTRH.render (cfg0 { KTR.sortKeyVals   = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat     = False
                         , KTR.hideBlankCols = True
                         , KTR.hideBlankRows = True
                         , KTR.equisizedCols = False
                         , KTR.colStackAt    = Just "hundreds"
                         }) nestedTable)
        [sq_f|README.md|]

    , testCase "nested table hide=none, fitted, colstack=hundreds" $
      cmpTables "nested table hide=none, fitted, colstack=hundreds"
      (KTRH.render (cfg0 { KTR.sortKeyVals   = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat     = False
                         , KTR.hideBlankCols = False
                         , KTR.hideBlankRows = False
                         , KTR.equisizedCols = False
                         , KTR.colStackAt    = Just "hundreds"
                         }) nestedTable)
      [sq_f|examples/hundreds_all.md|]

        -- duplication of the above in test/evenodd.md
    , testCase "nested table hide=none, fitted, colstack=hundreds" $
      do
       cmpTables "nested table hide=none, fitted, colstack=hundreds"
        (KTRH.render (cfg0 { KTR.sortKeyVals   = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat     = False
                           , KTR.hideBlankCols = False
                           , KTR.hideBlankRows = False
                           , KTR.equisizedCols = False
                           , KTR.colStackAt    = Just "hundreds"
                           }) nestedTable)
        [sq2_f|test/evenodd.md|]

    , testCase "nested table hide=none, fitted, colstack=hundreds, maxCells=60" $
      do
       cmpTables "nested table hide=none, fitted, colstack=hundreds, maxCells=60"
        (KTRH.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat     = False
                         , KTR.hideBlankCols = False
                         , KTR.hideBlankRows = False
                         , KTR.equisizedCols = False
                         , KTR.colStackAt    = Just "hundreds"
                         , KTR.maxCells      = 60
                         }) nestedTable)
        [sq4_f|test/evenodd.md|]

    , testCase "nested table hide=none, fitted, no colstack, maxCells=60" $
      do
       cmpTables "nested table hide=none, fitted, no colstack, maxCells=60"
        (KTRH.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat     = False
                         , KTR.hideBlankCols = False
                         , KTR.hideBlankRows = False
                         , KTR.equisizedCols = False
                         , KTR.colStackAt    = Nothing
                         , KTR.maxCells      = 60
                         }) nestedTable)
        [sq6_f|test/evenodd.md|]

    , testCase "nested table hideBlank=rol,col colstack=thousands" $
      cmpTables "nested table hideBlank=row,col colstack=thousands"
        (KTRH.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "thousands"
                           }) nestedTable)
        [sq2_f|README.md|]

    , testCase "nested table hideBlank=rol,col" $
      cmpTables "nested table hideBlank=row,col"
        (KTRH.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False

                           }) nestedTable)
        [sq3_f|README.md|]

    , testCase "big table grouped sorted" $
      cmpTables "big table grouped sorted"
      (KTRH.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = [ "Location", "Biome", "Category" ]
                         }) zooTable2)
      [sq_f|examples/zoo.md|]

    , testCase "big table grouped sorted no-subtype colstack" $
      let zt = KVI.fromList $
               foldl rmvSubtype [] $
               KVI.toList zooTable2
          rmvSubtype newl (keyspec, v) =
            let ks = filter (("Subtype" /=) . fst) keyspec
            in case lookup ks newl of
                 Nothing -> (ks,v) : newl
                 Just v' -> (ks, v' + v) : filter ((ks /=) . fst) newl
      in cmpTables "big table grouped sorted no-subtype colstack"
         (KTRH.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                            , KTR.rowRepeat   = False
                            , KTR.rowGroup    = [ "Location", "Biome", "Category" ]
                            , KTR.colStackAt  = Just "Name"
                            , KTR.equisizedCols = False
                            }) zt)
         [sq2_f|examples/zoo.md|]

    , testCase "big square table, numCols and numCells limited" $
      let tbl = foldl KVI.foldlInsert
                (mempty & KVI.valueColName .~ "Hits"
                 & KVI.keyVals
                  .~ [ ("Pitcher", [ fromString ("P" <> show n)
                                   | n <- [0..100::Int] ])
                     , ("Batter", [ fromString ("B" <> show n)
                                  | n <- [0..100::Int] ])
                     ])
                [ ([ ("Pitcher", fromString ("P" <> show p))
                   , ("Batter", fromString ("B" <> show b))
                   ], p * b)
                | p <- [0..100::Int]
                , b <- [0..100::Int]
                ]
      in cmpTables "big table grouped sorted no-subtype colstack"
         (KTRH.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                            , KTR.rowRepeat   = False
                            , KTR.rowGroup    = [ "Location", "Biome", "Category" ]
                            , KTR.colStackAt  = Just "Batter"
                            , KTR.equisizedCols = False
                            , KTR.maxCols = 20
                            , KTR.maxCells = 600
                            }) tbl)
         [sq2_f|test/bigsquare.md|]
    ]
