{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module AsciiRenderTests where

import           Control.Monad ( unless )
import           Data.String ( fromString )
import           Data.Text ( Text )
import qualified Data.Text as T
import           Lens.Micro ( (^.), (.~), (%~), (&) )
import           Test.Tasty
import           Test.Tasty.HUnit

import           SampleTables
import           TestQQDefs

import qualified Data.KVITable as KVI
import qualified Data.KVITable.Render as KTR
import qualified Data.KVITable.Render.ASCII as KTRA


cmpTables :: Text -> Text -> Text -> IO ()
cmpTables nm actual expected =
  unless (expected == actual) $ do
    let dl (e,a) = if e == a then db e else de " ↱" e <> "\n    " <> da " ↳" a
        db b = "|        > " <> b
        de m e = "|" <> m <> "expect> " <> e
        da m a = "|" <> m <> "actual> " <> a
        el = T.lines expected
        al = T.lines actual
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
    -- writeFile "test.txt" $ T.unpack $ T.unlines al
    assertFailure $ T.unpack $ T.unlines details



testAsciiRendering =
  testGroup "ASCII rendering" $
  let kvi0 = mempty :: KVI.KVITable Text
      cfg0 = KTR.defaultRenderConfig
      cfgWBlankRows = cfg0 { KTR.hideBlankRows = False }
  in
    [
      testCase "empty table" $
      cmpTables "empty table" (KTRA.render cfg0 kvi0) [sq|
****
| Value |
+-------+
****
|]

    , testCase "empty table with blanks" $
      cmpTables "empty table" (KTRA.render cfgWBlankRows kvi0) [sq|
****
| Value |
+-------+
|       |
****
|]

    , testCase "empty table with labels" $
      let kvi = mempty & KVI.keyVals @Float .~ [ ("foo", []), ("dog", []) ]
      in cmpTables "empty table with labels" (KTRA.render cfg0 kvi) [sq|
****
| foo | dog | Value |
+-----+-----+-------+
****|]

    , testCase "add key" $
      let t0 = mempty & KVI.keyVals .~ [ ("foo", ["bar", "baz"]) ] :: KVI.KVITable Text
          t1 = KVI.insert [ ("foo", "baz"), ("dog", "woof") ] "yo" $
               KVI.insert [ ("foo", "bar") ] "hi"
               t0
      in do KVI.rows t1 @?= [ ([ "bar", "" ], "hi")
                        , ([ "baz", "woof"], "yo")
                        ]
            cmpTables "add key" (KTRA.render cfg0 t1) [sq|
****
| foo |  dog | Value |
+-----+------+-------+
| bar |      |    hi |
| baz | woof |    yo |
****|]


    , testCase "deep add" $
      let t0 = mempty
               & KVI.keyVals .~ [ ("foo", ["bar", "baz"])
                                , ("moon", ["beam", "pie"])
                                ]
               & KVI.valueColName .~ "says"
               & KVI.keyValGen .~ const "?"
               :: KVI.KVITable Text
          t1 = KVI.insert [ ("foo", "Bill"), ("moon", "Ted"), ("dog", "arf arf") ] "Excellent!" $
               KVI.insert [ ("foo", "baz"), ("moon", "beam"), ("dog", "woof") ] "yo" $
               KVI.insert [ ("foo", "bar"), ("moon", "pie") ] "hi"
               t0
      in cmpTables "add key" (KTRA.render cfg0 t1) [sq|
****
|  foo | moon |     dog |       says |
+------+------+---------+------------+
| Bill |  Ted | arf arf | Excellent! |
|  bar |  pie |       ? |         hi |
|  baz | beam |    woof |         yo |
****|]

    , testCase "medium sized table render, sorted" $
      cmpTables "medium table"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|     gcc7 |    no |            1 |  good |
|     gcc7 |   yes |            0 |  good |
|     gcc7 |   yes |            3 |  ugly |
|     gcc8 |   yes |            0 |  good |
|     gcc8 |   yes |            1 |   bad |
|     gcc8 |   yes |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|   clang7 |    no |            0 |  good |
|   clang7 |    no |            1 |  good |
|   clang7 |    no |            3 |  good |
|   clang7 |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|  clang10 |   yes |            3 |  good |
****|]

    , testCase "medium sized table render, unsorted" $
      cmpTables "medium table"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Nothing }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |   yes |            0 |  good |
|     gcc7 |   yes |            3 |  ugly |
|     gcc7 |    no |            0 |   bad |
|     gcc7 |    no |            1 |  good |
|     gcc8 |   yes |            0 |  good |
|     gcc8 |   yes |            1 |   bad |
|     gcc8 |   yes |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|  clang10 |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|   clang7 |   yes |            3 |  good |
|   clang7 |    no |            0 |  good |
|   clang7 |    no |            1 |  good |
|   clang7 |    no |            3 |  good |
****|]

    , testCase "medium table, sorted, blank, row skip, no repeats" $
      cmpTables "medium table"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|          |   yes |            3 |  good |
****|]

    , testCase "medium table, sorted, blank row skip, no repeats, group unknown" $
      cmpTables "medium table s 0brow 0rep [unknown]"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = ["unknown"]
                         }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|          |   yes |            3 |  good |
****|]

    , testCase "medium table, sorted, blank row skip, no repeats, group compiler" $
      cmpTables "medium table s 0brow 0rep [unknown]"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = ["compiler"]
                         }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
+----------+-------+--------------+-------+
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
+----------+-------+--------------+-------+
|   clang6 |   yes |            0 |    ok |
+----------+-------+--------------+-------+
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
|  clang10 |    no |            3 |  good |
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
****|]

    , testCase "medium table, sorted, blank row skip, no repeats, multi-group" $
      cmpTables "medium table s 0brow 0rep [compiler,debug]"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = ["unknown", "compiler", "unk", "debug", "huh"]
                         }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|          |       |            1 |  good |
|          |-------+--------------+-------+
|          |   yes |            0 |  good |
|          |       |            3 |  ugly |
+----------+-------+--------------+-------+
|     gcc8 |   yes |            0 |  good |
|          |       |            1 |   bad |
|          |       |            3 |  true |
+----------+-------+--------------+-------+
|   clang6 |   yes |            0 |    ok |
+----------+-------+--------------+-------+
|   clang7 |    no |            0 |  good |
|          |       |            1 |  good |
|          |       |            3 |  good |
|          |-------+--------------+-------+
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
|  clang10 |    no |            3 |  good |
|          |-------+--------------+-------+
|          |   yes |            3 |  good |
+----------+-------+--------------+-------+
****|]

    , testCase "medium table, sorted, blank" $
      cmpTables "medium table"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.hideBlankRows = False }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|     gcc7 |    no |            1 |  good |
|     gcc7 |    no |            3 |       |
|     gcc7 |   yes |            0 |  good |
|     gcc7 |   yes |            1 |       |
|     gcc7 |   yes |            3 |  ugly |
|     gcc8 |    no |            0 |       |
|     gcc8 |    no |            1 |       |
|     gcc8 |    no |            3 |       |
|     gcc8 |   yes |            0 |  good |
|     gcc8 |   yes |            1 |   bad |
|     gcc8 |   yes |            3 |  true |
|   clang6 |    no |            0 |       |
|   clang6 |    no |            1 |       |
|   clang6 |    no |            3 |       |
|   clang6 |   yes |            0 |    ok |
|   clang6 |   yes |            1 |       |
|   clang6 |   yes |            3 |       |
|   clang7 |    no |            0 |  good |
|   clang7 |    no |            1 |  good |
|   clang7 |    no |            3 |  good |
|   clang7 |   yes |            0 |       |
|   clang7 |   yes |            1 |       |
|   clang7 |   yes |            3 |  good |
|  clang10 |    no |            0 |       |
|  clang10 |    no |            1 |       |
|  clang10 |    no |            3 |  good |
|  clang10 |   yes |            0 |       |
|  clang10 |   yes |            1 |       |
|  clang10 |   yes |            3 |  good |
****|]

    , testCase "medium sized table render, sorted, no blank, colstack unknown" $
      cmpTables "medium table s 0blnk colstk=unknown"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.colStackAt  = Just "unknown"
                         }) mediumKVI) [sq|
****
| compiler | debug | optimization | Value |
+----------+-------+--------------+-------+
|     gcc7 |    no |            0 |   bad |
|     gcc7 |    no |            1 |  good |
|     gcc7 |   yes |            0 |  good |
|     gcc7 |   yes |            3 |  ugly |
|     gcc8 |   yes |            0 |  good |
|     gcc8 |   yes |            1 |   bad |
|     gcc8 |   yes |            3 |  true |
|   clang6 |   yes |            0 |    ok |
|   clang7 |    no |            0 |  good |
|   clang7 |    no |            1 |  good |
|   clang7 |    no |            3 |  good |
|   clang7 |   yes |            3 |  good |
|  clang10 |    no |            3 |  good |
|  clang10 |   yes |            3 |  good |
****|]

    , testCase "medium sized table render, sorted, !blank, colstk optimization" $
      cmpTables "medium table s 0blnk colstk=optimization"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.hideBlankRows = True
                         , KTR.colStackAt  = Just "optimization"
                         }) mediumKVI) [sq|
****
| compiler | debug |    0 |    1 |    3 | <- optimization
+----------+-------+------+------+------+
|     gcc7 |    no |  bad | good |      |
|     gcc7 |   yes | good |      | ugly |
|     gcc8 |   yes | good |  bad | true |
|   clang6 |   yes |   ok |      |      |
|   clang7 |    no | good | good | good |
|   clang7 |   yes |      |      | good |
|  clang10 |    no |      |      | good |
|  clang10 |   yes |      |      | good |
****|]

    , testCase "medium, sorted, !blank, !row rpt, rgrp compiler colstk optimization" $
      cmpTables "medium table s 0blnk colstk=optimization rowgrp=compiler"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.hideBlankRows = True
                         , KTR.colStackAt  = Just "optimization"
                         , KTR.rowRepeat   = False
                         , KTR.rowGroup    = [ "compiler" ]
                         }) mediumKVI) [sq|
****
| compiler | debug |    0 |    1 |    3 | <- optimization
+----------+-------+------+------+------+
|     gcc7 |    no |  bad | good |      |
|          |   yes | good |      | ugly |
+----------+-------+------+------+------+
|     gcc8 |   yes | good |  bad | true |
+----------+-------+------+------+------+
|   clang6 |   yes |   ok |      |      |
+----------+-------+------+------+------+
|   clang7 |    no | good | good | good |
|          |   yes |      |      | good |
+----------+-------+------+------+------+
|  clang10 |    no |      |      | good |
|          |   yes |      |      | good |
+----------+-------+------+------+------+
****|]

    , testCase "medium sized table render, sorted, !blank, equisize, colstack debug" $
      cmpTables "medium table s 0blnk equisize colstk=debug"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = True
                           , KTR.colStackAt  = Just "debug"
                           }) mediumKVI) [sq|
****
| compiler | _______ no _______ | ______ yes _______ | <- debug
|          |    0 |    1 |    3 |    0 |    1 |    3 | <- optimization
+----------+------+------+------+------+------+------+
|     gcc7 |  bad | good |      | good |      | ugly |
|     gcc8 |      |      |      | good |  bad | true |
|   clang6 |      |      |      |   ok |      |      |
|   clang7 | good | good | good |      |      | good |
|  clang10 |      |      | good |      |      | good |
****|]

    , testCase "medium sized table render, sorted, !blank, fitsize, colstack debug" $
      cmpTables "medium table s 0blnk fitsize colstk=debug"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           , KTR.colStackAt  = Just "debug"
                           }) mediumKVI) [sq|
****
| compiler | _______ no _______ | ______ yes ______ | <- debug
|          |    0 |    1 |    3 |    0 |   1 |    3 | <- optimization
+----------+------+------+------+------+-----+------+
|     gcc7 |  bad | good |      | good |     | ugly |
|     gcc8 |      |      |      | good | bad | true |
|   clang6 |      |      |      |   ok |     |      |
|   clang7 | good | good | good |      |     | good |
|  clang10 |      |      | good |      |     | good |
****|]

    , testCaseSteps "small table right aligned" $ \step ->
      let tbl = foldl KVI.foldlInsert
                (mempty & KVI.valueColName .~ "name" :: KVI.KVITable Text) inp
          inp = [ ([("id", "2")], "Layla")
                , ([("id", "3")], "Jack Gabriel")
                , ([("id", "1")], "Sam")
                ]
      in do step "rows"  -- generally in insert order (foldr pushes from end)
            KVI.rows tbl @?= [ (["1"], "Sam")
                             , (["3"], "Jack Gabriel")
                             , (["2"], "Layla")
                             ]
            step "ASCII rendering unsorted"
            cmpTables "small table right aligned unsorted"
              (KTRA.render (cfg0 { KTR.sortKeyVals = Nothing }) tbl) [sq|
****
| id |         name |
+----+--------------+
|  1 |          Sam |
|  3 | Jack Gabriel |
|  2 |        Layla |
****|]
            step "ASCII rendering sorted"
            cmpTables "small table right aligned sorted"
              (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha }) tbl) [sq|
****
| id |         name |
+----+--------------+
|  1 |          Sam |
|  2 |        Layla |
|  3 | Jack Gabriel |
****|]

    , testCase "small multi-column float value table rendering" $
      let tbl = foldl KVI.foldlInsert
                (mempty & KVI.valueColName .~ "Annual Rainfall" :: KVI.KVITable Float) inp
          inp = [ ([("City name", "Adelaide"), ("Area", "1295"), ("Population", "1158259")],  600.5)
                , ([("City name", "Brisbane"), ("Area", "5905"), ("Population", "1857594")], 1146.4)
                , ([("City name", "Darwin"),   ("Area", "112"),  ("Population", "120900")],  1714.7)
                , ([("City name", "Hobart"),   ("Area", "1357"), ("Population", "205556")],   619.5)
                , ([("City name", "Melbourne"),("Area", "1566"), ("Population", "3806092")],  646.9)
                , ([("City name", "Perth"),    ("Area", "5386"), ("Population", "1554769")],  869.4)
                , ([("City name", "Sydney"),   ("Area", "2058"), ("Population", "4336374")], 1214.8)
                ]
      in cmpTables "small table float value table"
         (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha }) tbl) [sq|
****
| City name | Area | Population | Annual Rainfall |
+-----------+------+------------+-----------------+
|  Adelaide | 1295 |    1158259 |           600.5 |
|  Brisbane | 5905 |    1857594 |          1146.4 |
|    Darwin |  112 |     120900 |          1714.7 |
|    Hobart | 1357 |     205556 |           619.5 |
| Melbourne | 1566 |    3806092 |           646.9 |
|     Perth | 5386 |    1554769 |           869.4 |
|    Sydney | 2058 |    4336374 |          1214.8 |
****|]

    , testCase "big table grouped sorted" $
      cmpTables "big table grouped sorted"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.rowGroup = [ "Location", "Biome", "Category" ]
                         }) zooTable2)
      [uq_f|examples/zoo.md|]

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
         (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                            , KTR.rowRepeat = False
                            , KTR.rowGroup = [ "Location", "Biome", "Category" ]
                            , KTR.colStackAt = Just "Name"
                            , KTR.equisizedCols = False
                            }) zt)
         [uq2_f|examples/zoo.md|]

    , testCase "big table grouped sorted equisized" $
      cmpTables "big table grouped sorted equisized"
      (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.hideBlankCols = False
                         , KTR.equisizedCols = True
                         , KTR.rowGroup = [ "Branch" ]
                         , KTR.colStackAt = Just "ghcver"
                         }) testedTable)
      [sq|
        # Note: no seplines under system because it wasn't included in the row_group
****
|        system |     Branch |   Strategy | ___ ghc844 ____ | ___ ghc865 ____ | ___ ghc882 ____ | ___ ghc890 ____ | <- ghcver
|               |            |            |      N |      Y |      N |      Y |      N |      Y |      N |      Y | <- debug
+---------------+------------+------------+--------+--------+--------+--------+--------+--------+--------+--------+
| x86_64-darwin |    develop |      HEADs |      + |        |        |        |        |        |        |        |
|               |------------+------------+--------+--------+--------+--------+--------+--------+--------+--------+
|  x86_64-linux | PR-feature |      HEADs |        |        | FAIL*2 |        | FAIL*1 |        |        |        |
|               |            | submodules |      + |      + |      + |        | FAIL*1 |        |        |        |
|               |------------+------------+--------+--------+--------+--------+--------+--------+--------+--------+
|               |    develop |      HEADs |      + |      + |      + |        |        |        |        |        |
|               |            | submodules |      + |        |        |        | FAIL*1 |        |        |        |
|               |------------+------------+--------+--------+--------+--------+--------+--------+--------+--------+
|               |     master |      HEADs |        | FAIL*1 | FAIL*1 |        | FAIL*1 |        |        |        |
|               |            | submodules |      + | FAIL*1 | FAIL*1 |        |        |        |        |        |
|               |------------+------------+--------+--------+--------+--------+--------+--------+--------+--------+
****|]

    , testCase "big table grouped sorted fitsize colstack=ghcver" $
      cmpTables "big table grouped sorted fitsize colstack=ghcver"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.hideBlankCols = False
                         , KTR.equisizedCols = False
                         , KTR.rowGroup = [ "Branch" ]
                         , KTR.colStackAt = Just "ghcver"
                         }) testedTable)
        [sq|
        # Note: no seplines under system because it wasn't included in the row_group
****
|        system |     Branch |   Strategy | _ ghc844 _ | _ ghc865 _ | _ ghc882 _ | ghc890 | <- ghcver
|               |            |            | N |      Y |      N | Y |      N | Y |  N | Y | <- debug
+---------------+------------+------------+---+--------+--------+---+--------+---+----+---+
| x86_64-darwin |    develop |      HEADs | + |        |        |   |        |   |    |   |
|               |------------+------------+---+--------+--------+---+--------+---+----+---+
|  x86_64-linux | PR-feature |      HEADs |   |        | FAIL*2 |   | FAIL*1 |   |    |   |
|               |            | submodules | + |      + |      + |   | FAIL*1 |   |    |   |
|               |------------+------------+---+--------+--------+---+--------+---+----+---+
|               |    develop |      HEADs | + |      + |      + |   |        |   |    |   |
|               |            | submodules | + |        |        |   | FAIL*1 |   |    |   |
|               |------------+------------+---+--------+--------+---+--------+---+----+---+
|               |     master |      HEADs |   | FAIL*1 | FAIL*1 |   | FAIL*1 |   |    |   |
|               |            | submodules | + | FAIL*1 | FAIL*1 |   |        |   |    |   |
|               |------------+------------+---+--------+--------+---+--------+---+----+---+
****|]

    , testCase "big table grouped sorted fitsize colstack=Strategy" $
      cmpTables "big table grouped sorted fitsize colstack=Strategy"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                         , KTR.rowRepeat = False
                         , KTR.hideBlankCols = False
                         , KTR.equisizedCols = False
                         , KTR.rowGroup = [ "Branch" ]
                         , KTR.colStackAt = Just "Strategy"
                         }) testedTable)
        [sq|
        # Note: no seplines under system because it wasn't included in the row_group
****
|        system |     Branch | ___________________ HEADs ___________________ | ________________ submodules _________________ | <- Strategy
|               |            | _ ghc844 _ | _ ghc865 _ | _ ghc882 _ | ghc890 | _ ghc844 _ | _ ghc865 _ | _ ghc882 _ | ghc890 | <- ghcver
|               |            | N |      Y |      N | Y |      N | Y |  N | Y | N |      Y |      N | Y |      N | Y |  N | Y | <- debug
+---------------+------------+---+--------+--------+---+--------+---+----+---+---+--------+--------+---+--------+---+----+---+
| x86_64-darwin |    develop | + |        |        |   |        |   |    |   |   |        |        |   |        |   |    |   |
|               |------------+---+--------+--------+---+--------+---+----+---+---+--------+--------+---+--------+---+----+---+
|  x86_64-linux | PR-feature |   |        | FAIL*2 |   | FAIL*1 |   |    |   | + |      + |      + |   | FAIL*1 |   |    |   |
|               |------------+---+--------+--------+---+--------+---+----+---+---+--------+--------+---+--------+---+----+---+
|               |    develop | + |      + |      + |   |        |   |    |   | + |        |        |   | FAIL*1 |   |    |   |
|               |------------+---+--------+--------+---+--------+---+----+---+---+--------+--------+---+--------+---+----+---+
|               |     master |   | FAIL*1 | FAIL*1 |   | FAIL*1 |   |    |   | + | FAIL*1 | FAIL*1 |   |        |   |    |   |
|               |------------+---+--------+--------+---+--------+---+----+---+---+--------+--------+---+--------+---+----+---+
****|]

    , testCase "nested table hide=blankRows,blankCols colstack=ones" $
      cmpTables "nested table hide=blankRows,blankCols colstack=ones"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "ones"
                           }) nestedTable)
        [sq|
****
| millions | thousands | hundreds | tens |    0 |   1 | <- ones
+----------+-----------+----------+------+------+-----+
|        0 |         0 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|          |         1 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|          |         2 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|        1 |         0 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|          |         1 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|          |         2 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|        2 |         0 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|          |         1 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
|          |         2 |        1 |    2 | even | odd |
|          |           |        2 |    2 | even | odd |
****|]

    , testCase "nested table colstack=tens" $
      cmpTables "nested table colstack=tens"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "tens"
                           }) nestedTable)
        -- Note: the nestedTable declares a KeyVal of 0 for each key;
        -- without hideBlankCols true, this test would show a 0 value
        -- column for the tens stacked column.
        [sq|
****
| millions | thousands | hundreds | ___ 2 ____ | <- tens
|          |           |          |    0 |   1 | <- ones
+----------+-----------+----------+------+-----+
|        0 |         0 |        1 | even | odd |
|          |           |        2 | even | odd |
|          |         1 |        1 | even | odd |
|          |           |        2 | even | odd |
|          |         2 |        1 | even | odd |
|          |           |        2 | even | odd |
|        1 |         0 |        1 | even | odd |
|          |           |        2 | even | odd |
|          |         1 |        1 | even | odd |
|          |           |        2 | even | odd |
|          |         2 |        1 | even | odd |
|          |           |        2 | even | odd |
|        2 |         0 |        1 | even | odd |
|          |           |        2 | even | odd |
|          |         1 |        1 | even | odd |
|          |           |        2 | even | odd |
|          |         2 |        1 | even | odd |
|          |           |        2 | even | odd |
****|]

    , testCase "nested table hide=blankCols,blankRows colstack=hundreds" $
      cmpTables "nested table hide-blankCols,blankRows colstack=hundreds"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "hundreds"
                           }) nestedTable)
        [uq_f|README.md|]

    , testCase "nested table hide=none colstack=hundreds" $
      cmpTables "nested table hide=none colstack=hundreds"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = False
                           , KTR.hideBlankRows = False
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "hundreds"
                           }) nestedTable)
        [uq_f|examples/hundreds_all.md|]

    , testCase "nested table hide=none colstack=hundreds testfile" $
      cmpTables "nested table hide=none colstack=hundreds testfile"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = False
                           , KTR.hideBlankRows = False
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "hundreds"
                           }) nestedTable)
        [sq_f|test/evenodd.md|]

    , testCase "nested table hide=none colstack=hundreds, maxCells=60" $
      cmpTables "nested table hide=none colstack=hundreds, maxCells=60"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = False
                           , KTR.hideBlankRows = False
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "hundreds"
                           , KTR.maxCells = 60
                           }) nestedTable)
        [sq3_f|test/evenodd.md|]

    , testCase "nested table hide=none colstack=hundreds equisized" $
      cmpTables "nested table hide=none colstack=hundreds equisized"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = False
                           , KTR.hideBlankRows = False
                           , KTR.equisizedCols = True
                           , KTR.colStackAt = Just "hundreds"
                           }) nestedTable)
        [uq2_f|examples/hundreds_all.md|]

    , testCase "nested table hideBlank=rol,col colstack=thousands" $
      cmpTables "nested table hideBlank=row,col colstack=thousands"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           , KTR.colStackAt = Just "thousands"
                           }) nestedTable)
        [uq2_f|README.md|]

    , testCase "nested table hideBlank=rol,col" $
      cmpTables "nested table hideBlank=row,col"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           }) nestedTable)
        [uq3_f|README.md|]

    , testCase "nested table hideBlank=rol,col, maxCells=60" $
      cmpTables "nested table hideBlank=row,col, maxCells=60"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = True
                           , KTR.hideBlankRows = True
                           , KTR.equisizedCols = False
                           , KTR.maxCells = 60
                           }) nestedTable)
        [sq5_f|test/evenodd.md|]

    , testCase "nested table hideBlank=none" $
      cmpTables "nested table hideBlank=none"
        (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                           , KTR.rowRepeat = False
                           , KTR.hideBlankCols = False
                           , KTR.hideBlankRows = False
                           , KTR.equisizedCols = False
                           }) nestedTable)
        [sq|
****
| millions | thousands | hundreds | tens | ones | Value |
+----------+-----------+----------+------+------+-------+
|        0 |         0 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         1 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         2 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|        1 |         0 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         1 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         2 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|        2 |         0 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         1 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         2 |        0 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |        1 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    0 |    0 |       |
|          |           |          |      |    1 |       |
|          |           |          |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
****|]


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
         (KTRA.render (cfg0 { KTR.sortKeyVals = Just KTR.sortNumericAlpha
                            , KTR.rowRepeat   = False
                            , KTR.rowGroup    = [ "Location", "Biome", "Category" ]
                            , KTR.colStackAt  = Just "Batter"
                            , KTR.equisizedCols = False
                            , KTR.maxCols = 20
                            , KTR.maxCells = 600
                            }) tbl)
         [sq_f|test/bigsquare.md|]
      ]
