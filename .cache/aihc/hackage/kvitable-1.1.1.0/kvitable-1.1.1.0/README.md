The `KVITable` is similar to a `Map`, but the keys to the map are a
list of `Key=Val` data.  Although the `KVITable` is perfectly useable
as a container in this fashion, the main use of the `KVITable` is in
rendering this data in various configurations; because of this focus,
there is no particular attention to other container aspects such as:
performance, space usage, etc.

An example table can be created via:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.KVITable

nestedTable = foldl foldlInsert
              (mempty
                & keyVals .~ [ ("millions",  ["0"])
                             , ("thousands", ["0"])
                             , ("hundreds",  ["0"])
                             , ("tens",      ["0"])
                             , ("ones",      ["0"])
                             ]
              )
              [ ([("millions", T.pack $ show m)
                 ,("thousands", T.pack $ show t)
                 ,("hundreds", T.pack $ show h)
                 ,("tens", T.pack $ show d)
                 ,("ones", T.pack $ show o)],
                  if (o `rem` 2) == 1 then "odd" else "even")
              | m <- [0..2 :: Int]
              , t <- [0..2 :: Int]
              , h <- [1..2 :: Int]
              , d <- [2..2 :: Int]
              , o <- [0..1 :: Int]
              ]
```

This Haskell code generates a table where the keys are the various
scale indicators along with a corresponding key value.  The table
entries themselves are the words `odd` or `even`.

Rendering this table in ASCII mode, with blank rows and columns
hidden, and enabling column stacking at the "hundreds" key column can
be done with the following code:

```haskell
import Data.KVITable.Render.ASCII

render (defaultRenderConfig { sortKeyVals   = True
                            , rowRepeat     = False
                            , hideBlankCols = True
                            , hideBlankRows = True
                            , equisizedCols = False
                            , colStackAt    = Just "hundreds"
                            }) nestedTable
```

The output from this rendering will look like:

```
____ snip vv ____
| millions | thousands | ___ 1 ____ | ___ 2 ____ | <- hundreds
|          |           | ___ 2 ____ | ___ 2 ____ | <- tens
|          |           |    0 |   1 |    0 |   1 | <- ones
+----------+-----------+------+-----+------+-----+
|        0 |         0 | even | odd | even | odd |
|          |         1 | even | odd | even | odd |
|          |         2 | even | odd | even | odd |
|        1 |         0 | even | odd | even | odd |
|          |         1 | even | odd | even | odd |
|          |         2 | even | odd | even | odd |
|        2 |         0 | even | odd | even | odd |
|          |         1 | even | odd | even | odd |
|          |         2 | even | odd | even | odd |
____ snip ^^ ____
```

When rendered to HTML instead by using the same code but importing
`Data.KVITable.Render.HTML` (and please use a little CSS to make
things prettier), the following is obtained:

****
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th"><div><span>millions</span></div></th>
      <th rowspan="3" class="kvitable_th"><div><span>thousands</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>1</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;hundreds</th>
    </tr>
    <tr class="kvitable_tr">
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;tens</th>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;ones</th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>2</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>2</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>2</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
  </tbody>
</table>

****

## Different ColStack specification

By changing the `colStackAt` specification in the rendering
configuration from the "hundreds" column to the "thousands" column,
the column at which `Key Val`'s are shown as column headers instead of
rows is changed and the following ASCII results are obtained:

```
____ snip vv ____
| millions | __________ 0 __________ | __________ 1 __________ | __________ 2 __________ | <- thousands
|          | ___ 1 ____ | ___ 2 ____ | ___ 1 ____ | ___ 2 ____ | ___ 1 ____ | ___ 2 ____ | <- hundreds
|          | ___ 2 ____ | ___ 2 ____ | ___ 2 ____ | ___ 2 ____ | ___ 2 ____ | ___ 2 ____ | <- tens
|          |    0 |   1 |    0 |   1 |    0 |   1 |    0 |   1 |    0 |   1 |    0 |   1 | <- ones
+----------+------+-----+------+-----+------+-----+------+-----+------+-----+------+-----+
|        0 | even | odd | even | odd | even | odd | even | odd | even | odd | even | odd |
|        1 | even | odd | even | odd | even | odd | even | odd | even | odd | even | odd |
|        2 | even | odd | even | odd | even | odd | even | odd | even | odd | even | odd |
____ snip ^^ ____
```

or as HTML:

****
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th rowspan="4" class="kvitable_th"><div><span>millions</span></div></th>
      <th colspan="4" class="kvitable_th multicol"><div><span>0</span></div></th>
      <th colspan="4" class="kvitable_th multicol"><div><span>1</span></div></th>
      <th colspan="4" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;thousands</th>
    </tr>
    <tr class="kvitable_tr">
      <th colspan="2" class="kvitable_th multicol"><div><span>1</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>1</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>1</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;hundreds</th>
    </tr>
    <tr class="kvitable_tr">
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;tens</th>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>1</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;ones</th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>2</span></div></th>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
  </tbody>
</table>

****

## No column stacking

Alternatively, the `colStackAt` rendering configuration parameter may
be specified as `Nothing`, indicating that all `Key Val` values are to
be specified on separate rows, with no stacked columns.  The ASCII
form of this is:

```
____ snip vv ____
| millions | thousands | hundreds | tens | ones | Value |
+----------+-----------+----------+------+------+-------+
|        0 |         0 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         1 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         2 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|        1 |         0 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         1 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         2 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|        2 |         0 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         1 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |         2 |        1 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
|          |           |        2 |    2 |    0 |  even |
|          |           |          |      |    1 |   odd |
____ snip ^^ ____
```

and the HTML form is

****
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>millions</span></div></th>
      <th class="kvitable_th"><div><span>thousands</span></div></th>
      <th class="kvitable_th"><div><span>hundreds</span></div></th>
      <th class="kvitable_th"><div><span>tens</span></div></th>
      <th class="kvitable_th"><div><span>ones</span></div></th>
      <th class="kvitable_th"><div><span>Value</span></div></th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th rowspan="12" class="kvitable_th"><div><span>0</span></div></th>
      <th rowspan="4" class="kvitable_th"><div><span>0</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="4" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="4" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="12" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="4" class="kvitable_th"><div><span>0</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="4" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="4" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="12" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="4" class="kvitable_th"><div><span>0</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="4" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="4" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>1</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
      <tr class="kvitable_tr">
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th rowspan="2" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td">even</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td">odd</td>
    </tr>
  </tbody>
</table>

****

More examples can be found in the examples subdirectory, including:

 * [Zoo Inventory](examples/zoo.md)
