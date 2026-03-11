This example uses the same generated `KVITable` data that was
described in the README, but formats the output differently.

```haskell
render (defaultRenderConfig { KTR.sortKeyVals   = True
                            , KTR.rowRepeat     = False
                            , KTR.hideBlankCols = False
                            , KTR.hideBlankRows = False
                            , KTR.equisizedCols = False
                            , KTR.colStackAt    = Just "hundreds"
                            }
       )
       nestedTable
```

Here, the hiding of blank rows and columns is disabled, which makes
the table significantly larger:

```
____ snip vv ____
| millions | thousands | _____ 0 _____ | _______ 1 ________ | _______ 2 ________ | <- hundreds
|          |           | _ 0 _ | _ 2 _ | _ 0 _ | ___ 2 ____ | _ 0 _ | ___ 2 ____ | <- tens
|          |           | 0 | 1 | 0 | 1 | 0 | 1 |    0 |   1 | 0 | 1 |    0 |   1 | <- ones
+----------+-----------+---+---+---+---+---+---+------+-----+---+---+------+-----+
|        0 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|        1 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|        2 |         0 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         1 |   |   |   |   |   |   | even | odd |   |   | even | odd |
|          |         2 |   |   |   |   |   |   | even | odd |   |   | even | odd |
____ snip ^^ ____
```

*****
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th"><div><span>millions</span></div></th>
      <th rowspan="3" class="kvitable_th"><div><span>thousands</span></div></th>
      <th colspan="4" class="kvitable_th multicol"><div><span>0</span></div></th>
      <th colspan="4" class="kvitable_th multicol"><div><span>1</span></div></th>
      <th colspan="4" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;hundreds</th>
    </tr>
    <tr class="kvitable_tr">
      <th colspan="2" class="kvitable_th multicol"><div><span>0</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>0</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>2</span></div></th>
      <th colspan="2" class="kvitable_th multicol"><div><span>0</span></div></th>
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
      <th rowspan="3" class="kvitable_th"><div><span>0</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>2</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th"><div><span>1</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>2</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th rowspan="3" class="kvitable_th"><div><span>2</span></div></th>
      <th class="kvitable_th"><div><span>0</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>1</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>2</span></div></th>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td"></td>
      <td class="kvitable_td">even</td>
      <td class="kvitable_td">odd</td>
    </tr>
  </tbody>
</table>

*****

## Equisized columns

For the ASCII layout, the rendering configuration can set
`equisizedCols` to `True` to keep each column the same size.

```
____ snip vv ____
| millions | thousands | ___________ 0 ___________ | ___________ 1 ___________ | ___________ 2 ___________ | <- hundreds
|          |           | ____ 0 ____ | ____ 2 ____ | ____ 0 ____ | ____ 2 ____ | ____ 0 ____ | ____ 2 ____ | <- tens
|          |           |    0 |    1 |    0 |    1 |    0 |    1 |    0 |    1 |    0 |    1 |    0 |    1 | <- ones
+----------+-----------+------+------+------+------+------+------+------+------+------+------+------+------+
|        0 |         0 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|          |         1 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|          |         2 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|        1 |         0 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|          |         1 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|          |         2 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|        2 |         0 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|          |         1 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
|          |         2 |      |      |      |      |      |      | even |  odd |      |      | even |  odd |
____ snip ^^ ____
```

This setting has no effect on the HTML layout; to achieve the same
effect for HTML, CSS settings should be used.
