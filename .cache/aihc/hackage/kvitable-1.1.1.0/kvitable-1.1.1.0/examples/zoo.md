In this example, the inventory of various Zoo animals from different
City Zoos is maintained in a `KVITable`, where each animal is
additionally classified by parameters such as Biome and Diet.

The `KVITable` creation is somewhat lengthy due to the size of the
inventory; the [SampleTables.hs](../test/SampleTables.hs) file should be
consulted for the definition of the `KVITable` `zooTable` and
`zooTable2`.

Rendering the data without any column stacking, but with multiple row
groupings:

```
import Data.KVITable.Render.ASCII

render (defaultRenderConfig
        { sortKeyVals = True
        , rowRepeat   = False
        , rowGroup = [ "Location", "Biome", "Category" ]
        })
       zooTable2
```

results in the following:

```
____ snip vv ____
|  Location |    Biome | Category |      Diet |    Name |     Subtype | Count |
+-----------+----------+----------+-----------+---------+-------------+-------+
|        LA |   Jungle |   Animal | Herbivore |   Hippo |             |     1 |
|           |----------+----------+-----------+---------+-------------+-------+
|           | Savannah |   Animal | Carnivore |    Lion |             |     4 |
|           |          |          | Herbivore | Giraffe |             |     2 |
|           |          |          |           |   Rhino |             |     3 |
+-----------+----------+----------+-----------+---------+-------------+-------+
|     Miami |    Polar |     Bird | Carnivore | Penguin |      Gentoo |    20 |
|           |----------+----------+-----------+---------+-------------+-------+
|           | Savannah |   Animal | Carnivore |    Lion |             |     2 |
|           |          |          | Herbivore | Giraffe | Reticulated |     3 |
+-----------+----------+----------+-----------+---------+-------------+-------+
|  New York | Savannah |   Animal | Carnivore |    Lion |             |     3 |
+-----------+----------+----------+-----------+---------+-------------+-------+
| San Diego |   Jungle |   Animal |  Omnivore |    Bear |         Sun |     1 |
|           |----------+----------+-----------+---------+-------------+-------+
|           |   Plains |   Animal |  Omnivore |    Bear |       Black |     1 |
|           |          |          |           |         |       Brown |     1 |
|           |----------+----------+-----------+---------+-------------+-------+
|           |    Polar |   Animal |  Omnivore |    Bear |       Polar |     1 |
|           |          |----------+-----------+---------+-------------+-------+
|           |          |     Bird | Carnivore | Penguin |     Emperor |     8 |
|           |          |          |           |         |      Gentoo |     2 |
|           |----------+----------+-----------+---------+-------------+-------+
|           | Savannah |   Animal | Carnivore |    Lion |             |     9 |
+-----------+----------+----------+-----------+---------+-------------+-------+
____ snip ^^ ____
```

or in HTML (with CSS inherited from the Markdown
configuration... imagine how much nicer this could be with your own
CSS styling!):

******

<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>Location</span></div></th>
      <th class="kvitable_th"><div><span>Biome</span></div></th>
      <th class="kvitable_th"><div><span>Category</span></div></th>
      <th class="kvitable_th"><div><span>Diet</span></div></th>
      <th class="kvitable_th"><div><span>Name</span></div></th>
      <th class="kvitable_th"><div><span>Subtype</span></div></th>
      <th class="kvitable_th"><div><span>Count</span></div></th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=4><div><span>LA</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Jungle</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Herbivore</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Hippo</span></div></th>
      <th class="kvitable_th last_in_group"><div><span></span></div></th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group" rowspan=3><div><span>Animal</span></div></th>
      <th class="kvitable_th"><div><span>Carnivore</span></div></th>
      <th class="kvitable_th"><div><span>Lion</span></div></th>
      <th class="kvitable_th"><div><span></span></div></th>
      <td class="kvitable_td">4</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th" rowspan=2><div><span>Herbivore</span></div></th>
      <th class="kvitable_th"><div><span>Giraffe</span></div></th>
      <th class="kvitable_th"><div><span></span></div></th>
      <td class="kvitable_td">2</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Rhino</span></div></th>
      <th class="kvitable_th last_in_group"><div><span></span></div></th>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3><div><span>Miami</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Polar</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Bird</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Carnivore</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Penguin</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Gentoo</span></div></th>
      <td class="kvitable_td last_in_group">20</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Animal</span></div></th>
      <th class="kvitable_th"><div><span>Carnivore</span></div></th>
      <th class="kvitable_th"><div><span>Lion</span></div></th>
      <th class="kvitable_th"><div><span></span></div></th>
      <td class="kvitable_td">2</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Herbivore</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Giraffe</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Reticulated</span></div></th>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>New York</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Carnivore</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Lion</span></div></th>
      <th class="kvitable_th last_in_group"><div><span></span></div></th>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=7><div><span>San Diego</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Jungle</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Omnivore</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Bear</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Sun</span></div></th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Plains</span></div></th>
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Animal</span></div></th>
      <th class="kvitable_th" rowspan=2><div><span>Omnivore</span></div></th>
      <th class="kvitable_th" rowspan=2><div><span>Bear</span></div></th>
      <th class="kvitable_th"><div><span>Black</span></div></th>
      <td class="kvitable_td">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Brown</span></div></th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3><div><span>Polar</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Omnivore</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Bear</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Polar</span></div></th>
      <td class="kvitable_td last_in_group">1</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Bird</span></div></th>
      <th class="kvitable_th" rowspan=2><div><span>Carnivore</span></div></th>
      <th class="kvitable_th" rowspan=2><div><span>Penguin</span></div></th>
      <th class="kvitable_th"><div><span>Emperor</span></div></th>
      <td class="kvitable_td">8</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Gentoo</span></div></th>
      <td class="kvitable_td last_in_group">2</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Carnivore</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Lion</span></div></th>
      <th class="kvitable_th last_in_group"><div><span></span></div></th>
      <td class="kvitable_td last_in_group">9</td>
    </tr>
  </tbody>
</table>

******

## With Column Stacking

Removing the "Subtype" field from the zoo table and then modifying the
rendering configuration to add column stacking on the `"Name"` key
`Val`s results in:

```
____ snip vv ____
|  Location |    Biome | Category |      Diet | Bear | Giraffe | Hippo | Lion | Penguin | Rhino | <- Name
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
|        LA |   Jungle |   Animal | Herbivore |      |         |     1 |      |         |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           | Savannah |   Animal | Carnivore |      |         |       |    4 |         |       |
|           |          |          | Herbivore |      |       2 |       |      |         |     3 |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
|     Miami |    Polar |     Bird | Carnivore |      |         |       |      |      20 |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           | Savannah |   Animal | Carnivore |      |         |       |    2 |         |       |
|           |          |          | Herbivore |      |       3 |       |      |         |       |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
|  New York | Savannah |   Animal | Carnivore |      |         |       |    3 |         |       |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
| San Diego |   Jungle |   Animal |  Omnivore |    1 |         |       |      |         |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           |   Plains |   Animal |  Omnivore |    2 |         |       |      |         |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           |    Polar |   Animal |  Omnivore |    1 |         |       |      |         |       |
|           |          |----------+-----------+------+---------+-------+------+---------+-------+
|           |          |     Bird | Carnivore |      |         |       |      |      10 |       |
|           |----------+----------+-----------+------+---------+-------+------+---------+-------+
|           | Savannah |   Animal | Carnivore |      |         |       |    9 |         |       |
+-----------+----------+----------+-----------+------+---------+-------+------+---------+-------+
____ snip ^^ ____
```

And as HTML:

******
<table class="kvitable">
  <thead class="kvitable_head">
    <tr class="kvitable_tr">
      <th class="kvitable_th"><div><span>Location</span></div></th>
      <th class="kvitable_th"><div><span>Biome</span></div></th>
      <th class="kvitable_th"><div><span>Category</span></div></th>
      <th class="kvitable_th"><div><span>Diet</span></div></th>
      <th class="kvitable_th"><div><span>Bear</span></div></th>
      <th class="kvitable_th"><div><span>Giraffe</span></div></th>
      <th class="kvitable_th"><div><span>Hippo</span></div></th>
      <th class="kvitable_th"><div><span>Lion</span></div></th>
      <th class="kvitable_th"><div><span>Penguin</span></div></th>
      <th class="kvitable_th"><div><span>Rhino</span></div></th>
      <th class="rightlabel kvitable_th">&nbsp;&larr;Name</th>
    </tr>
  </thead>
  <tbody class="kvitable_body">
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3><div><span>LA</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Jungle</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Herbivore</span></div></th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">1</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Animal</span></div></th>
      <th class="kvitable_th"><div><span>Carnivore</span></div></th>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">4</td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Herbivore</span></div></th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">2</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">3</td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=3><div><span>Miami</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Polar</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Bird</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Carnivore</span></div></th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">20</td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Animal</span></div></th>
      <th class="kvitable_th"><div><span>Carnivore</span></div></th>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">2</td>
      <td class="kvitable_td">
      </td>
      <td class="kvitable_td">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Herbivore</span></div></th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">3</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>New York</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Carnivore</span></div></th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">3</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=5><div><span>San Diego</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Jungle</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Omnivore</span></div></th>
      <td class="kvitable_td last_in_group">1</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Plains</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Omnivore</span></div></th>
      <td class="kvitable_td last_in_group">2</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group" rowspan=2><div><span>Polar</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Omnivore</span></div></th>
      <td class="kvitable_td last_in_group">1</td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Bird</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Carnivore</span></div></th>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">
      </td>
      <td class="kvitable_td last_in_group">10</td>
      <td class="kvitable_td last_in_group">
      </td>
    </tr>
    <tr class="kvitable_tr">
      <th class="kvitable_th last_in_group"><div><span>Savannah</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Animal</span></div></th>
      <th class="kvitable_th last_in_group"><div><span>Carnivore</span></div></th>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group">9</td>
      <td class="kvitable_td last_in_group"></td>
      <td class="kvitable_td last_in_group"></td>
    </tr>
  </tbody>
</table>

******
