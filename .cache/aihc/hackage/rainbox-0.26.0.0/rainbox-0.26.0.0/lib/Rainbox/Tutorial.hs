-- Modules might be imported solely so Haddock can hyperlink the
-- identifiers

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

{-| The Rainbox tutorial

Rainbox helps you create arrangements of (possibly) colorful text
boxes. This module contains a tutorial.  Typically the "Rainbox"
module contains all you need.  There is also a "Rainbox.Core" module
which contains all the innards of Rainbox, but ordinarily you won't
need it.

The basic building block of Rainbox is known as a @core@.  A core is
either a single 'Chunk' or a blank box of arbitrary size.  A core made
of a single 'Chunk' should not contain any newlines.  Leave newline
handling up to Rainbox.  However, Rainbox will not check to ensure
that your 'Chunk' does not contain any newline characters.  If it does
contain newlines, your boxes will not look right.  Also, Rainbox needs
to know how wide your 'Chunk' are, in columns.  To measure width,
Rainbox simply counts the number of characters in the 'Chunk'.
Therefore, if you need accented characters, be sure to use a single
character, not composed characters.  That is, to get á, use U+00E1,
not U+00B4 and U+0061.

Many things in Rainbox have height and width.  Both the height and
width of an object can be zero, but never less than zero.  A @core@
made from a 'Chunk' always has a height of 1, and its width is equal
to the number of characters in the 'Text's that make up the 'Chunk'.
A @core@ made from a blank box has the height and width that you give
it, though neither its height nor its width is ever smaller than zero.

The next biggest building block after the @core@ is @payload@.  There
are two different types of payloads: vertical payloads and horizontal
ones.  A vertical payload aligns itself next to other vertical
payloads on a vertical axis, creating a chain of payloads.  The
vertical payload also has an alignment.  The alignment determines
whether the payload lines up along the axis on its left side, right
side, or in the center of the payload.

The vertical payload also has a background color, which has type
'Radiant'.  Think of the background color as extending infinitely from
both the left and right side of the vertical payload.  When the
vertical payload is combined with other vertical payloads into a 'Box'
'Vertical', this background color is used as necessary so that the
'Box' 'Vertical' forms a rectangle.

The horizontal payload is similar to the vertical payload, but the
axis is horizontal rather than vertical.  The alignment determines
whether the payload aligns the axis along the top, center, or bottom
of the payload.  A horizontal payload also contains a background
color; it extends infinitely from both the top and bottom of the
horizontal payload.

Finally, the biggest building block of Rainbox is the box.  There are
two types of boxes: a 'Box' 'Horizontal', which holds zero or more horizontal
payloads, and a 'Box' 'Vertical', which holds zero or more vertical payloads.
Each kind of box is a 'Monoid', so you can combine it using the usual
monoid functions.  So, to give a visual, a 'Box' 'Vertical' with five payloads
might look like this:

@

-- function: 'box1'

           V Vertical axis

           +----+
           | v1 |
           |    |
           +----+--------+
           |     v2      |
           |             |
  +--------+-------------+
  |        |
  |  v3    |
  |        |
  |        |
  +--------+----------+
           |    v4    |
      +----+----+-----+
      | v5      |
      |         |
      +---------+

@

Each payload is marked in the middle with @v1@, @v2@, etc.  Note how
each payload has a different size.  @v1@ and @v2@, and @v4@ have a
'left' alignment, as their left edge is lined up with the vertical
axis.  @v3@ has a 'right' alignment.  @v5@ has a 'center' alignment.
Think of each payload has having a background color extending
infinitely off of its left and right sides.  These five payloads put
together make a 'Box' 'Vertical'.  Since 'Box' 'Vertical' is a monoid,
you can combine various 'Box' 'Vertical'.  Indeed, the pictured 'Box'
'Vertical' can be built only by combining smaller 'Box' 'Vertical', as
when you create payloads they are always given to you as a single
payload wrapped in a 'Box' 'Vertical'.

Now, you want to render your 'Box' 'Vertical'.  You use the 'render' function,
which makes a sequence of Rainbow 'Chunk'.  This turns your 'Box' 'Vertical'
into a nice rectangle for on-screen rendering:

@

-- function: 'renderBox1'

  +--------+----+--------+
  |        | v1 |        |
  |        |    |        |
  +--------+----+--------+
  |        |     v2      |
  |        |             |
  +--------+-------------+
  |        |             |
  |  v3    |             |
  |        |             |
  |        |             |
  +--------+----------+--+
  |        |    v4    |  |
  +---+----+----+-----+--+
  |   | v5      |        |
  |   |         |        |
  +---+---------+--------+

@

The spaces to the left and right of each payload are filled in with
the appropriate background color, which is the background
color of the adjoining payload.


What if you want to place the 'Box' 'Vertical' alongside another box?  If you
want to put it next to another 'Box' 'Vertical', just use 'mappend' or '<>'. But
what if you want to put it next to a 'Box' 'Horizontal'?  Let's suppose you have a
'Box' 'Horizontal' that looks like this:

@

-- function: 'box2'

                        +----+
                        | h1 |
                        |    |
Horizontal Axis >       +----+----------+
                             |          |
                             |   h2     |
                             |          |
                             +----------+

@

The @h1@ payload has alignment 'bottom', because its bottom edge is
lined up along the horizontal axis.  The @h2@ payload has alignment
'top'.  You want to connect this 'Box' 'Horizontal' with the 'Box'
'Vertical' made above.  You can't connect them directly because they
are different types.  You can, however, take a complete 'Box' and wrap
it inside another 'Box'.  This allows you to wrap a 'Box' 'Vertical'
inside of a 'Box' 'Horizontal', and vice versa, or even wrap a 'Box'
'Horizontal' inside of another 'Box' 'Horizontal'.  You do this with
the 'wrap' function, which is applied to the alignment for the new
box, its background color, and the box you want to wrap.  ao, let's
say you take the 'Box' 'Vertical' created above and wrap it inside a
'Box' 'Horizontal' with 'top' alignment and a background color, and
then you combine it with the 'Box' 'Horizontal' created above.  The
result:

@

-- function: 'box3'

                         +----+
                         | h1 |
                         |    |
  +--------+----+--------+----+----------+
  |        | v1 |        |    |          |
  |        |    |        |    |    h2    |
  +--------+----+--------+    |          |
  |        |     v2      |    +----------+
  |        |             |
  +--------+-------------+
  |        |             |
  |  v3    |             |
  |        |             |
  |        |             |
  +--------+----------+--+
  |        |    v4    |  |
  +---+----+----+-----+--+
  |   | v5      |        |
  |   |         |        |
  +---+---------+--------+

@

The old 'Box' 'Vertical', which is now wrapped in a 'Box'
'Horizontal', now has a background color which extends infinitely from
the top and bottom of the box.  It is now just a payload inside of the
'Box' 'Horizontal'.  The other two payloads in the 'Box' 'Horizontal',
@h1@ and @h2@, also have background colors extending from their tops
and bottoms.

So, when you render this 'Box' 'Horizontal' with 'render', you get this:


@

-- function: 'renderBox3'

  +----------------------+----+----------+
  |                      | h1 |          |
  |                      |    |          |
  +--------+----+--------+----+----------+
  |        | v1 |        |    |          |
  |        |    |        |    |    h2    |
  +--------+----+--------+    |          |
  |        |     v2      |    +----------+
  |        |             |    |          |
  +--------+-------------+    |          |
  |        |             |    |          |
  |  v3    |             |    |          |
  |        |             |    |          |
  |        |             |    |          |
  +--------+----------+--+    |          |
  |        |    v4    |  |    |          |
  +---+----+----+-----+--+    |          |
  |   | v5      |        |    |          |
  |   |         |        |    |          |
  +---+---------+--------+----+----------+

@

The area above the old 'Box' 'Vertical' has the background color that we used in
the application of 'convert'.  The area below the @h1@ payload has its
background color, and the area above and below the @h2@ payload has
its background color.

What if you just want to create an empty space?  You can create entire
blank boxes with 'blank', but often it is enough to use 'spacer',
which gives you a one-dimensional 'Box' 'Horizontal' or 'Box'
'Vertical'.  If you are creating a 'Box' 'Horizontal', 'spacer' gives
you a box with width, but no height; for a 'Box' 'Vertical', you get a
box with height, but no width.  So, to return to the example 'Box'
'Horizontal', let's say you want to add a blank space a few columns
wide on the right side.  You 'mappend' a 'Box' 'Horizontal' created
with 'spacer' to get this:

@

-- function: 'box4'

                        +----+
                        | h1 |
                        |    |
Horizontal Axis >       +----+----------+--+
                             |          |
                             |   h2     |
                             |          |
                             +----------+
@

On the right side you now have a payload with width, but no height.
But it does have a background color.  So when you 'render' the box, you
get this:


@

-- function: 'renderBox4'

                        +----+----------+--+
                        | h1 |          |  |
                        |    |          |  |
                        +----+----------+  |
                        |    |          |  |
                        |    |   h2     |  |
                        |    |          |  |
                        +----+----------+--+

@

You can also use 'spreader' to make a 'Box' 'Horizontal' taller or a
'Box' 'Vertical' wider.  'spreader' creates a one-dimensional 'Box'
that is perpendicular to the axis.  Pay attention to the alignment of
the 'spreader' as this will determine how your box expands.  Let's say
I want to take the 'Box' that contains the @h1@ and @h2@ payloads as
created above, but I also want to make sure the box is at least 12
rows tall.  To do this, 'mappend' a 'spreader' that is 12 rows tall.
The result upon 'render'ing is:

@
                        
-- functions: 'box5' 'renderBox5'

                        +----+----------+--+
                        |    |          |  |
                        +----+          |  |
                        | h1 |          |  |
                        |    |          |  |
                        +----+----------+  |
                        |    |          |  |
                        |    |   h2     |  |
                        |    |          |  |
                        |    +----------+  |
                        |    |          |  |
                        +----+----------+--+
@

Those are the basics of the Rainbox model, which should be enough to
get you started.  Also helpful are the 'tableByRows' and
'tableByColumns' functions, which will help you build a simple grid
that resembles a spreadsheet; see its documentation for hints to get
started with that.  You will also find an example using the
'tableByRows', as well as code to produce all of the examples shown
above, in the source code below.

== Why the use of 'Seq' everywhere, rather than lists?

Rainbox uses 'Seq' from "Data.Sequence" because lists can be
infinite.  Practically every function in Rainbox will not accept
infinite inputs, because Rainbox needs to know exactly how long and
wide various payloads and boxes are in order to line them up
correctly.  Use of the 'Seq' most accurately reflects the fact that
Rainbox does not work on infinite inputs.

-}
module Rainbox.Tutorial where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.List (intersperse)
import Data.Monoid ((<>))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X
import qualified Rainbow
import qualified Rainbox

-- | Create a 'Box' for the given text.  The default foreground and
-- background colors of the terminal are used for the 'Text'; the
-- given background is used as the background color for any added
-- padding.
textBox :: Rainbow.Radiant -> Text -> Rainbox.Box a
textBox r = Rainbox.fromChunk Rainbox.center r . Rainbow.chunk

-- | Centers the given 'Box' within a larger 'Box' that has the given
-- height and width and background color.  The larger 'Box' has the
-- given 'Alignment'.
within
  :: Rainbox.Orientation a
  => Rainbox.Alignment a
  -> Int
  -- ^ Number of rows
  -> Int
  -- ^ Number of columns
  -> Rainbow.Radiant
  -- ^ Background color
  -> Rainbox.Box a
  -> Rainbox.Box a
within a r c b
  = Rainbox.wrap a b
  . mappend (Rainbox.spreader Rainbox.center r)
  . Rainbox.wrap Rainbox.centerH b
  . mappend (Rainbox.spreader Rainbox.center c)
  . Rainbox.wrap Rainbox.centerV b

-- | Puts the given text in the center of a box.  The resulting box is
-- center aligned.
textWithin
  :: Rainbox.Orientation a
  => Rainbox.Alignment a
  -> Int
  -- ^ Number of rows
  -> Int
  -- ^ Number of columns
  -> Rainbow.Radiant
  -- ^ Background color for smaller box
  -> Rainbow.Radiant
  -- ^ Background color for larger box
  -> Text
  -> Rainbox.Box a
textWithin a r c bs bl = Rainbox.wrap a bl . within a r c bs . textBox bs

box1 :: Rainbox.Box Rainbox.Vertical
box1 = mconcat
  [ textWithin Rainbox.left   4 6  Rainbow.blue      Rainbow.green   "v1"
  , textWithin Rainbox.left   4 15 Rainbow.red       Rainbow.magenta "v2"
  , textWithin Rainbox.right  6 10 Rainbow.yellow    Rainbow.blue    "v3"
  , textWithin Rainbox.left   3 12 Rainbow.green     Rainbow.red     "v4"
  , textWithin Rainbox.center 4 11 Rainbow.magenta   Rainbow.blue    "v5"
  ]

renderBox1 :: IO ()
renderBox1 = mapM_ Rainbow.putChunk . toList . Rainbox.render $ box1

box2 :: Rainbox.Box Rainbox.Horizontal
box2 = mconcat
  [ textWithin Rainbox.bottom 4 6  Rainbow.magenta Rainbow.green "h1"
  , textWithin Rainbox.top    5 12 Rainbow.blue    Rainbow.yellow "h2"
  ]

renderBox2 :: IO ()
renderBox2 = mapM_ Rainbow.putChunk . toList . Rainbox.render $ box2

box3 :: Rainbox.Box Rainbox.Horizontal
box3 = mconcat
  [ Rainbox.wrap Rainbox.top Rainbow.yellow box1
  , box2
  ]

renderBox3 :: IO ()
renderBox3 = mapM_ Rainbow.putChunk . toList . Rainbox.render $ box3

box4 :: Rainbox.Box Rainbox.Horizontal
box4 = box2 <> Rainbox.spacer Rainbow.cyan 3

renderBox4 :: IO ()
renderBox4 = mapM_ Rainbow.putChunk . toList . Rainbox.render $ box4

box5 :: Rainbox.Box Rainbox.Horizontal
box5 = box4 <> Rainbox.spreader Rainbox.center 12

renderBox5 :: IO ()
renderBox5 = mapM_ Rainbow.putChunk . toList . Rainbox.render $ box5

-- Sample code for 'tableByRows'
--
-- Here is a simple data type representing stations in the
-- Washington DC Metrorail system.

data Line
  = Red
  | Blue
  | Orange
  | Green
  | Yellow
  | Silver
  deriving (Eq, Ord, Show, Enum)

data Station = Station
  { name :: Text
  , metroLines :: [Line]
  , address :: [Text]
  , underground :: Bool
  }

nameCell :: Rainbow.Radiant -> Text -> Rainbox.Cell
nameCell bk nm = Rainbox.Cell cks Rainbox.top Rainbox.center bk
  where
    cks = Seq.singleton . Seq.singleton $ (Rainbow.chunk nm & Rainbow.back bk)

linesCell :: Rainbow.Radiant -> [Line] -> Rainbox.Cell
linesCell bk lns = Rainbox.Cell cks Rainbox.top Rainbox.center bk
  where
    cks = Seq.fromList . fmap (lineRow bk) $ lns

lineRow :: Rainbow.Radiant -> Line -> Seq Rainbow.Chunk
lineRow bk li = Seq.singleton ck
  where
    ck = Rainbow.chunk (X.pack . show $ li) & Rainbow.fore clr & Rainbow.back bk
    clr = case li of
      Red -> Rainbow.red
      Blue -> Rainbow.blue
      Orange -> Rainbow.yellow <> Rainbow.color256 220
      Green -> Rainbow.green
      Yellow -> Rainbow.yellow
      Silver -> Rainbow.white <> Rainbow.grey


addressCell :: Rainbow.Radiant -> [Text] -> Rainbox.Cell
addressCell bk lns = Rainbox.Cell cks Rainbox.top Rainbox.center bk
  where
    cks = Seq.fromList . fmap addrRow $ lns
    addrRow txt = Seq.singleton $ Rainbow.chunk txt & Rainbow.back bk

undergroundCell :: Rainbow.Radiant -> Bool -> Rainbox.Cell
undergroundCell bk bl
  = Rainbox.Cell (Seq.singleton . Seq.singleton $ ck) Rainbox.top Rainbox.left bk
  where
    ck = (Rainbow.chunk $ if bl then "Yes" else "No") & Rainbow.back bk

stations :: [Station]
stations =
  [ Station "Metro Center" [Red, Orange, Silver, Blue]
            ["607 13th St NW", "Washington, DC 20005"] True

  , Station "L'Enfant Plaza" [Orange, Silver, Blue, Green, Yellow]
            ["600 Maryland Ave SW", "Washington, DC 20024"] True

  , Station "Silver Spring" [Red]
            ["8400 Colesville Rd", "Silver Spring, MD 20910"] False

  , Station "Court House" [Silver, Orange]
            ["2100 Wilson Blvd", "Arlington, VA 22201"] True

  , Station "Prince George's Plaza" [Green, Yellow]
            ["3575 East-West Hwy", "Hyattsville, MD 20782"] True
  ]

coloredBack :: Rainbow.Radiant
coloredBack = Rainbow.color256 195

-- | Converts a 'Station' to a list of 'Cell'.

stationRow :: Rainbow.Radiant -> Station -> [Rainbox.Cell]
stationRow bk st =
  [ nameCell bk . name $ st
  , linesCell bk . metroLines $ st
  , addressCell bk . address $ st
  , undergroundCell bk . underground $ st
  ]

-- Building a table of stations on a row-by-row basis.

allStationRows :: [[Rainbox.Cell]]
allStationRows
  = zipWith stationRowWithSpacers (cycle [coloredBack, mempty]) stations
  where
    stationRowWithSpacers bk
      = intersperse (Rainbox.separator bk 1)
      . stationRow bk

verticalStationTable :: Rainbox.Box Rainbox.Vertical
verticalStationTable
  = Rainbox.tableByRows
  . Seq.fromList
  . fmap Seq.fromList
  $ allStationRows

renderVerticalStationTable :: IO ()
renderVerticalStationTable
  = mapM_ Rainbow.putChunk . toList . Rainbox.render $ verticalStationTable

-- Building a table of stations on a column-by-column basis.

stationColumn :: Station -> [Rainbox.Cell]
stationColumn st =
  [ nameCell coloredBack . name $ st
  , linesCell bland . metroLines $ st
  , addressCell coloredBack . address $ st
  , undergroundCell bland . underground $ st
  ]
  where
    bland = mempty

allStationColumns :: [[Rainbox.Cell]]
allStationColumns
  = intersperse spacerColumn
  . map stationColumn
  $ stations
  where
   spacerColumn = take 4 (cycle backgrounds)
    where
      backgrounds =
        [ Rainbox.separator coloredBack 1
        , Rainbox.separator mempty 1
        ]

horizontalStationTable :: Rainbox.Box Rainbox.Horizontal
horizontalStationTable
  = Rainbox.tableByColumns
  . Seq.fromList
  . fmap Seq.fromList
  $ allStationColumns


renderHorizontalStationTable :: IO ()
renderHorizontalStationTable
  = mapM_ Rainbow.putChunk . toList . Rainbox.render $ horizontalStationTable
