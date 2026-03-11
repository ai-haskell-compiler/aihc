{-# LANGUAGE RebindableSyntax #-}

-- | Mathematics does not rigorously define a [space](https://en.wikipedia.org/wiki/Space_(mathematics\)), leaving library devs free to explore.
--
-- “But who can quantify
--  the algebra of space,
--  or weigh those worlds that swim
--  each in its place?
--  Who can outdo the dark?
--  And what computer knows
--  how beauty comes to birth -
--  shell star and rose?
--
-- ~ Technicians by Jean Kenward”
-- ~ John Foster
module NumHask.Space
  ( -- * Usage
    -- $setup

    -- * Space
    -- $space
    module NumHask.Space.Types,

    -- * Instances
    module NumHask.Space.Point,
    module NumHask.Space.Range,
    module NumHask.Space.Rect,
    module NumHask.Space.Time,
    module NumHask.Space.Histogram,
  )
where

import NumHask.Space.Histogram hiding ()
import NumHask.Space.Point hiding ()
import NumHask.Space.Range hiding ()
import NumHask.Space.Rect hiding ()
import NumHask.Space.Time hiding ()
import NumHask.Space.Types hiding ()

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space
-- >>> Point 1 1
-- Point 1 1
--
-- >>> one :: Range Double
-- Range -0.5 0.5
--
-- >>> grid OuterPos (Range 0 50 :: Range Double) 5
-- [0.0,10.0,20.0,30.0,40.0,50.0]

-- $space
--
-- Space is an interesting cross-section of many programming domains.
--
-- - A 'Range' is a 'Space' of numbers.
--
-- - A 'Rect' is a 'Space' of 'Point's. It can also be a 'Space' of 'Rect's (but this is not yet coded up here).
--
-- - A time span is a 'Space' containing moments of time.
--
-- - A 'Histogram' is a divided 'Range' with a count of elements within each division.
