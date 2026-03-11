module Singleraeh.Equality where

import Data.Type.Equality

-- | Do one thing if @a@ and @b@ are equal, else do something else.
--
-- Handy for chaining 'testEquality's e.g. when singling type family equation
-- lists.
--
-- Use like so:
--
-- > testEqN sn sm1 _ $ testEqN sn sm2 _ $ ...
--
-- TODO really handy. give a larger, concrete example
testEqElse
    :: forall f a b r. TestEquality f
    => f a -> f b -> (a ~ b => r) -> r -> r
testEqElse sa sb rEq rNEq =
    case testEquality sa sb of
      Just Refl -> gcastWith Refl rEq
      Nothing   -> rNEq
