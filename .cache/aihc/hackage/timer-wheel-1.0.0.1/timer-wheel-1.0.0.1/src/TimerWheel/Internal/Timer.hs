module TimerWheel.Internal.Timer
  ( Timer (..),
    cancel,
  )
where

import TimerWheel.Internal.Prelude

-- | A registered timer, parameterized by the result of attempting to cancel it:
--
--     * A one-shot timer may only be canceled if it has not already fired.
--     * A recurring timer can always be canceled.
--
-- __API summary__
--
-- +-------------+----------+
-- | Create      | Modify   |
-- +=============+==========+
-- | 'register'  | 'cancel' |
-- +-------------+----------+
-- | 'recurring' |          |
-- +-------------+----------+
newtype Timer a
  = Timer (IO a)

-- | Cancel a timer.
cancel :: Timer a -> IO a
cancel =
  coerce
