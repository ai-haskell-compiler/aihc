{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Control.Timer.Tick
-- Copyright   :  (C) 2018 Francesco Ariis
-- License     :  BSD3 (see LICENSE file)
--
-- Maintainer  :  Francesco Ariis <fa-ml@ariis.it>
-- Stability   :  provisional
-- Portability :  portable
--
-- Timers and timed resources (animations, etc.) utilities for tick-based
-- programs.
--
--------------------------------------------------------------------------------


module Control.Timer.Tick ( -- * Simple timers
                            creaTimer,
                            creaBoolTimer,
                            creaTimerLoop,
                            creaBoolTimerLoop,
                            -- * Timed resources
                            Timed,
                            creaTimedRes,
                            Loop(..),
                            ExpBehaviour(..),
                            -- * Use
                            tick,
                            ticks,
                            reset,
                            lapse,
                            -- * Query
                            isLive,
                            isExpired,
                            fetchFrame,
                            getFrames
                          )


       where
 
import GHC.Generics (Generic)

-----------
-- TYPES --
-----------

-- | A timed resource is a timer which, at any given moment, points to
-- a specific item (like an animation).
--
-- Example:
--
-- @
-- timer = creaTimedRes (Times 1 Elapse) [(2, "a "), (1, "b "), (2, "c ")]
-- test t | isExpired t = putStrLn "Fine."
--        | otherwise   = do putStr (fetchFrame t)
--                           test (tick t)
--
--    -- λ> test timer
--    -- a a b c c Fine.
-- @
--
data Timed a = TimedRes { -- init
                          tSteps    :: [TimerStep a],
                          tLoop     :: Loop,
                          tOrigLoop :: Loop,

                          -- convenience
                          tLoopTicks   :: Integer,
                          tExpireTicks :: Maybe Integer,

                          --  curr
                          tCurrTick :: Integer,
                          tExpired  :: Bool
                        }
        deriving (Show, Eq, Generic)

type TimerStep a = (Integer, a)

-- | Number of times to repeat the animation.
data Loop = -- | Loops forever, never expires.
            AlwaysLoop
            -- | Repeats the cycle for a fixed number of times.
          | Times Integer ExpBehaviour
     deriving (Show, Eq, Generic)

-- | Expire behaviour.
data ExpBehaviour =
        -- | Expires upon __reaching__ last frame.
          Reach
        -- | Expires when last frame is __over__.
        | Elapse
    deriving (Show, Eq, Generic)

-- todo Monoid (or semigroup) <> for timers [2.0]

-- | Mapping on frames.
instance Functor Timed where
    fmap f t = t { tSteps = fmap (\(i, a) -> (i, f a))
                                 (tSteps t) }

------------
-- CREATE --
------------

-- | A simple off/on timer expiring in fixed number of ticks.
--
-- Example:
--
-- @
-- timer = creaTimer Nothing (Just "Over!") 4
-- test t | isExpired t = print (fetchFrame t)
--        | otherwise   = do print (fetchFrame t)
--                           test (tick t)
--
--    -- λ> test timer
--    -- Nothing
--    -- Nothing
--    -- Nothing
--    -- Nothing
--    -- Just \"Over\"!
-- @
creaTimer :: a -> a -> Integer -> Timed a
creaTimer off on i = creaTimedRes (Times 1 Reach) [(i, off), (1, on)]

-- | A looped version of 'creaTimer'.
creaTimerLoop :: a -> a -> Integer -> Timed a
creaTimerLoop off on i = creaTimedRes AlwaysLoop [(i, off), (1, on)]

-- | Shorthand for: @'creaTimer' False True i@.
creaBoolTimer :: Integer -> Timed Bool
creaBoolTimer i = creaTimer False True i

-- | Shorthand for: @'creaTimerLoop' False True i@.
creaBoolTimerLoop :: Integer -> Timed Bool
creaBoolTimerLoop i = creaTimerLoop False True i

-- | Most general way to create a time-based resource (like an animation).
-- 'Loop' controls the expiring behaviour, @[(Integer, a)]@ is a list of
-- frames and their duration.
creaTimedRes :: Loop -> [(Integer, a)] -> Timed a
creaTimedRes _ [] = error "creaTimedRes: cannot create an empty TimedRes."
creaTimedRes _ ss | any ((<1) . fst) ss =
                    error "creaTimedRes: cannot have <1 durations."
creaTimedRes (Times t _) _ | t < 1 =
                    error "creaTimedRes: cannot have non-positive number \
                          \of cycles."
creaTimedRes l ss = TimedRes ss l l
                             loopTicks expTicks
                             0 isExp
    where
          loopTicks = sum . map fst $ ss

          expTicks = case l of
                       AlwaysLoop     -> Nothing
                       Times _ Reach  -> Just $ sum . map fst $ init ss
                       Times _ Elapse -> Just loopTicks

          isExp | length ss == 1 &&
                  isReach l         = True
                | otherwise         = False

          isReach (Times _ Reach) = True
          isReach _               = False

-------------
-- OPERATE --
-------------

-- | Ticks the timer (one step).
tick :: Timed a -> Timed a
tick t | isExpired t = t
       | willExpire  = expire t'
       | willLoop    = loop t'
       | otherwise   = t'
    where
          newTicks = tCurrTick t + 1
          t'       = t { tCurrTick = newTicks }

          willExpire = case tLoop t of
                         Times 1 _ -> Just newTicks == tExpireTicks t
                         _         -> False
          willLoop   = not willExpire &&
                       newTicks == tLoopTicks t

loop :: Timed a -> Timed a
loop tm = case tLoop tm of
            AlwaysLoop -> tm { tCurrTick = 0 }
            -- il check è già dentro a tick
            Times n eb -> tm { tLoop = Times (n-1) eb,
                               tCurrTick = 0 }

expire :: Timed a -> Timed a
expire tm = -- need this as last tick on Elapse is OOB
            if isElB (tLoop tm)
              then expx { tCurrTick = tCurrTick tm - 1 }
              else expx
    where
          expx = case tLoop tm of
                   Times 1 eb  -> tm { tLoop = Times 0 eb,
                                       tExpired = True }
                   _           -> error "non 1 Times in `expire`."

          isElB (Times _ Elapse) = True
          isElB _                = False

-- | Ticks the timer (multiple steps).
ticks :: Integer -> Timed a -> Timed a
ticks 1 t = tick t
ticks n t | n < 1     = error "non-positive number passed to `ticks`."
          | otherwise = ticks (n-1) (tick t)

-- | Ticks the timer until 'isExpired' is @True@.
lapse :: Timed a -> Timed a
lapse t | isExpired t = t
        | otherwise   = lapse (tick t)

-- | Antonym of 'isExpired'.
--
-- > isLive = not isExpired
isLive :: Timed a -> Bool
isLive t = not $ tExpired t

-- | Checks wheter the timer is expired (an expired timer will not
-- respond to 'tick').
isExpired :: Timed a -> Bool
isExpired t = tExpired t

-- | Fetches the current resource of the timer.
fetchFrame :: Timed a -> a
fetchFrame t = bl !! fromIntegral (tCurrTick t)
    where
          bl = concatMap (\(c, a) -> replicate (fromIntegral c) a) $ tSteps t

-- | Return a list of all frames plus their duration.
getFrames :: Timed a -> [(Integer, a)]
getFrames t = tSteps t

-- | Resets the timer to its original state.
reset :: Timed a -> Timed a
reset t = t { tCurrTick = 0,
              tExpired = False,
              tLoop = tOrigLoop t }

