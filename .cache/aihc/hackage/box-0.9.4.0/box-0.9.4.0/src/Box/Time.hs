-- | Timing effects.
module Box.Time
  ( sleep,
    stampNow,
    stampE,
    Gap,
    gaps,
    fromGaps,
    fromGapsNow,
    gapEffect,
    skip,
    replay,
    gapSkipEffect,
    speedEffect,
    speedSkipEffect,
  )
where

import Box.Connectors
import Box.Emitter
import Control.Applicative
import Control.Concurrent
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Bool
import Data.Fixed (Fixed (MkFixed))
import Data.Time
import Prelude

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Box
-- >>> import Prelude

-- | Sleep for x seconds.
sleep :: Double -> IO ()
sleep x = threadDelay (floor $ x * 1e6)

-- | convenience conversion to Double
fromNominalDiffTime :: NominalDiffTime -> Double
fromNominalDiffTime t = fromInteger i * 1e-12
  where
    (MkFixed i) = nominalDiffTimeToSeconds t

-- | convenience conversion from Double
toNominalDiffTime :: Double -> NominalDiffTime
toNominalDiffTime x =
  let d0 = ModifiedJulianDay 0
      days = floor (x / fromNominalDiffTime nominalDay)
      secs = x - fromIntegral days * fromNominalDiffTime nominalDay
      t0 = UTCTime d0 (picosecondsToDiffTime 0)
      t1 = UTCTime (addDays days d0) (picosecondsToDiffTime $ floor (secs / 1.0e-12))
   in diffUTCTime t1 t0

-- | Add the current time
stampNow :: a -> IO (LocalTime, a)
stampNow a = do
  t <- getCurrentTime
  pure (utcToLocalTime utc t, a)

-- | Add the current time stamp.
--
-- @
-- > toListM . stampE <$|> (qList [1..3])
-- [(2022-08-30 01:55:16.517127,1),(2022-08-30 01:55:16.517132,2),(2022-08-30 01:55:16.517135,3)]
-- @
stampE ::
  Emitter IO a ->
  Emitter IO (LocalTime, a)
stampE = witherE (fmap Just . stampNow)

-- | Usually represents seconds.
type Gap = Double

-- | Convert stamped emitter to gap between emits in seconds
--
-- > toListM <$|> (gaps =<< (fromGapsNow =<< (qList (zip (0:repeat 1) [1..4]))))
-- > [(0.0,1),(1.0,2),(1.0,3),(1.0,4)]
gaps :: Emitter IO (LocalTime, a) -> CoEmitter IO (Gap, a)
gaps e = evalEmitter Nothing $ Emitter $ do
  r <- lift $ emit e
  case r of
    Nothing -> pure Nothing
    Just a' -> do
      t' <- get
      let delta u = maybe 0 (fromNominalDiffTime . diffLocalTime u) t'
      put (Just $ fst a')
      pure $ Just $ first delta a'

-- | Convert gaps in seconds to stamps starting from an initial supplied 'LocalTime'
fromGaps :: LocalTime -> Emitter IO (Gap, a) -> CoEmitter IO (LocalTime, a)
fromGaps t0 e = evalEmitter t0 $ Emitter $ do
  r <- lift $ emit e
  case r of
    Nothing -> pure Nothing
    Just a' -> do
      t' <- get
      let t'' = addLocalTime (toNominalDiffTime (fst a')) t'
      put t''
      pure $ Just (t'', snd a')

-- | Convert gaps in seconds to stamps starting with current time
--
-- > toListM <$|> (fromGapsNow =<< (qList (zip (0:repeat 1) [1..4])))
-- > [(2022-08-30 22:57:33.835228,1),(2022-08-30 22:57:34.835228,2),(2022-08-30 22:57:35.835228,3),(2022-08-30 22:57:36.835228,4)]
fromGapsNow :: Emitter IO (Gap, a) -> CoEmitter IO (LocalTime, a)
fromGapsNow e = do
  t0 <- liftIO getCurrentTime
  fromGaps (utcToLocalTime utc t0) e

-- | Convert a (Gap,a) emitter to an a emitter, with delays between emits of the gap.
gapEffect ::
  Emitter IO (Gap, a) ->
  Emitter IO a
gapEffect as =
  Emitter $ do
    a <- emit as
    case a of
      (Just (s, a')) -> sleep s >> pure (Just a')
      _ -> pure Nothing

-- | Using the Gap emitter, adjust the Gap for a (Gap, a) emitter
speedEffect ::
  Emitter IO Gap ->
  Emitter IO (Gap, a) ->
  Emitter IO a
speedEffect speeds as =
  Emitter $ do
    s <- emit speeds
    a <- emit as
    case (s, a) of
      (Just s', Just (g, a')) -> sleep (g / s') >> pure (Just a')
      _ -> pure Nothing

-- | Only add a Gap effect if greater than the Int emitter
--
-- effect is similar to a fast-forward of the first n emits
gapSkipEffect ::
  Emitter IO Int ->
  Emitter IO Gap ->
  CoEmitter IO Gap
gapSkipEffect n e = evalEmitter 0 $ Emitter $ do
  n' <- lift $ emit n
  e' <- lift $ emit e
  count <- get
  modify (1 +)
  case (n', e') of
    (_, Nothing) -> pure Nothing
    (Nothing, _) -> pure Nothing
    (Just n'', Just e'') ->
      pure $ Just (bool e'' 0 (n'' >= count))

-- | Only add a Gap if greater than the Int emitter
--
-- effect is similar to a fast-forward of the first n emits
speedSkipEffect ::
  Emitter IO (Int, Gap) ->
  Emitter IO (Gap, a) ->
  CoEmitter IO a
speedSkipEffect p e = evalEmitter 0 $ Emitter $ do
  p' <- lift $ emit p
  e' <- lift $ emit e
  count <- get
  modify (1 +)
  case (p', e') of
    (_, Nothing) -> pure Nothing
    (Nothing, _) -> pure Nothing
    (Just (n, s), Just (g, a)) ->
      lift $ sleep (bool (g / s) 0 (n >= count)) >> pure (Just a)

-- | Ignore the first n gaps and immediately emit them.
skip :: Int -> Emitter IO (Gap, a) -> CoEmitter IO (Gap, a)
skip sk e = evalEmitter (sk + 1) $ Emitter $ do
  skip' <- get
  e' <- lift $ emit e
  case e' of
    Nothing -> pure Nothing
    Just (secs, a) -> do
      case skip' of
        0 -> pure (Just (secs, a))
        _ -> do
          put (skip' - 1)
          pure (Just (0, a))

-- | Replay a stamped emitter, adjusting the speed of the replay.
--
-- > toListM . stampE <$|> (replay 0.1 1 =<< (fromGapsNow =<< (qList (zip (0:repeat 1) [1..4]))))
-- > [(2022-08-31 02:29:39.643831,1),(2022-08-31 02:29:39.643841,2),(2022-08-31 02:29:39.746998,3),(2022-08-31 02:29:39.849615,4)]
replay :: Double -> Int -> Emitter IO (LocalTime, a) -> CoEmitter IO a
replay speed sk e = gapEffect . fmap (first (speed *)) <$> (skip sk =<< gaps e)
