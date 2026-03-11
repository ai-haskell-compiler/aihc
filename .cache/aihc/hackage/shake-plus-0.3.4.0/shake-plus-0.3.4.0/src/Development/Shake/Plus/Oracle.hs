{- |
   Module     : Development.Shake.Plus.Oracle
   License    : MIT
   Stability  : experimental

Oracle utilities in "Development.Shake" lifted to `MonadAction` and `MonadRules`.
-}
module Development.Shake.Plus.Oracle (
  addOracle
, addOracleCache
, addOracleHash
, askOracle
, askOracles
, Development.Shake.RuleResult
) where

import           Control.Exception.Extra
import qualified Development.Shake
import           Development.Shake.Plus.Core
import           RIO

-- | Lifted version of `Development.Shake.addOracle` using `RAction` runner.
addOracle :: (MonadRules m, MonadReader r m, RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial)
          => (q -> RAction r a)
          -> m (q -> RAction r a)
addOracle ract = ask >>= \r -> liftRules $ (liftAction . ) <$> Development.Shake.addOracle (runRAction r . ract)

-- | Lifted version of `Development.Shake.addOracleCache` using `RAction` runner.
addOracleCache :: (MonadRules m, MonadReader r m, RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial)
               => (q -> RAction r a)
               -> m (q -> RAction r a)
addOracleCache ract = ask >>= \r -> liftRules $ (liftAction . ) <$> Development.Shake.addOracleCache (runRAction r . ract)

-- | Lifted version of `Development.Shake.addOracleHash` using `RAction` runner.
addOracleHash :: (MonadRules m, MonadReader r m, RuleResult q ~ a, ShakeValue q, ShakeValue a, Partial)
               => (q -> RAction r a)
               -> m (q -> RAction r a)
addOracleHash ract = ask >>= \r -> liftRules $ (liftAction . ) <$> Development.Shake.addOracleHash (runRAction r . ract)

-- | Lifted version of `Development.Shake.askOracle`.
askOracle :: (MonadAction m, RuleResult q ~ a, ShakeValue q, ShakeValue a) => q -> m a
askOracle = liftAction . Development.Shake.askOracle

-- | Lifted version of `Development.Shake.askOracles`.
askOracles :: (MonadAction m, RuleResult q ~ a, ShakeValue q, ShakeValue a) => [q]-> m [a]
askOracles = liftAction . Development.Shake.askOracles

