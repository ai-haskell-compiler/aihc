{-# LANGUAGE OverloadedStrings #-}
-- | A small utility module that provides a foundation for dynamically enabling and disabling features.
module Control.FeatureFlag where
import Control.Monad
import Data.Text (Text)

-- | A simple toggle for selectively enabling or disabling functionality.
data FeatureToggle a = Enabled | Disabled
  deriving (Read, Show, Eq)

-- | A union of different feature providers which maintains a currently active provider and facilities for changing providers.
--
-- Use this when you don\'t need to disable a feature, just to replace the implementation.
data FeatureProvider a = FeatureProvider
  { enabledProvider :: a
  , enabledProviderName :: Text
  , availableProviders :: [(Text, a)]
  , defaultProvider :: a
  }

-- | Enable a feature.
enable :: FeatureToggle a -> FeatureToggle a
enable = const Enabled

-- | Disable a feature.
disable :: FeatureToggle a -> FeatureToggle a
disable = const Disabled

-- | Flip a toggle from enabled to disabled or vice versa.
toggle :: FeatureToggle a -> FeatureToggle a
toggle t = case t of
  Enabled -> Disabled
  Disabled -> Enabled

-- | Switch on values depending on whether a toggle is enabled or disabled.
withToggle :: FeatureToggle a
  -> b -- return when the toggle is enabled
  -> b -- return when the toggle is disabled
  -> b
withToggle t x y = case t of
  Enabled -> x
  Disabled -> y

-- | Execute an action only when the specified feature is enabled.
whenEnabled :: (Functor m, Monad m) => FeatureToggle a -> m b -> m ()
whenEnabled t m = case t of
  Enabled -> void m
  _ -> return ()

-- | Execute an action only when the specified feature is disabled.
whenDisabled :: (Functor m, Monad m) => FeatureToggle a -> m b -> m ()
whenDisabled t m = case t of
  Disabled -> void m
  _ -> return ()

-- | Replace the current feature provider with another provider.
-- Returns Left if the default provider is used due to a failed lookup.
-- Returns Right if the lookup succeeded.
--
-- Use \"default\" as the lookup value if you want to explicitly load the default provider.
use :: Text -> FeatureProvider a -> Either (FeatureProvider a) (FeatureProvider a)
use name p = if name == "default"
  then Right useDefault
  else case lookup name $ availableProviders p of
    Nothing -> Left useDefault
    Just ep -> Right $ p { enabledProvider = ep, enabledProviderName = name }
  where useDefault = p { enabledProvider = defaultProvider p, enabledProviderName = "default" }

-- | Apply a function that takes a feature provided by a "FeatureProvider".
withProvider :: FeatureProvider a -> (a -> b) -> b
withProvider p f = f $ enabledProvider p

