-- | This module contains functions for setting environment variables from the 'RichEnv' types as well as functions for transforming between the different types used by this library ('Values', 'Mappings' and 'Prefixes').
module RichEnv.Setters (mappingsToValues, prefixesToValues, valuesToEnv, valuesToEnvList, richEnvToValues) where

import Data.Bifunctor (first)
import Data.HashMap.Strict qualified as HM
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import RichEnv.Types (Environment, RichEnv (..), fromEnvironment)
import RichEnv.Types.Mappings (Mappings (Mappings, unMappings))
import RichEnv.Types.Prefixes (Prefixes (Prefixes, unPrefixes))
import RichEnv.Types.Values (Values (Values, unValues))
import System.Environment (setEnv)

-- | Takes a 'Values' object and sets its contents as environment variables.
valuesToEnv :: Values -> IO ()
valuesToEnv = mapM_ (uncurry setEnv) . fromEnvironment . HM.toList . unValues

-- | Takes a 'Values' object and transforms it into a list of key-value pairs representing environment variables.
--
-- > valuesToEnvList = Data.HashMap.Strict.toList . unValues
valuesToEnvList :: Values -> Environment
valuesToEnvList = HM.toList . unValues

-- | Takes an environment variable list and all the name mappings and prepares a set of environment variables according to the RichEnv rules.
--
-- >>> mappingsToValues [("FOO", "bar"), ("SOME", "thing")] (Mappings $ HM.fromList [("OTHER", "FOO")])
-- Values {unValues = fromList [("OTHER","bar")]}
mappingsToValues :: Environment -> Mappings -> Values
mappingsToValues _ (Mappings m) | null m = mempty
mappingsToValues currentEnv m =
  let mappings' = unMappings m
      value from = lookup from currentEnv
      setMappingValue _ Nothing = id
      setMappingValue k (Just v) = HM.insert k v
      mappingsToValues' k v = setMappingValue k (value v)
   in Values $ HM.foldrWithKey' mappingsToValues' mempty mappings'

-- | Takes an environment variable list and all the prefix mappings and prepares a set of environment variables according to the 'RichEnv' rules.
--
-- >>> prefixesToValues [("FOO", "bar"), ("SOME", "thing")] (Prefixes $ HM.fromList [("OTHER", ["FOO"])])
-- Values {unValues = fromList [("OTHER","bar")]}
prefixesToValues :: Environment -> Prefixes -> Values
prefixesToValues _ (Prefixes p) | null p = mempty
prefixesToValues currentEnv p =
  let prefixes' = unPrefixes p
      prefixesToValues' k v env = env <> setNewPrefix k v currentEnv
      res = if null prefixes' then currentEnv else HM.foldrWithKey' prefixesToValues' mempty prefixes'
   in toValues res

-- | Replace the prefixes of the environment variables with a new prefix.
setNewPrefix ::
  -- | New prefix
  Text ->
  -- | Old prefixes
  [Text] ->
  -- | Current environment list
  Environment ->
  -- | Updated environment list
  Environment
setNewPrefix newPrefix [] currentEnv = fmap (first (newPrefix <>)) currentEnv
setNewPrefix newPrefix [""] currentEnv = fmap (first (newPrefix <>)) currentEnv
setNewPrefix newPrefix oldPrefixes currentEnv =
  let varsWithoutPrefixes = removePrefix currentEnv <$> oldPrefixes
      newPrefixedVars = (fmap . fmap) (first (newPrefix <>)) varsWithoutPrefixes
   in mconcat newPrefixedVars

-- | Remove a prefix from the environment variables.
removePrefix :: Environment -> Text -> Environment
removePrefix currentEnv oldPrefix =
  let getWithoutPrefix old (k, v) = T.stripPrefix old k >>= \sk -> pure (sk, v)
   in mapMaybe (getWithoutPrefix oldPrefix) currentEnv

-- | Create a 'Values' object from an 'Environment'.
toValues :: Environment -> Values
toValues = Values . HM.fromList

-- | Takes an environment variable list and a 'RichEnv' object and generates a 'Values' object.
--
-- >>> richEnvToValues RichEnv.Types.defaultRichEnv [("FOO", "bar"), ("SOME", "thing")]
-- Values {unValues = fromList []}
--
-- >>> import RichEnv.Types.Values as V
-- >>> let richEnvValue = RichEnv.Types.defaultRichEnv { values = V.fromList [("OTHER", "var")]}
-- >>> let envList = [("FOO", "bar"), ("SOME", "thing")]
-- >>> richEnvToValues richEnvValue envList
-- Values {unValues = fromList [("OTHER","var")]}
--
-- >>> import RichEnv.Types.Mappings as M
-- >>> let richEnvValue = RichEnv.Types.defaultRichEnv { mappings = M.fromList [("SOME", "FOO")]}
-- >>> let envList = [("FOO", "bar"), ("SOME", "thing"), ("SOME", "other")]
-- >>> richEnvToValues richEnvValue envList
-- Values {unValues = fromList [("SOME","bar")]}
--
-- >>> import RichEnv.Types.Prefixes as P
-- >>> let richEnvValue = RichEnv.Types.defaultRichEnv { prefixes = P.fromList [("NEW_", ["PREFIXED_"])]}
-- >>> let envList = [("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]
-- >>> richEnvToValues richEnvValue envList
-- Values {unValues = fromList [("NEW_VAR","content"),("NEW_VAR2","content2")]}
richEnvToValues :: RichEnv -> Environment -> Values
richEnvToValues re currentEnv =
  let vvs = values re
      vms = flip mappingsToValues (mappings re)
      vps = flip prefixesToValues (prefixes re)
   in vvs <> vms currentEnv <> vps currentEnv
