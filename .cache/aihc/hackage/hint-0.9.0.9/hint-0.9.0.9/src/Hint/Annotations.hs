module Hint.Annotations (
    getModuleAnnotations,
    getValAnnotations
) where

import Data.Data
import GHC.Serialized

import Hint.Base
import qualified Hint.GHC as GHC

#if MIN_VERSION_ghc(9,2,0)
import GHC (ms_mod)
import GHC.Driver.Env (hsc_mod_graph)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types (hsc_mod_graph, ms_mod)
#else
import HscTypes (hsc_mod_graph, ms_mod)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Annotations
import GHC.Utils.Monad (concatMapM)
#else
import Annotations
import MonadUtils (concatMapM)
#endif

-- Get the annotations associated with a particular module.
getModuleAnnotations :: (Data a, MonadInterpreter m) => a -> String -> m [a]
getModuleAnnotations _ x = do
    mods <- GHC.mgModSummaries . hsc_mod_graph <$> runGhc GHC.getSession
    let x' = filter ((==) x . GHC.moduleNameString . GHC.moduleName . ms_mod) mods
    concatMapM (anns . ModuleTarget . ms_mod) x'

-- Get the annotations associated with a particular function.
getValAnnotations :: (Data a, MonadInterpreter m) => a -> String -> m [a]
getValAnnotations _ s = do
    names <- runGhc $ GHC.parseName s
    concatMapM (anns . NamedTarget) names

anns :: (MonadInterpreter m, Data a) => AnnTarget GHC.Name -> m [a]
anns target = runGhc $ GHC.findGlobalAnns deserializeWithData target
