module Hint.Conversions (
      typeToString, kindToString, moduleToString, isSucceeded
) where

import qualified Hint.GHC as GHC

import Hint.Base

-- --------- Types / Kinds -----------------------

typeToString :: MonadInterpreter m => GHC.Type -> m String
typeToString t
 = do -- Unqualify necessary types
      -- (i.e., do not expose internals)
      unqual <- runGhc GHC.getPrintUnqual
      withDynFlags $ \df ->
        -- TODO: get unit state from somewhere?
        return $ GHC.showSDocForUser df GHC.emptyUnitState unqual (GHC.pprTypeForUser t)

kindToString :: MonadInterpreter m => GHC.Kind -> m String
kindToString k
 = withDynFlags $ \df ->
     return $ GHC.showSDoc df (GHC.pprTypeForUser k)

-- ---------------- Modules --------------------------

moduleToString :: GHC.Module -> String
moduleToString = GHC.moduleNameString . GHC.moduleName

-- ---------------- Misc -----------------------------

isSucceeded :: GHC.SuccessFlag -> Bool
isSucceeded GHC.Succeeded = True
isSucceeded GHC.Failed    = False
