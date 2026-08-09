{-# LANGUAGE OverloadedStrings #-}

-- | Add the executable entry point to a desugared @Main@ module.
module Aihc.Fc.Main
  ( MainEntrypointError (..),
    addMainEntrypoint,
    mainEntryBindingName,
  )
where

import Aihc.Fc.Subst (maximumProgramUnique)
import Aihc.Fc.Syntax
import Aihc.Tc.Types (TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Data.Text (Text)

data MainEntrypointError
  = MainModuleMissing
  | MainBindingMissing
  | MainBindingAmbiguous
  | MainBindingNotIO !TcType
  | MainEntrypointAlreadyPresent
  deriving (Eq, Show)

mainEntryBindingName :: Text
mainEntryBindingName = "$aihc.main"

-- | Add @$aihc.main = GHC.TopHandler.runMainIO Main.main@ directly in
-- System FC. The caller supplies the installed origin of @runMainIO@.
addMainEntrypoint :: FcSymbolOrigin -> FcProgram -> Either MainEntrypointError FcProgram
addMainEntrypoint runMainOrigin program@(FcProgram moduleId topBinds)
  | fcModuleName moduleId /= "Main" = Left MainModuleMissing
  | any ((== mainEntryBindingName) . sourceName) topLevelBinders = Left MainEntrypointAlreadyPresent
  | otherwise =
      case filter ((== "main") . sourceName) topLevelBinders of
        [] -> Left MainBindingMissing
        [mainVar] -> do
          resultType <- maybe (Left (MainBindingNotIO (varType mainVar))) Right (ioResultType (varType mainVar))
          let runMainVar = fcExternalVar runMainOrigin selectedRunMainType
              entryVar = Var mainEntryBindingName (freshTermUnique program) (varType mainVar)
              entryExpression = FcApp (FcTyApp (FcVar runMainVar) resultType) (FcVar mainVar)
          pure (FcProgram moduleId (topBinds <> runMainDeclarations <> [FcTopBind (FcNonRec entryVar entryExpression)]))
        _ -> Left MainBindingAmbiguous
  where
    topLevelBinders = concatMap binders topBinds
    existingRunMainTypes = [ty | FcExternal origin ty <- topBinds, origin == runMainOrigin]

    selectedRunMainType =
      case existingRunMainTypes of
        existingType : _ -> existingType
        [] -> runMainType

    runMainDeclarations =
      case existingRunMainTypes of
        [] -> [FcExternal runMainOrigin selectedRunMainType]
        _ -> []

    sourceName var = maybe (varName var) fcOriginName (varResolvedName var)

binders :: FcTopBind -> [Var]
binders (FcTopBind (FcNonRec var _)) = [var]
binders (FcTopBind (FcRec bindings)) = map fst bindings
binders _ = []

ioResultType :: TcType -> Maybe TcType
ioResultType (TcTyCon (TyCon "IO" 1) [resultType]) = Just resultType
ioResultType _ = Nothing

runMainType :: TcType
runMainType =
  TcForAllTy resultVar (TcFunTy (ioType (TcTyVar resultVar)) (ioType (TcTyVar resultVar)))
  where
    resultVar = TyVarId "a" (Unique (-1))

ioType :: TcType -> TcType
ioType resultType = TcTyCon (TyCon "IO" 1) [resultType]

freshTermUnique :: FcProgram -> Unique
freshTermUnique = Unique . (+ 1) . maximumProgramUnique
