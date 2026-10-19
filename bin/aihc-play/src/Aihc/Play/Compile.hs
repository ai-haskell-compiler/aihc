{-# LANGUAGE OverloadedStrings #-}

-- | Compile one checked program through the shared compiler modules.
module Aihc.Play.Compile (compileProgram) where

import Aihc.Fc.Desugar qualified as Fc
import Aihc.Grin.Cps qualified as Grin
import Aihc.Grin.Gc qualified as Grin
import Aihc.Grin.Lower qualified as Grin
import Aihc.Lir.Lower qualified as Lir
import Aihc.Parser.Syntax (Module)
import Aihc.Play.Json (Json (..))
import Aihc.Play.Session
import Aihc.Tc (TcInterface, mkTcKinds, tcModuleBindings)
import Aihc.Wasm.Lir qualified as Wasm
import Data.Text (Text)
import Data.Text qualified as T

-- | Return assembly for the browser assembler and linker.
compileProgram :: Session -> Text -> Json
compileProgram session source =
  case checkProgram session source >>= compile of
    Left problems -> JObject [("ok", JBool True), ("errors", JArray (map problemJson problems))]
    Right assembly -> JObject [("ok", JBool True), ("errors", JArray []), ("assembly", JArray (map JString assembly))]
  where
    compile (modules, interface) = do
      sources <- mapM (compileModule interface) modules
      entry <- failure "lir" (Lir.lowerEntry Lir.wasip3Target False False)
      entrySource <- failure "wasm" (Wasm.compileLirModule entry)
      pure (sources <> [entrySource])
    compileModule :: TcInterface -> Module -> Either [Problem] Text
    compileModule interface checked = do
      let result = Fc.desugarModuleFc config (tcModuleBindings wiring checked) interface checked
      if Fc.dsSuccess result
        then do
          plain <- either (Left . problem "grin" . T.pack) Right (Grin.lowerProgram prim (Fc.dsProgram result))
          cps <- failure "grin" (Grin.toCpsGrin plain)
          lir <- failure "lir" (Lir.lowerModule Lir.wasip3Target Lir.defaultModuleSettings (Grin.lowerGc cps))
          failure "wasm" (Wasm.compileLirModule lir)
        else Left (map (Problem "fc" Nothing . T.pack) (Fc.dsErrors result))
    prim = sessionPrim session
    wiring = sessionWiring session
    config = Fc.allPublicDesugarConfig (mkTcKinds wiring) prim
    problem stage message = [Problem stage Nothing message]
    failure stage = either (Left . problem stage . T.pack . show) Right
