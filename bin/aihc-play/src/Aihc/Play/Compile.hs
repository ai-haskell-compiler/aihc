{-# LANGUAGE OverloadedStrings #-}

-- | Compile one checked program through the shared compiler modules.
module Aihc.Play.Compile (compileProgram, compileCheckedModule) where

import Aihc.Fc.Desugar qualified as Fc
import Aihc.Grin.Cps qualified as Grin
import Aihc.Grin.Gc qualified as Grin
import Aihc.Grin.Lower qualified as Grin
import Aihc.Lir.Lower qualified as Lir
import Aihc.Parser.Syntax (Extension)
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Play.Json (Json (..))
import Aihc.Play.Session
import Aihc.Tc (TcInterface, mkTcKinds, tcModuleBindings)
import Aihc.Tc.Syntax (Module)
import Aihc.Wasm.Lir qualified as Wasm
import Data.Text (Text)
import Data.Text qualified as T

-- | Return assembly for the browser assembler and linker.
-- | Compile the module of the reader, with the results of its splices, and
-- the entry of the program.
compileProgram :: Session -> Syntax.Module -> [Extension] -> Json
compileProgram session modu extensions =
  case checkProgram session modu extensions >>= compile of
    Left problems -> JObject [("ok", JBool True), ("errors", JArray (map problemJson problems))]
    Right assembly -> JObject [("ok", JBool True), ("errors", JArray []), ("assembly", JArray (map JString assembly))]
  where
    compile (modules, interface) = do
      sources <- mapM (compileCheckedModule Wasm.Static session interface) modules
      entry <- failure "lir" (Lir.lowerEntry Lir.wasip3Target False [])
      entrySource <- failure "wasm" (Wasm.compileLirModule entry)
      pure (sources <> [entrySource])
    failure stage = either (Left . problem stage . T.pack . show) Right
    problem stage message = [Problem stage Nothing message]

-- | Compile one checked module to assembly with a code model.
compileCheckedModule :: Wasm.CodeModel -> Session -> TcInterface -> Module -> Either [Problem] Text
compileCheckedModule model session interface checked = do
  let result = Fc.desugarModuleFc config (tcModuleBindings wiring checked) interface checked
  if Fc.dsSuccess result
    then do
      plain <- either (Left . problem "grin" . T.pack) Right (Grin.lowerProgram prim (Fc.dsProgram result))
      cps <- failure "grin" (Grin.toCpsGrin plain)
      lir <- failure "lir" (Lir.lowerModule Lir.wasip3Target Lir.defaultModuleSettings (Grin.lowerGc cps))
      failure "wasm" (Wasm.compileLirModuleFor model lir)
    else Left (map (Problem "fc" Nothing . T.pack) (Fc.dsErrors result))
  where
    prim = sessionPrim session
    wiring = sessionWiring session
    config = Fc.allPublicDesugarConfig (mkTcKinds wiring) prim
    problem stage message = [Problem stage Nothing message]
    failure stage = either (Left . problem stage . T.pack . show) Right
