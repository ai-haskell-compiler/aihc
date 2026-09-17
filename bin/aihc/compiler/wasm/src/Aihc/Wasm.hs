-- | The WebAssembly component world of the WASI P3 target. The code
-- generator is "Aihc.Wasm.Lir"; the runtime sources are the @aihc-rts@
-- package under @core-libs@.
module Aihc.Wasm
  ( wasip3WorldPath,
  )
where

import Aihc.DataFiles (getDataFileName)

-- | The directory holding @command.wit@ and its dependencies. The link
-- embeds the component type of this world into the core module, and the
-- C bindings of the runtime are generated from it; see
-- @scripts/update-wit-bindings.sh@.
wasip3WorldPath :: IO FilePath
wasip3WorldPath = getDataFileName "compiler/wasm/runtime/wit"
