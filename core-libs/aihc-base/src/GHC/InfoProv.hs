-- | Where a heap object comes from. AIHC does not write a table of the
-- source of each info table, so no object has this information.
module GHC.InfoProv
  ( InfoProv (..),
    InfoProvEnt,
    ipLoc,
    ipeProv,
    peekInfoProv,
    whereFrom,
  )
where

import Foreign.Ptr (Ptr)
import GHC.Internal.ClosureTypes (ClosureType)
import Prelude

-- | The source of an info table.
data InfoProv = InfoProv
  { ipName :: String,
    ipDesc :: ClosureType,
    ipTyDesc :: String,
    ipLabel :: String,
    ipUnitId :: String,
    ipMod :: String,
    ipSrcFile :: String,
    ipSrcSpan :: String
  }
  deriving (Eq, Show)

-- | An entry of the table of info table sources of the GHC runtime system.
data InfoProvEnt

-- | The file and the span of the source of an info table.
ipLoc :: InfoProv -> String
ipLoc provenance = ipSrcFile provenance ++ ":" ++ ipSrcSpan provenance

-- | The 'InfoProv' of an entry of the GHC table. AIHC has no such table.
ipeProv :: Ptr InfoProvEnt -> Ptr InfoProv
ipeProv _ = errorWithoutStackTrace "GHC.InfoProv.ipeProv is not supported by AIHC"

-- | Read an 'InfoProv' from memory. AIHC has no such records.
peekInfoProv :: Ptr InfoProv -> IO InfoProv
peekInfoProv _ = errorWithoutStackTrace "GHC.InfoProv.peekInfoProv is not supported by AIHC"

-- | The source of the info table of a value. AIHC has no such information,
-- so the result is always 'Nothing', as in a GHC program that has no table
-- of info table sources.
whereFrom :: a -> IO (Maybe InfoProv)
whereFrom _ = pure Nothing
