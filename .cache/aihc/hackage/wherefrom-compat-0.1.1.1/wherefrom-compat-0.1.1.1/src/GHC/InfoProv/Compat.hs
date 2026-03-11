{-# LANGUAGE CPP #-}
-- | This module provides a stable interface to access GHC's info provenance information.
-- This is helpful for seeing metadata about heap objects.
module GHC.InfoProv.Compat
  ( InfoProv(..)
  , whereFrom
  ) where

#if MIN_VERSION_base(4,20,0)
import qualified GHC.InfoProv as IP

data InfoProv = InfoProv {
  ipName :: String,
  ipDesc :: String,
  ipTyDesc :: String,
  ipLabel :: String,
  ipMod :: String,
  ipSrcFile :: String,
  ipSrcSpan :: String
} deriving (Eq, Show)


-- | Get information about where a value originated from.
--
-- This information is stored statically in a binary when @super-duper@ is enabled.
-- The source positions will be greatly improved by also enabled debug information with @-g3@.
-- Finally you can enable @-fdistinct-constructor-tables@ to get more precise information about data constructor allocations.
--
-- The information is collect by looking at the info table address of a specific closure and then consulting a specially generated map (by @-finfo-table-map@)
-- to find out where we think the best source position to describe that info table arose from.
whereFrom :: a -> IO (Maybe InfoProv)
whereFrom v = do
  xs <- IP.whereFrom v
  pure $ do
    ip <- xs
    Just $
      InfoProv
       { ipName = IP.ipName ip
       , ipDesc = show . fromEnum $ IP.ipDesc ip
       , ipTyDesc = IP.ipTyDesc ip
       , ipLabel = IP.ipLabel ip
       , ipMod = IP.ipMod ip
       , ipSrcFile = IP.ipSrcFile ip
       , ipSrcSpan = IP.ipSrcSpan ip
       }

#elif MIN_VERSION_base(4,18,0)
import GHC.InfoProv (InfoProv(..), whereFrom)
#elif MIN_VERSION_base(4,17,0)
import qualified GHC.Stack.CCS as CCS

data InfoProv = InfoProv {
  ipName :: String,
  ipDesc :: String,
  ipTyDesc :: String,
  ipLabel :: String,
  ipMod :: String,
  ipSrcFile :: String,
  ipSrcSpan :: String
} deriving (Eq, Show)


-- | Get information about where a value originated from.
--
-- This information is stored statically in a binary when @super-duper@ is enabled.
-- The source positions will be greatly improved by also enabled debug information with @-g3@.
-- Finally you can enable @-fdistinct-constructor-tables@ to get more precise information about data constructor allocations.
--
-- The information is collect by looking at the info table address of a specific closure and then consulting a specially generated map (by @-finfo-table-map@)
-- to find out where we think the best source position to describe that info table arose from.
whereFrom :: a -> IO (Maybe InfoProv)
whereFrom v = do
  xs <- CCS.whereFrom v
  pure $ do
    ip <- xs
    -- srcFileSpan has the format: `test/Spec.hs:12:1-11`
    (srcFile, _:srcSpan) <- Just . break (== ':') $ CCS.ipLoc ip
    Just $
      InfoProv
       { ipName = CCS.ipName ip
       , ipDesc = CCS.ipDesc ip
       , ipTyDesc = CCS.ipTyDesc ip
       , ipLabel = CCS.ipLabel ip
       , ipMod = CCS.ipMod ip
       -- the following fields are not available in this version of base
       , ipSrcFile = srcFile
       , ipSrcSpan = srcSpan
       }
#else
import qualified GHC.Stack.CCS as CCS

data InfoProv = InfoProv {
  ipName :: String,
  ipDesc :: String,
  ipTyDesc :: String,
  ipLabel :: String,
  ipMod :: String,
  ipSrcFile :: String,
  ipSrcSpan :: String
} deriving (Eq, Show)


-- | Get information about where a value originated from.
--
-- This information is stored statically in a binary when @super-duper@ is enabled.
-- The source positions will be greatly improved by also enabled debug information with @-g3@.
-- Finally you can enable @-fdistinct-constructor-tables@ to get more precise information about data constructor allocations.
--
-- The information is collect by looking at the info table address of a specific closure and then consulting a specially generated map (by @-finfo-table-map@)
-- to find out where we think the best source position to describe that info table arose from.
whereFrom :: a -> IO (Maybe InfoProv)
whereFrom v = do
  xs <- CCS.whereFrom v
  pure $ do
    [name, desc, tyDesc, label, modName, srcFileSpan] <- Just xs
    -- srcFileSpan has the format: `test/Spec.hs:12:1-11`
    (srcFile, _:srcSpan) <- Just $ break (== ':') srcFileSpan
    Just $
      InfoProv
       { ipName = name
       , ipDesc = desc
       , ipTyDesc = tyDesc
       , ipLabel = label
       , ipMod = modName
       , ipSrcFile = srcFile
       , ipSrcSpan = srcSpan
       }
#endif
