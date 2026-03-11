{-# LANGUAGE CPP #-}
module Util (SrcLoc(..), mapLocations, (</>)) where

import           System.FilePath

#if MIN_VERSION_base(4,8,1) && !MIN_VERSION_base(4,9,0)
import qualified GHC.SrcLoc as GHC
import           Data.CallStack hiding (SrcLoc(..))

data SrcLoc = SrcLoc {
  srcLocPackage :: String
, srcLocModule :: String
, srcLocFile :: String
, srcLocStartLine :: Int
, srcLocStartCol :: Int
, srcLocEndLine :: Int
, srcLocEndCol :: Int
} deriving (Eq, Show)

mapLocations :: CallStack -> [(String, SrcLoc)]
mapLocations = map (fmap mapLocation)
  where
    mapLocation location = SrcLoc {
      srcLocPackage = GHC.srcLocPackage location
    , srcLocModule = GHC.srcLocModule location
    , srcLocFile = GHC.srcLocFile location
    , srcLocStartLine = GHC.srcLocStartLine location
    , srcLocStartCol = GHC.srcLocStartCol location
    , srcLocEndLine = GHC.srcLocEndLine location
    , srcLocEndCol = GHC.srcLocEndCol location
    }
#else
import           Data.CallStack
mapLocations :: CallStack -> CallStack
mapLocations = id
#endif
