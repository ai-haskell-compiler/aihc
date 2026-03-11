{-# LANGUAGE CPP #-}
module Data.SrcLoc (SrcLoc(..)) where

#if MIN_VERSION_base(4,9,0)
import           GHC.Stack (SrcLoc(..))
#elif MIN_VERSION_base(4,8,1)
import           GHC.SrcLoc (SrcLoc(..))
#else
data SrcLoc = SrcLoc {
  srcLocPackage :: String
, srcLocModule :: String
, srcLocFile :: String
, srcLocStartLine :: Int
, srcLocStartCol :: Int
, srcLocEndLine :: Int
, srcLocEndCol :: Int
} deriving (Eq, Show)
#endif
