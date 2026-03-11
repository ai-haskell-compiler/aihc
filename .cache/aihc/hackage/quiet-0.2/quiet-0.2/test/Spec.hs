{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}

#if __GLASGOW_HASKELL__ >= 860
{-# LANGUAGE DerivingVia #-}
#endif

import GHC.Generics (Generic)
import Quiet
import System.Exit (exitFailure)

#if __GLASGOW_HASKELL__ >= 860
newtype UserId = UserId { unUserId :: String }
  deriving (Generic)
  deriving (Show) via (Quiet UserId)
#else
newtype UserId = UserId { unUserId :: String }
  deriving (Generic)
instance Show UserId where showsPrec = qshowsPrec
#endif

main :: IO ()
main =
  if show (UserId "simon") == "UserId \"simon\"" then
    pure ()
  else
    exitFailure
