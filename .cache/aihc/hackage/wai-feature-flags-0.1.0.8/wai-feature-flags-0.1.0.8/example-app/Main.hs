{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.Foldable (asum)
import GHC.Generics (Generic)
import qualified Network.FeatureFlags as FeatureFlags
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  store <- FeatureFlags.memoryStore
  featureFlagsAdmin <- FeatureFlags.mkApplication store
  Warp.run 8080 (application store featureFlagsAdmin)

application :: FeatureFlags.Store Flags -> Wai.Application -> Wai.Application
application store featureFlagsAdmin req respond = do
  flags <- FeatureFlags.fetch store
  case (Wai.requestMethod req, Wai.pathInfo req) of
    -- Mount feature flags page at /admin/flags.
    (_, "admin" : "flags" : rest) ->
      featureFlagsAdmin
        req {Wai.pathInfo = rest}
        respond
    ("GET", ["window"]) ->
      respond $
        Wai.responseLBS
          (toEnum 200)
          []
          (if openWindow flags then "open" else "closed")
    ("GET", ["pigeons"]) ->
      respond $
        Wai.responseLBS
          (toEnum 200)
          []
          (if feedPigeons flags then "fed" else "hungry")
    _ -> respond (Wai.responseLBS (toEnum 404) [] "")

data Flags
  = Flags
      { openWindow :: Bool,
        feedPigeons :: Bool
      }
  deriving (Generic)

instance FeatureFlags.Flags Flags
