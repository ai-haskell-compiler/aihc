{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Network.Bugsnag
  ( tests,
  )
where

import qualified Data.Aeson
import qualified Data.ByteString
import qualified Network.Bugsnag as Bugsnag
import qualified System.Exit

-- |
-- We don't have a schema to validate our generated JSON against, but we do
-- have a sample report document we can pull from the API website. If we parse
-- that using decoders configured in the same way our encoders are, then we have
-- some guarantee (although not a complete one) that our encoders produce the
-- correct format.
tests :: IO ()
tests = do
  json <- Data.ByteString.readFile "./test/sample-report.json"
  let result = Data.Aeson.eitherDecodeStrict' json
  case result of
    Right (_ :: Bugsnag.Report) -> pure ()
    Left err -> System.Exit.die err
