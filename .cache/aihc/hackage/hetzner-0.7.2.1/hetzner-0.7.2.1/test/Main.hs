
-- | If the environment variable @HETZNER_API_TOKEN@ isn't set, this test succeeds without
--   doing anything. If it is set to a valid Hetzner token, this test will create and destroy
--   resources in Hetzner Cloud, which carries some cost, albeit small.
module Main (main) where

import Hetzner.Cloud qualified as Hetzner
import Control.Monad (void)

main :: IO ()
main = do
  mtoken <- Hetzner.getTokenFromEnv
  case mtoken of
    Nothing -> putStrLn "Environment variable HETZNER_API_TOKEN not provided. Skipping test."
    Just token -> do
      putStrLn "Creating test server..."
      cserver <- Hetzner.createServer token $ Hetzner.defaultNewServer "test"
      mapM_ (Hetzner.waitForAction token) $
        fmap Hetzner.actionID $
          Hetzner.createdServerAction cserver : Hetzner.createdServerNextActions cserver
      let server = Hetzner.createdServer cserver
      putStrLn "Server created:"
      print server
      putStrLn "Deleting test server..."
      void $ Hetzner.deleteServer token (Hetzner.serverID server)
        >>= Hetzner.waitForAction token . Hetzner.actionID
