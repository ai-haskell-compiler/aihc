module Fedora.Krb (
  krbTicket,
  maybeFasIdFromKrb,
  fasIdFromKrb
  )
where

import Control.Monad
import qualified Data.List as L
import Data.Maybe
import SimpleCmd

krbTicket :: IO ()
krbTicket = do
  entry <- klistEntryFedora
  if null entry
    then error' "No krb5 ticket found for FEDORAPROJECT.ORG"
    else
    when (last entry == "(Expired)") $ do
    putStrLn $ unwords entry
    fkinit $ extractFasId entry
    putStrLn ""
  where
    fkinit muser = do
      let opts = maybe [] (\user -> ["-u", user]) muser
      -- FIXME test for fkinit
      ok <- cmdBool "fkinit" opts
      unless ok $ fkinit muser

extractFasId :: [String] -> Maybe String
extractFasId =
  fmap (removeSuffix "@FEDORAPROJECT.ORG") . L.find ("@FEDORAPROJECT.ORG" `L.isSuffixOf`)

maybeFasIdFromKrb :: IO (Maybe String)
maybeFasIdFromKrb = extractFasId <$> klistEntryFedora

fasIdFromKrb :: IO String
fasIdFromKrb = do
  mfasid <- maybeFasIdFromKrb
  case mfasid of
    Nothing -> error' "Could not determine FAS id from klist"
    Just fasid -> return fasid

-- gets first FEDORAPROJECT.ORG entry
klistEntryFedora :: IO [String]
klistEntryFedora = do
  -- FIXME test for klist
  mres <- cmdMaybe "klist" ["-l"]
  return $
    maybe []
    (words . fromMaybe "" . L.find ("@FEDORAPROJECT.ORG" `L.isInfixOf`) . lines)
    mres
