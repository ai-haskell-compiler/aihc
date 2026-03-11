-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Torrent.Scrape
-- Copyright   :  (c) Lemmih 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- <http://en.wikipedia.org/wiki/Tracker_scrape>
-----------------------------------------------------------------------------

{-# LANGUAGE PatternGuards #-}

module Data.Torrent.Scrape
    ( ScrapeInfo(..)
    , parseScrapeInfo
    , scrapeUrl
    ) where

import Data.Char
import Data.BEncode
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import System.FilePath
import qualified Data.Map as Map

data ScrapeInfo = ScrapeInfo
    { scrapeSeeds    :: Integer
    , scrapeLeechers :: Integer
    } deriving (Read,Show)

parseScrapeInfo :: ByteString -> Maybe ScrapeInfo
parseScrapeInfo bs
    = case bRead bs of
        Just (BDict dict)
            -> do BDict files  <- Map.lookup "files" dict
                  [BDict dict'] <- return (Map.elems files)
                  BInt seeders <- Map.lookup "complete" dict'
                  BInt peers   <- Map.lookup "incomplete" dict'
                  return $ ScrapeInfo
                             { scrapeSeeds = seeders
                             , scrapeLeechers = peers }
        _ -> Nothing

scrapeUrl :: ByteString -> [String] -> Maybe String
scrapeUrl _hash [] = Nothing
scrapeUrl hash (announce:rs)
    = case splitFileName announce of
        (path,file_)
            | (file,ext) <- splitExtension file_
            , ("announce",rest) <- break (=='?') file
            -> let info_hash = urlEncode (BS.unpack hash)
                   file' = "scrape" ++ if null rest
                                       then "?info_hash="++info_hash
                                       else "&info_hash="++info_hash
               in Just (path </> file' <.> ext)
        _ -> scrapeUrl hash rs

urlEncode :: String -> String
urlEncode [] = []
urlEncode s = foldr worker [] s
  where worker c cs =
          if isReservedChar c then let (a, b) = ord c `divMod` 16
                                   in '%' : intToDigit a : intToDigit b : cs
                              else c : cs
        isReservedChar x =
          x < '0' || x > '9' && x < 'A' || x > 'Z'
                             && x < 'a' || x > 'z'


