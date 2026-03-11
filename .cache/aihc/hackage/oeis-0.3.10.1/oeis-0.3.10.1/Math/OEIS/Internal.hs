-- | This module exists just to facilitate testing.
-- /Nothing here is part of the OEIS API./

module Math.OEIS.Internal where

--------------------------------------------------------------------------------

import Control.Arrow (second, (***))
import Data.Char     (isSpace, toUpper, toLower)
import Data.List     (intercalate, isPrefixOf, foldl')
import Network.URI   (parseURI, URI)

import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Client.TLS   as HTTP
import qualified Network.HTTP.Types        as HTTP
import qualified Data.ByteString.Lazy.UTF8 as U

import Math.OEIS.Types

--------------------------------------------------------------------------------

baseSearchURI :: String
baseSearchURI = "https://oeis.org/search?fmt=text&q="

idSearchURI :: String -> String
idSearchURI n = baseSearchURI ++ "id:" ++ n

seqSearchURI :: SequenceData -> String
seqSearchURI xs = baseSearchURI ++ intercalate "," (map show xs)

getOEIS :: (a -> String) -> a -> IO [OEISSequence]
getOEIS toURI key =
    case parseURI (toURI key) of
      Nothing  -> return []
      Just uri -> do
          mbody <- get uri
          return $ maybe [] parseOEIS mbody

get :: URI -> IO (Maybe String)
get uri = do
    httpman <- HTTP.newManager HTTP.tlsManagerSettings
    let request = HTTP.requestFromURI_ uri
    response <- HTTP.httpLbs request httpman
    return $ case HTTP.statusIsSuccessful (HTTP.responseStatus response) of
        False -> Nothing
        True -> Just (U.toString (HTTP.responseBody response))

readKeyword :: String -> Keyword
readKeyword = read . capitalize

capitalize :: String -> String
capitalize ""     = ""
capitalize (c:cs) = toUpper c : map toLower cs

emptyOEIS :: OEISSequence
emptyOEIS = OEIS [] [] [] "" [] [] [] [] "" 0 0 [] [] [] [] []

addElement :: (Char, String) -> OEISSequence -> OEISSequence
addElement ('I', x) c = c { catalogNums = words x }
addElement (t, x)   c | t `elem` "STU" = c { sequenceData = nums ++ sequenceData c }
    where nums = map read $ csvItems x
addElement (t, x)   c | t `elem` "VWX" = c { signedData = nums ++ signedData c }
    where nums = map read $ csvItems x
addElement ('N', x) c = c { description = x                  }
addElement ('D', x) c = c { references  = x : references c }
addElement ('H', x) c = c { links       = x : links c      }
addElement ('F', x) c = c { formulas    = x : formulas c   }
addElement ('Y', x) c = c { xrefs       = x : xrefs c      }
addElement ('A', x) c = c { author      = x                  }
addElement ('O', x) c = c { offset      = read o
                          , firstGT1    = read f }
  where (o,f) = second tail . span (/=',') $ x
addElement ('p', x) c = c { programs    = (Maple, x) :
                                            programs c     }
addElement ('t', x) c = c { programs    = (Mathematica, x) :
                                            programs c     }
addElement ('o', x) c = c { programs    = (Other, x) :
                                            programs c     }
addElement ('E', x) c = c { extensions  = x : extensions c }
addElement ('e', x) c = c { examples    = x : examples c   }
addElement ('K', x) c = c { keywords    = parseKeywords x    }
addElement ('C', x) c = c { comments    = x : comments c   }
addElement _ c = c

parseOEIS :: String -> [OEISSequence]
parseOEIS x = if "No results." `isPrefixOf` (ls!!3)
                then []
                else go . dropWhile ((/= 'I') . fst) . parseRawOEIS $ ls'
    where ls = lines x
          ls' = init . drop 5 $ ls
          go [] = []
          go (i:xs) = foldl' (flip addElement) emptyOEIS (reverse (i:ys)) : go zs
              where (ys, zs) = break ((== 'I') . fst) xs

parseRawOEIS :: [String] -> [(Char, String)]
parseRawOEIS = map parseItem . combineConts

parseKeywords :: String -> [Keyword]
parseKeywords = map readKeyword . csvItems

csvItems :: String -> [String]
csvItems "" = []
csvItems x = item : others
    where (item, rest) = span (/=',') x
          others = csvItems $ del ',' rest

del :: Char -> String -> String
del _ ""     = ""
del c (x:xs) | c==x      = xs
             | otherwise = x:xs

parseItem :: String -> (Char, String)
parseItem s = (c, str)
    where ( '%':c:_ , rest) = splitWord s
          (_, str )    = if c == 'I' then ("", rest)
                                     else splitWord rest

combineConts :: [String] -> [String]
combineConts (s@('%':_:_) : ss) =
  uncurry (:) . (joinConts s *** combineConts) . break isItem $ ss
combineConts ss = ss

splitWord :: String -> (String, String)
splitWord = second trimLeft . break isSpace

isItem :: String -> Bool
isItem x = not (null x) && '%' == head x

joinConts :: String -> [String] -> String
joinConts s conts = s ++ concatMap trimLeft conts

trimLeft :: String -> String
trimLeft = dropWhile isSpace
