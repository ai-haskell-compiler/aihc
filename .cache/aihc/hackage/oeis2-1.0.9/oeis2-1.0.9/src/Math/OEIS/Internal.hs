{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Math.OEIS.Internal where

import           Control.Lens        ((^?!), (^?))
import           Control.Monad       (when)
import           Data.Aeson          (decodeStrict)
import           Data.Aeson.Key      (fromText)
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.Maybe          (fromJust, fromMaybe, isNothing)
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
import qualified Data.Text.IO        as T
import qualified Data.Vector         as V
import           Network.HTTP.Simple (getResponseBody, httpBS, parseRequest)
import           System.IO.Unsafe    (unsafePerformIO)

import           Math.OEIS.Types


---------------
-- JSON Keys --
---------------
intKeys = [
  "number", "references", "revision"
  ]
textKeys = [
  "id", "data", "name", "keyword", "offset", "author", "time", "created"
  ]
textsKeys = [
  "comment", "reference", "link", "formula", "example", "maple", "mathematica",
  "program", "xref", "ext"
  ]
keys = intKeys ++ textKeys ++ textsKeys :: Texts


----------------
-- Text utils --
----------------
(+.+) = T.append
(.+)  = T.cons
(+.)  = T.snoc


----------------------
-- Get JSON of OEIS --
----------------------
showSeqData :: SeqData -> T.Text
showSeqData = T.pack . tail . init . show

readSeqData :: String -> SeqData
readSeqData str = case reads ("[" ++ str ++ "]") of
                    [(sd, "")] -> sd
                    _          -> []

baseSearchURI :: T.Text
baseSearchURI = "https://oeis.org/search?fmt=json&q="

addPrefix :: SearchStatus -> T.Text
addPrefix (SubSeq ints) = "seq:" +.+ showSeqData ints
addPrefix ss            = let (cst, txt) = T.breakOn " " $ T.pack $ show ss
                              pref       = T.toLower cst +.+ ":"
                              txt'       = T.init $ T.tail $ T.strip txt
                          in pref +.+ txt'

searchURI :: SearchStatus -> T.Text
searchURI ss = baseSearchURI +.+ addPrefix ss

openURL :: T.Text -> IO T.Text
openURL x = T.decodeUtf8 . getResponseBody <$> (httpBS =<< parseRequest (T.unpack x))

getJSON :: SearchStatus -> Int -> IO T.Text
getJSON (Others txt) _ = return txt -- for test
getJSON ss n           = openURL $ searchURI ss +.+ "&start=" +.+ T.pack (show n)


----------------
-- Parse JSON --
----------------
-- Get all search results --
getResults :: SearchStatus -> Int -> Int -> V.Vector Value -> IO (V.Vector Value)
getResults ss start bound vs = do
  when (bound < 0) $ fail "Upper-bound number of search results mast be non-negative."
  jsn <- getJSON ss start
  let results' = decodeStrict $ T.encodeUtf8 jsn :: Maybe Array
      results = case results' of
        Nothing  -> return []
        Just vs' ->
          let len    = V.length vs'
              start' = start + 10
              diff   = case bound of
                         0 -> len
                         _ -> bound - start
          in case ss of
               ID _     -> return vs'
               Others _ -> return vs'
               _ ->
                if bound /= 0 && diff <= 10 || len /= 10 then
                  return $ vs V.++ V.take diff vs'
                else
                  getResults ss start' bound $ vs V.++ vs'
  results

-- Get nth search result --
getResult :: SearchStatus -> Int -> IO (Maybe Value)
getResult ss n = do
  results <- getResults ss 0 (n + 1) []
  let result = results V.!? n
  return result

-- Get each data in result --
getData :: Value -> T.Text -> (T.Text, Maybe OEISData)
getData result k
  | k `elem` intKeys   = getIntData result k
  | k `elem` textKeys  = getTextData result k
  | k `elem` textsKeys = getTextsData result k
  | otherwise          = (k, Nothing)

getIntData :: Value -> T.Text -> (T.Text, Maybe OEISData)
getIntData result k
  = let d = result ^? key (fromText k) ._Integer
    in case d of
      Nothing -> (k, Nothing)
      _       ->
        case k of
          "number" -> let d'  = T.pack $ show $ fromJust d
                          len = T.length d'
                      in (k, Just $ TXT $ 'A' .+ T.replicate (6 - len) "0" +.+ d')
          _        -> (k, INT <$> d)

getTextData :: Value -> T.Text -> (T.Text, Maybe OEISData)
getTextData result k
  = let d = result ^? key (fromText k) ._String
    in case d of
      Nothing -> (k, Nothing)
      _       ->
        case k of
          "keyword" -> (k, KEYS . map readKeyword . T.splitOn "," <$> d)
          "data"    -> let d' = T.unpack $ '[' .+ fromJust d +. ']'
                       in (k, Just $ SEQ (read d' :: SeqData))
          "id"      -> (k, TXTS . T.splitOn " " <$> d)
          "offset"  -> (k, INT . read . T.unpack . T.take 1 <$> d)
          _         -> (k, TXT <$> d)

getTextsData :: Value -> T.Text -> (T.Text, Maybe OEISData)
getTextsData result k
  = let ds  = result ^? key (fromText k) . _Array
    in case ds of
      Nothing -> (k, Nothing)
      _       ->
        let ts  = (\i -> result ^?! key (fromText k) . nth i . _String) <$> [0..(len - 1)]
            len = fromJust $ V.length <$> ds
        in case k of
          "program" -> let prgs = parsePrograms emptyProgram [] ts
                       in (k, Just $ PRGS prgs)
          _         -> (k, Just $ TXTS ts)


resultLen :: SearchStatus -> IO (Maybe Int)
resultLen ss = do
  jsn <- getJSON ss 0
  return $ fromInteger <$> jsn ^? key "count" . _Integer

emptyOEIS :: OEISSeq
emptyOEIS = OEIS "" [] [] "" [] [] [] [] [] [] [] [] [] [] 0 "" [] 0 0 "" ""

addElement :: OEISSeq -> (T.Text, Maybe OEISData) -> OEISSeq
addElement seq (k, Just (TXT t))
  = case k of
      "number"  -> seq {number = t}
      "name"    -> seq {name = t}
      "author"  -> seq {author = t}
      "time"    -> seq {time = t}
      "created" -> seq {created = t}
      _         -> seq
addElement seq (k, Just (TXTS ts))
  = case k of
      "id"          -> seq {ids = ts}
      "comment"     -> seq {comment = ts}
      "reference"   -> seq {reference = ts}
      "link"        -> seq {link = ts}
      "formula"     -> seq {formula = ts}
      "example"     -> seq {example = ts}
      "maple"       -> seq {maple = ts}
      "mathematica" -> seq {mathematica = ts}
      "xref"        -> seq {xref = ts}
      "ext"         -> seq {ext = ts}
      _             -> seq
addElement seq (k, Just (INT n))
  = case k of
      "offset"     -> seq {offset = n}
      "references" -> seq {references = n}
      "revision"   -> seq {revision = n}
      _            -> seq
addElement seq ("data"   , Just (SEQ s))   = seq {seqData = s}
addElement seq ("keyword", Just (KEYS ks)) = seq {keyword = ks}
addElement seq ("program", Just (PRGS ps)) = seq {program = ps}
addElement seq (_, _) = seq

parseOEIS :: Value -> OEISSeq
parseOEIS result = foldl' addElement emptyOEIS $ map (getData result) keys


-- Parse Keyword --
readKeyword :: T.Text -> Keyword
readKeyword txt =
  let str = T.unpack $ capitalize txt
  in case reads str of
       [(kw, "")] -> kw
       _          -> Other

capitalize :: T.Text -> T.Text
capitalize "" = ""
capitalize cs = toUpper (T.head cs) .+ T.map toLower (T.tail cs)


-- Parse Program --
emptyProgram = ("", []) :: Program

parsePrograms :: Program -> [Program] -> [T.Text] -> [Program]
parsePrograms _ prgs [] = prgs
parsePrograms (lang0, funcs) prgs (t : ts)
  | T.head t == '(' = let prgs' = prgs ++ [(lang, [func])]
                      in parsePrograms (lang, [func]) prgs' ts
  | null prgs       = let prgs' = prgs ++ [("", [t])]
                      in parsePrograms ("", [t]) prgs' ts
  | otherwise       = let funcs' = filter (not . T.null) funcs ++ [t]
                          prgs'  = init prgs ++ [(lang0, funcs')]
                      in parsePrograms (lang0, funcs ++ [t]) prgs' ts
  where
    (lang', func') = T.breakOn ")" t
    lang           = T.tail lang'
    func           = T.strip $ T.tail func'
