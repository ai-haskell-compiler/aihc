{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.IPv6Addr
    ( IPv6Addr (..)
    , maybeIPv6Addr
    , maybePureIPv6Addr
    , maybeFullIPv6Addr
    , sameIPv6Addr

    -- * Conversions
    , toIPv6
    , toHostName
    , toIP6ARPA
    , toUNC

    -- * Utilities
    , getIPv6AddrOf
    , randIPv6Addr
    , randIPv6AddrWithPrefix

    -- * Manipulations
    , IPv6AddrToken (..)
    , randIPv6AddrChunk
    , randPartialIPv6Addr
    , macAddrToIPv6AddrTokens
    , getTokIPv6AddrOf
    , getTokMacAddrOf ) where

import           Control.Applicative  ((<|>))
import           Control.Monad        (guard, replicateM)
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char            (intToDigit, isDigit)
import           Data.IP              (IPv6)

#if !MIN_VERSION_base(4,20,0)
import           Data.List            (elemIndex, elemIndices, foldl', group,
                                       intersperse, isSuffixOf, uncons)
#else
import           Data.List            (elemIndex, elemIndices, group,
                                       intersperse, isSuffixOf, uncons)
#endif


import           Data.Maybe           (fromJust, isJust)

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid          ((<>))
#endif

import qualified Data.Text            as T hiding (foldl')
import qualified Data.Text.Read       as R (decimal)

#if MIN_VERSION_network (2,7,0)
import           Network.Socket       (HostName)
#else
import           Network              (HostName)
#endif

import           Network.Info
import           Numeric              (showHex)
import           System.Random        (randomRIO)

newtype IPv6Addr = IPv6Addr { unIPv6Addr :: T.Text }

instance Show IPv6Addr where
  show (IPv6Addr a) = T.unpack a

instance Eq IPv6Addr where
  (==) (IPv6Addr a) (IPv6Addr b) =
    (unIPv6Addr <$> maybePureIPv6Addr a) == (unIPv6Addr <$> maybePureIPv6Addr b)

instance ToJSON IPv6Addr where
  toJSON (IPv6Addr a) = String a

instance FromJSON IPv6Addr where
  parseJSON (String s) =
    case maybeIPv6Addr s of
      Just a  -> pure a
      Nothing -> fail "Not An IPv6 Address"
  parseJSON _          = fail "JSON String Expected"

data IPv6AddrToken
  = SixteenBit  !T.Text -- ^ A four hexadecimal digits group representing a 16-Bit chunk
  | AllZeros            -- ^ An all zeros 16-Bit chunk
  | Colon               -- ^ A separator between 16-Bit chunks
  | DoubleColon         -- ^ A double-colon stands for a unique compression of many consecutive 16-Bit chunks
  | IPv4Addr    !T.Text -- ^ An embedded IPv4 address as representation of the last 32-Bit
  deriving (Eq, Show)

-- | Returns 'Just' the text representation of a canonized
-- 'IPv6Addr' in conformation with RFC 5952, or 'Nothing'.
--
-- > maybeIPv6Addr "0:0::FFFF:192.0.2.128" == Just (IPv6Addr "::ffff:192.0.2.128")
--
maybeIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeIPv6Addr t = maybeTokIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns 'Just' a pure 'IPv6Addr', or 'Nothing'.
--
-- > maybePureIPv6Addr "::ffff:192.0.2.128" == Just (IPv6Addr "::ffff:c000:280")
--
maybePureIPv6Addr :: T.Text -> Maybe IPv6Addr
maybePureIPv6Addr t = maybeTokPureIPv6Addr t >>= ipv6TokensToIPv6Addr

-- | Returns 'Just' a pure and fully expanded 'IPv6Addr', or 'Nothing'.
--
-- > maybeFullIPv6Addr "::ffff:192.0.2.128" == Just (IPv6Addr "0000:0000:0000:0000:0000:ffff:c000:0280")
--
maybeFullIPv6Addr :: T.Text -> Maybe IPv6Addr
maybeFullIPv6Addr t =
  maybeTokPureIPv6Addr t >>=
    (ipv6TokensToIPv6Addr . expandTokens . fromDoubleColon)

-- | Returns 'True' if arguments are two textual representations of a same IPv6 address.
{-# DEPRECATED sameIPv6Addr "Use the Eq instance." #-}
sameIPv6Addr :: T.Text -> T.Text -> Bool
sameIPv6Addr a b =
  case maybePureIPv6Addr a of
    Nothing -> False
    Just a' ->
      case maybePureIPv6Addr b of
        Nothing -> False
        Just b' -> a' == b'

-- | Returns the reverse lookup domain name corresponding of the given IPv6 address (RFC 3596 Section 2.5).
--
-- > toIP6ARPA (IPv6Addr "4321:0:1:2:3:4:567:89ab") == "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.IP6.ARPA."
--
toIP6ARPA :: IPv6Addr -> T.Text
toIP6ARPA a =
  T.reverse (T.concatMap go $ unIPv6Addr $ fromJust $ maybeFullIPv6Addr $ unIPv6Addr a) <> "IP6.ARPA."
  where
    go ':' = T.empty
    go c   = "." <> T.pack [c]

-- | Returns the Windows UNC path name of the given IPv6 Address.
--
-- > toUNC (IPv6Addr "2001:0DB8:002a:1005:230:48ff:fe73:989d") == "2001-db8-2a-1005-230-48ff-fe73-989d.ipv6-literal.net"
--
toUNC :: IPv6Addr -> T.Text
toUNC a =
  T.concatMap go (unIPv6Addr $ fromJust $ maybePureIPv6Addr $ unIPv6Addr a) <> ".ipv6-literal.net"
  where
    go ':' = "-"
    go c   = T.pack [c]

-- | Given an 'IPv6Addr', returns the corresponding 'HostName'.
toHostName :: IPv6Addr -> HostName
toHostName = show

-- | Given an 'IPv6Addr', returns the corresponding 'Data.IP.IPv6' address.
toIPv6 :: IPv6Addr -> Data.IP.IPv6
toIPv6 = read . show

-- | Returns 'Just' the canonized 'IPv6Addr' of the given local network interface,
-- or 'Nothing'.
--
-- > getIPv6AddrOf "eth0"
--
getIPv6AddrOf :: String -> IO (Maybe IPv6Addr)
getIPv6AddrOf s =
  maybe Nothing (maybeIPv6Addr . T.pack . show) . lookup s <$> networkInterfacesIPv6AddrList

-- | Returns a random 'IPv6Addr'.
randIPv6Addr :: IO IPv6Addr
randIPv6Addr = fromJust <$> randIPv6AddrWithPrefix Nothing

-- | Returns a random 'IPv6Addr', optionally with the given prefix.
--
-- > randIPv6AddrWithPrefix (Just "4321:0:1:2:3:4")
--
randIPv6AddrWithPrefix :: Maybe T.Text -> IO (Maybe IPv6Addr)
randIPv6AddrWithPrefix Nothing = do
  r   <- randomRIO (1,8)
  tks <-
    case r of
      8 -> randPartialIPv6Addr 8
      _ -> do
        r' <- randomRIO (1,8-r)
        case r + r' of
          7 -> concat <$>
            sequence [ randPartialIPv6Addr r
                     , pure [Colon,AllZeros,Colon]
                     , randPartialIPv6Addr r'
                     ]
          8 -> randPartialIPv6Addr 8
          _ -> concat <$>
            sequence [ randPartialIPv6Addr r
                     , pure [DoubleColon]
                     , randPartialIPv6Addr r'
                     ]
  return (ipv6TokensToIPv6Addr tks)
randIPv6AddrWithPrefix (Just p) = do
  let mtks = maybeIPv6AddrTokens p
  guard (isJust mtks)
  let tks = fromJust mtks
  ntks <- do
    let ctks = countChunks tks
    return $
      case (snd ctks :: Int) of
        0 -> 8 - fst ctks
        1 -> 6 - fst ctks
        _ -> 0
  guard (ntks > 0)
  rtks <- randPartialIPv6Addr ntks
  let tks' = addColon tks <> rtks
  guard (isIPv6Addr tks')
  return $ ipv6TokensToIPv6Addr $
    (toDoubleColon . fromDoubleColon) tks'
  where
    countChunks =
      foldr go (0,0)
      where
        go c (a,b) =
          case c of
            SixteenBit _ -> (a+1,b)
            AllZeros     -> (a+1,b)
            DoubleColon  -> (a,b+1)
            _            -> (a,b)
    addColon ts =
      case last ts of
        SixteenBit _ -> ts <> pure Colon
        AllZeros     -> ts <> pure Colon
        _            -> ts


-- ------------------------------------------------------------------------ --
-- Manipulations of IPv6 address chunks                                     --
-- ------------------------------------------------------------------------ --

-- | Returns 'Just' a random 'SixteenBit' token based on a mask \"____\", each
-- underscore being replaced by a random hexadecimal digit.
--
-- > randIPv6AddrChunk "_f__" == Just (SixteenBit "bfd4")
--
randIPv6AddrChunk :: String -> IO IPv6AddrToken
randIPv6AddrChunk m =
  mapM getHex m >>= \h -> pure (SixteenBit $ T.dropWhile (=='0') $ T.pack h)
  where
    getHex c
      | c == '_'  = getDigit
      | otherwise = pure c

-- | Generates a random partial 'IPv6Addr' with n 'SixteenBit'.
randPartialIPv6Addr :: Int -> IO [IPv6AddrToken]
randPartialIPv6Addr n =
  if n > 0 && n < 9
    then
      intersperse Colon <$>
        replicateM n (SixteenBit . T.pack <$> replicateM 4 getDigit)
    else pure []

-- | Given a MAC address, returns 'Just' the corresponding 'IPv6AddrToken' list, or 'Nothing'.
--
-- > macAddrToIPv6AddrTokens "fa:1d:58:cc:95:16" == Just [SixteenBit "fa1d",Colon,SixteenBit "58cc",Colon,SixteenBit "9516"]
--
macAddrToIPv6AddrTokens :: T.Text -> Maybe [IPv6AddrToken]
macAddrToIPv6AddrTokens t =
  case parse macAddr t of
    Done "" b -> intersperse Colon <$> b
    _         -> Nothing

--
-- Functions based upon Network.Info to get local MAC and IPv6 addresses.
--
-- | Given a valid name of a local network interface, returns 'Just' the list of
-- tokens of the interface's IPv6 address, or 'Nothing'.
--
-- > getTokIPv6AddrOf "eth0" == Just [SixteenBit "fe80",DoubleColon,SixteenBit "fa1d",Colon,SixteenBit "58cc",Colon,SixteenBit "9516"]
--
getTokIPv6AddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokIPv6AddrOf s =
  maybe Nothing (maybeTokIPv6Addr . T.pack . show) . lookup s <$> networkInterfacesIPv6AddrList

-- | Given a valid name of a local network interface,
-- returns 'Just' the corresponding list of 'IPv6AddrToken' of the interface's MAC Address,
-- or 'Nothing'.
--
-- > getTokMacAddrOf "eth0" == Just [SixteenBit "fa1d",Colon,SixteenBit "58cc",Colon,SixteenBit "9516"]
--
getTokMacAddrOf :: String -> IO (Maybe [IPv6AddrToken])
getTokMacAddrOf s =
  maybe Nothing (macAddrToIPv6AddrTokens . T.pack . show) . lookup s <$> networkInterfacesMacAddrList
  where
    networkInterfacesMacAddrList = getNetworkInterfaces >>=
      \n -> return (networkInterfacesMac <$> n)
      where networkInterfacesMac (NetworkInterface n _ _ m) = (n,m)

getDigit :: IO Char
getDigit = intToDigit <$> randomRIO (0,15)

-- ------------------------------------------------------------------------- --
-- Internals                                                                 --
-- ------------------------------------------------------------------------- --

-- | Given an arbitrary list of 'IPv6AddrToken', returns the corresponding 'T.Text'.
ipv6TokensToText :: [IPv6AddrToken] -> T.Text
ipv6TokensToText = T.concat . fmap ipv6TokenToText

-- | Returns the corresponding 'T.Text' of an IPv6 address token.
ipv6TokenToText :: IPv6AddrToken -> T.Text
ipv6TokenToText (SixteenBit s) = s
ipv6TokenToText Colon          = ":"
ipv6TokenToText DoubleColon    = "::"
ipv6TokenToText AllZeros       = "0"  -- "A single 16-bit 0000 field MUST be represented as 0" (RFC 5952, 4.1)
ipv6TokenToText (IPv4Addr a)   = a

-- | Returns 'True' if a list of 'IPv6AddrToken' constitutes a valid IPv6 Address.
isIPv6Addr :: [IPv6AddrToken] -> Bool
isIPv6Addr [] = False
isIPv6Addr [DoubleColon] = True
isIPv6Addr [DoubleColon,SixteenBit "1"] = True
isIPv6Addr tks =
  diffNext tks && (do
    let cdctks = countDoubleColon tks
        lentks = length tks
        lasttk = last tks
        lenconst = (lentks == 15 && cdctks == 0) || (lentks < 15 && cdctks == 1)
    firstValidToken tks &&
      (case countIPv4Addr tks :: Int of
         0 -> case lasttk of
                SixteenBit _ -> lenconst
                DoubleColon  -> lenconst
                AllZeros     -> lenconst
                _            -> False
         1 -> case lasttk of
                IPv4Addr _ ->
                  (lentks == 13 && cdctks == 0) || (lentks < 12 && cdctks == 1)
                _          -> False
         _ -> False))
         where
           diffNext [] = False
           diffNext [_] = True
           diffNext (t:ts) = do
             let h = justHead ts
             case t of
               DoubleColon ->
                 case h of
                   Colon -> False
                   _     -> True
               SixteenBit _ ->
                 case h of
                   SixteenBit _ -> False
                   AllZeros     -> False
                   _            -> diffNext ts
               AllZeros     ->
                 case h of
                   SixteenBit _ -> False
                   AllZeros     -> False
                   _            -> diffNext ts
               _            -> diffNext ts
           firstValidToken l =
             case justHead l of
               SixteenBit _ -> True
               DoubleColon  -> True
               AllZeros     -> True
               _            -> False
           countDoubleColon = length . elemIndices DoubleColon

countIPv4Addr :: [IPv6AddrToken] -> Int
countIPv4Addr =
  foldr oneMoreIPv4Addr 0
  where
    oneMoreIPv4Addr t c =
      case t of
        IPv4Addr _ -> c + 1
        _          -> c

-- | This is the main function which returns 'Just' the list of a tokenized IPv6
-- address text representation validated against RFC 4291 and canonized
-- in conformation with RFC 5952, or 'Nothing'.
maybeTokIPv6Addr :: T.Text -> Maybe [IPv6AddrToken]
maybeTokIPv6Addr t = do
  tks <- maybeIPv6AddrTokens t
  guard (isIPv6Addr tks)
  return (ipv4AddrReplacement . toDoubleColon . fromDoubleColon $ tks)
  where
    ipv4AddrReplacement tks =
      if ipv4AddrRewrite tks
        then init tks <> ipv4AddrToIPv6AddrTokens (last tks)
        else tks

-- | Returns 'Just' the list of tokenized pure IPv6 address, always rewriting an
-- embedded IPv4 address if present.
maybeTokPureIPv6Addr :: T.Text -> Maybe [IPv6AddrToken]
maybeTokPureIPv6Addr t = do
  tks <- maybeIPv6AddrTokens t
  guard (isIPv6Addr tks)
  return (toDoubleColon . ipv4AddrReplacement . fromDoubleColon $ tks)
  where
    ipv4AddrReplacement tks' =
      init tks' <> ipv4AddrToIPv6AddrTokens (last tks')

-- | Tokenize a 'T.Text' into 'Just' a list of 'IPv6AddrToken', or 'Nothing'.
maybeIPv6AddrTokens :: T.Text -> Maybe [IPv6AddrToken]
maybeIPv6AddrTokens t =
  case readText t of
    Done "" l -> Just l
    _         -> Nothing
  where
    readText t' =
      feed
        (parse (many1 $ ipv4Addr <|> sixteenBit <|> doubleColon <|> colon) t')
        T.empty

-- | An embedded IPv4 address have to be rewritten to output a pure IPv6 Address
-- text representation in hexadecimal digits. But some well-known prefixed IPv6
-- addresses have to keep visible in their text representation the fact that
-- they deals with IPv4 to IPv6 transition process (RFC 5952 Section 5):
--
-- IPv4-compatible IPv6 address like "::1.2.3.4"
--
-- IPv4-mapped IPv6 address like "::ffff:1.2.3.4"
--
-- IPv4-translated address like "::ffff:0:1.2.3.4"
--
-- IPv4-translatable address like "64:ff9b::1.2.3.4"
--
-- ISATAP address like "fe80::5efe:1.2.3.4"
--
ipv4AddrRewrite :: [IPv6AddrToken] -> Bool
ipv4AddrRewrite tks =
  case last tks of
    IPv4Addr _ -> do
      let itks = init tks
      not  (itks == [DoubleColon]
         || itks == [DoubleColon,SixteenBit tokffff,Colon]
         || itks == [DoubleColon,SixteenBit tokffff,Colon,AllZeros,Colon]
         || itks == [SixteenBit "64",Colon,SixteenBit "ff9b",DoubleColon]
         || [SixteenBit "200",Colon,SixteenBit tok5efe,Colon] `isSuffixOf` itks
         || [AllZeros,Colon,SixteenBit tok5efe,Colon] `isSuffixOf` itks
         || [DoubleColon,SixteenBit tok5efe,Colon] `isSuffixOf` itks)
    _          -> False
  where
    tokffff = "ffff"
    tok5efe = "5efe"

-- | Rewrites an embedded 'IPv4Addr' into the corresponding list of pure 'IPv6Addr' tokens.
--
-- > ipv4AddrToIPv6AddrTokens (IPv4Addr "127.0.0.1") == [SixteenBits "7f0",Colon,SixteenBits "1"]
--
ipv4AddrToIPv6AddrTokens :: IPv6AddrToken -> [IPv6AddrToken]
ipv4AddrToIPv6AddrTokens t =
  case t of
    IPv4Addr a -> do
      let m = toHex a
      [  SixteenBit ((!!) m 0 <> addZero ((!!) m 1))
       , Colon
       , SixteenBit ((!!) m 2 <> addZero ((!!) m 3)) ]
    _          -> [t]
    where
      toHex a = (\x -> T.pack $ showHex (read (T.unpack x)::Int) "") <$> T.split (=='.') a
      addZero d = if T.length d == 1 then "0" <> d else d

expandTokens :: [IPv6AddrToken] -> [IPv6AddrToken]
expandTokens =
  map expandToken
  where
    expandToken (SixteenBit s) = SixteenBit (T.justifyRight 4 '0' s)
    expandToken AllZeros       = SixteenBit "0000"
    expandToken t              = t

fromDoubleColon :: [IPv6AddrToken] -> [IPv6AddrToken]
fromDoubleColon tks =
  if DoubleColon `notElem` tks
    then tks
    else do
      let s = splitAt (fromJust $ elemIndex DoubleColon tks) tks
          fsts = fst s
          snds = if not (null (snd s)) then justTail(snd s) else []
          fste = if null fsts then [] else fsts <> [Colon]
          snde = if null snds then [] else Colon : snds
      fste <> allZerosTokensReplacement(quantityOfAllZerosTokenToReplace tks) <> snde
      where
        allZerosTokensReplacement x = intersperse Colon (replicate x AllZeros)
        quantityOfAllZerosTokenToReplace y =
          ntks tks - foldl' (\c d -> if (d /= DoubleColon) && (d /= Colon) then c+1 else c) 0 y
          where
            ntks tks' = if countIPv4Addr tks' == 1 then 7 else 8

toDoubleColon :: [IPv6AddrToken] -> [IPv6AddrToken]
toDoubleColon tks =
  zerosToDoubleColon tks (zerosRunToReplace $ zerosRunsList tks)
  where
    -- No all zeros token, so no double colon replacement...
    zerosToDoubleColon ls (_,0) = ls
    -- "The symbol '::' MUST NOT be used to shorten just one 16-bit 0 field" (RFC 5952 4.2.2)
    zerosToDoubleColon ls (_,1) = ls
    zerosToDoubleColon ls (i,l) =
      let ls' = filter (/= Colon) ls
      in intersperse Colon (Prelude.take i ls') <> [DoubleColon] <> intersperse Colon (drop (i+l) ls')
    zerosRunToReplace t =
      let l = longestLengthZerosRun t
      in (firstLongestZerosRunIndex t l,l)
      where
        firstLongestZerosRunIndex x y = sum . map snd $ Prelude.takeWhile (/=(True,y)) x
        longestLengthZerosRun x =
          maximum (longest <$> x)
          where
            longest l' =
              case l' of
                (True,i) -> i
                _        -> 0
    zerosRunsList x =
      helper <$> groupZerosRuns x
      where
        helper h = (justHead h == AllZeros, lh) where lh = length h
        groupZerosRuns = group . filter (/= Colon)

ipv6TokensToIPv6Addr :: [IPv6AddrToken] -> Maybe IPv6Addr
ipv6TokensToIPv6Addr = Just . IPv6Addr . ipv6TokensToText

networkInterfacesIPv6AddrList :: IO [(String,Network.Info.IPv6)]
networkInterfacesIPv6AddrList =
  fmap networkInterfacesIPv6Addr <$> getNetworkInterfaces
  where
    networkInterfacesIPv6Addr (NetworkInterface n _ a _) = (n,a)

macAddr :: Parser (Maybe [IPv6AddrToken])
macAddr = do
  n1 <- count 2 hexaChar <* ":"
  n2 <- count 2 hexaChar <* ":"
  n3 <- count 2 hexaChar <* ":"
  n4 <- count 2 hexaChar <* ":"
  n5 <- count 2 hexaChar <* ":"
  n6 <- count 2 hexaChar
  return $ maybeIPv6AddrTokens $ T.pack $ concat [n1,n2,n3,n4,n5,n6]

sixteenBit :: Parser IPv6AddrToken
sixteenBit = do
  r <- ipv6AddrFullChunk <|> count 3 hexaChar <|> count 2 hexaChar <|> count 1 hexaChar
  -- "Leading zeros MUST be suppressed" (RFC 5952, 4.1)
  let r' = T.dropWhile (=='0') (T.pack r)
  return $
    if T.null r'
      then AllZeros
      -- Hexadecimal digits MUST be in lowercase (RFC 5952 4.3)
      else SixteenBit (T.toLower r')

ipv4Addr :: Parser IPv6AddrToken
ipv4Addr = do
  n1 <- manyDigits <* "."
  guard (n1 /= T.empty)
  n2 <- manyDigits <* "."
  guard (n2 /= T.empty)
  n3 <- manyDigits <* "."
  guard (n3 /= T.empty)
  n4 <- manyDigits
  guard (n4 /= T.empty)
  return (IPv4Addr $ T.intercalate "." [n1,n2,n3,n4])
  where
    manyDigits = takeWhile1 isDigit >>= \ds ->
      return $
        case R.decimal ds :: Either String (Integer, T.Text) of
          Right (n,_) ->
            if n < 256
              then T.pack (show n)
              else T.empty
          Left  _     -> T.empty

doubleColon :: Parser IPv6AddrToken
doubleColon = do
  _ <- string "::"
  return DoubleColon

colon :: Parser IPv6AddrToken
colon = do
  _ <- string ":"
  return Colon

ipv6AddrFullChunk :: Parser String
ipv6AddrFullChunk = count 4 hexaChar

hexaChar :: Parser Char
hexaChar = satisfy (inClass "0-9a-fA-F")

-- Helper functions
justHead :: [a] -> a
justHead l = fst (fromJust $ uncons l)

justTail :: [a] -> [a]
justTail l = snd (fromJust $ uncons l)

