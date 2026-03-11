{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | 'Parser' for a Netscape/Mozilla cookie jar

Provides:

* parsing functions that parse the Netscape/Mozilla cookie jar file format

* @'Builder's@ that provide an incomplete roundtrip with the
  parser.

    * __incomplete__ because some of the fields in @Cookie@ are not
      saved in the Netscape/Mozilla cookie jar; see `cookieBuilder`.

* combinators to ease use with "Network.Http.Client", like 'usingCookiesFromFile'', e.g,

  > httpWithCookies :: Manager -> FilePath -> Request -> IO (Response a)
  > httpWithCookies manager cookieJarPath req = do
  >   let httpLbs' = usingCookiesFromFile' cookiePath $ flip httpLbs manager
  >   httpLbs' req
-}
module Web.Cookie.Jar
  ( -- * read and write files
    writeJar
  , writeJar'
  , writeNetscapeJar
  , readJar
  , readJarX
  , BadJarFile (..)

    -- * use with http-client
  , addCookiesFromFile
  , saveCookies
  , usingCookiesFromFile
  , usingCookiesFromFile'

    -- * Cookie jar format

    -- ** parsing
  , cookieJarParser
  , cookieParser
  , parseCookieJar

    -- ** printing
  , netscapeJarBuilder
  , jarBuilder
  , jarBuilder'
  , cookieBuilder

    -- * re-exports
  , parseOnly
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
  ( Parser
  , char
  , decimal
  , endOfLine
  , isEndOfLine
  , many'
  , parseOnly
  , skipSpace
  , skipWhile
  , takeWhile1
  , try
  )
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder
  ( Builder
  , byteString
  , char7
  , integerDec
  , toLazyByteString
  )
import qualified Data.ByteString.Lazy as L
import Data.Char (ord)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX
  ( posixSecondsToUTCTime
  , utcTimeToPOSIXSeconds
  )
import Network.HTTP.Client
  ( Cookie (..)
  , CookieJar
  , Request
  , Response
  , createCookieJar
  , destroyCookieJar
  , insertCookiesIntoRequest
  , updateCookieJar
  )
import System.Directory (doesFileExist)


-- | Perform a HTTP request first loading then saving any matching cookies from a cookie file
usingCookiesFromFile :: FilePath -> Request -> (Request -> IO (Response b)) -> IO (Response b)
usingCookiesFromFile jarPath req doReq = do
  req' <- addCookiesFromFile jarPath req
  resp <- doReq req'
  saveCookies jarPath resp req'


-- | 'usingCookiesFromFile' with arguments re-ordered
usingCookiesFromFile' :: FilePath -> (Request -> IO (Response b)) -> Request -> IO (Response b)
usingCookiesFromFile' p = flip (usingCookiesFromFile p)


{- | Add any appropriate Cookies from a cookie file to a @Request@

 - if the file is absent, no update occurs
 - throws @BadJarFile@ if the file can't be parsed
-}
addCookiesFromFile
  :: FilePath
  -- ^ path to the cookie file
  -> Request
  -> IO Request
addCookiesFromFile dataPath req = do
  pathExists <- doesFileExist dataPath
  if not pathExists
    then pure req
    else do
      now <- getCurrentTime
      readJarX dataPath >>= \jar -> do
        let (req', _jar') = insertCookiesIntoRequest req jar now
        pure req'


{- | Update the cookie file with any cookies in the response

When the file does not exist, it's created as long as the parent directory
exists and permits the file to be written

The output is saved to the cookie file using 'writeJar'

throws an exception if:

 - cannot write due to permissions or parent directory not existing
 - the file exists, but cannot be parsed into Cookies
-}
saveCookies :: FilePath -> Response a -> Request -> IO (Response a)
saveCookies dataPath resp req = do
  pathExists <- doesFileExist dataPath
  old <- if pathExists then readJarX dataPath else pure (createCookieJar [])
  now <- getCurrentTime
  let (updated, resp_) = updateCookieJar resp req now old
  writeJar dataPath updated
  pure resp_


-- | Reasons a jar file could not be loaded
data BadJarFile = InvalidJar
  deriving (Eq, Show)


instance Exception BadJarFile


-- | Parse a @ByteString@ containing a cookie jar in the Netscape/Mozilla format
parseCookieJar :: ByteString -> Either String CookieJar
parseCookieJar = parseOnly cookieJarParser


-- | @Parser@ for a cookie jar in the Netscape/Mozilla format
cookieJarParser :: Parser CookieJar
cookieJarParser = createCookieJar <$> many' cookieParser


{- | Parser for one cookie/line in a cookie jar in the Netscape/Mozilla format
This will also consume any comment lines preceding the cookie line.

This parser recognizes the magic prefix @#HttpOnly_# and sets the appropriate
field in the @Cookie@ datatype
-}
cookieParser :: Parser Cookie
cookieParser =
  let
    httpOnlyLine = try $ "#HttpOnly_" *> cookieParser' True
    commentLine = "#" *> skipWhile notEndOfLine *> endOfLine *> cookieParser
    cookieLine = cookieParser' False
   in
    skipSpace *> (httpOnlyLine <|> commentLine <|> cookieLine)


-- | Basic parser for a line containing a cookie in the Netscape/Mozilla format
cookieParser' :: Bool -> Parser Cookie
cookieParser' cookie_http_only = do
  let
    epoch = posixSecondsToUTCTime 0
    -- component parsers
    tab = void $ char '\t'
    parseString = takeWhile1 (/= '\t')
    parseBool = True <$ "TRUE" <|> False <$ "FALSE"
    parseTime = posixSecondsToUTCTime . fromInteger <$> decimal
    parseValue = takeWhile1 notEndOfLine
  cookie_domain <- parseString
  tab
  cookie_host_only <- parseBool
  tab
  cookie_path <- parseString
  tab
  cookie_secure_only <- parseBool
  tab
  cookie_expiry_time <- parseTime
  tab
  cookie_name <- parseString
  tab
  cookie_value <- parseValue
  endOfLine <|> pure ()
  pure $
    Cookie
      { cookie_domain
      , cookie_path
      , cookie_secure_only
      , cookie_expiry_time
      , cookie_name
      , cookie_value
      , cookie_host_only
      , cookie_http_only
      , -- fields not represented by the cookie jar format
        cookie_creation_time = epoch
      , cookie_last_access_time = epoch
      , cookie_persistent = True
      }


notEndOfLine :: Char -> Bool
notEndOfLine = not . isEndOfLine . fromIntegral . ord


-- | Like 'jarBuilder' but outputs the Netscape header before the cookie lines
netscapeJarBuilder :: CookieJar -> Builder
netscapeJarBuilder = jarBuilder' netscapeHeader


netscapeHeader :: Builder
netscapeHeader = "# Netscape HTTP Cookie File\n"


-- | Print a cookie jar in the Netscape/Mozilla format, with no header
jarBuilder :: CookieJar -> Builder
jarBuilder = foldMap ((<> "\n") . cookieBuilder) . destroyCookieJar


-- | Like 'jarBuilder' but outputs a header before the cookie lines
jarBuilder' :: Builder -> CookieJar -> Builder
jarBuilder' header = (header <>) . jarBuilder


-- | Writes a cookie jar to the given path in the Netscape/Mozilla format, with no header
writeJar :: FilePath -> CookieJar -> IO ()
writeJar fp = L.writeFile fp . toLazyByteString . jarBuilder


-- | Like 'writeJar', but outputs a header before the cookie lines
writeJar' :: Builder -> FilePath -> CookieJar -> IO ()
writeJar' header fp =
  L.writeFile fp
    . toLazyByteString
    . jarBuilder'
      header


-- | Like 'writeJar', but outputs the Netscape header before the cookie lines
writeNetscapeJar :: FilePath -> CookieJar -> IO ()
writeNetscapeJar = writeJar' netscapeHeader


-- | Read a Cookie Jar from a file.
readJar :: FilePath -> IO (Either String CookieJar)
readJar = fmap parseCookieJar . BS.readFile


-- | Like 'readJar', but throws @BadJarFile@ in @IO@ if the read fails
readJarX :: FilePath -> IO CookieJar
readJarX p =
  let handleErr = either (const $ throwIO InvalidJar) pure
   in readJar p >>= handleErr


{- | Builder for one cookie; generates a single line in the Cookie Jar file format

the values of the following fields are not output, as the file format does
support them.

- 'cookie_creation_time'
- 'cookie_last_access_time'
- 'cookie_persistent'
-}
cookieBuilder :: Cookie -> Builder
cookieBuilder c =
  let
    httpOnly True = "#HttpOnly_"
    httpOnly False = mempty
    bool True = "TRUE"
    bool False = "FALSE"
    unixTime = integerDec . round . utcTimeToPOSIXSeconds
    tab = char7 '\t'
   in
    httpOnly (cookie_http_only c)
      <> byteString (cookie_domain c)
      <> tab
      <> bool (cookie_host_only c)
      <> tab
      <> byteString (cookie_path c)
      <> tab
      <> bool (cookie_secure_only c)
      <> tab
      <> unixTime (cookie_expiry_time c)
      <> tab
      <> byteString (cookie_name c)
      <> tab
      <> byteString (cookie_value c)
