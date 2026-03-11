{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Provides parser for @$HOME/.netrc@ files
--
-- The implemented grammar is approximately:
--
-- > NETRC := (WS|<comment>)* (ENTRY (WS+ <comment>*)+)* ENTRY?
-- >
-- > ENTRY := 'machine' WS+ <value> WS+ ((account|username|password) WS+ <value>)*
-- >        | 'default' WS+ (('account'|'username'|'password') WS+ <value>)*
-- >        | 'macdef' <value> LF (<line> LF)* LF
-- >
-- > WS := (LF|SPC|TAB)
-- >
-- > <line>  := !LF+
-- > <value> := !WS+
-- > <comment> := '#' !LF* LF
--
-- As an extension to the @.netrc@-format as described in .e.g.
-- <http://linux.die.net/man/5/netrc netrc(5)>, @#@-style comments are
-- tolerated.  Comments are currently only allowed before, between,
-- and after @machine@\/@default@\/@macdef@ entries. Be aware though
-- that such @#@-comment are not supported by all @.netrc@-aware
-- applications, including @ftp(1)@.
module Network.NetRc
    ( -- * Types
      NetRc(..)
    , NetRcHost(..)
    , NetRcMacDef(..)

      -- * Formatters
    , netRcToBuilder
    , netRcToByteString

      -- * Parsers
    , netRcParsec
    , parseNetRc

      -- * Utilities
    , readUserNetRc
    ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import           Data.Data
import           Data.Either (rights, lefts)
import           Data.List (intersperse)
#if !MIN_VERSION_base(4,20,0)
import           Data.List (foldl')
#endif
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup ((<>))
#endif
import           GHC.Generics
import           System.Environment
import           System.IO.Error
import qualified Text.Parsec as P
import qualified Text.Parsec.ByteString as P

-- | @machine@ and @default@ entries describe remote accounts
--
-- __Invariant__: fields must not contain any @TAB@s, @SPACE@, or @LF@s.
data NetRcHost = NetRcHost
    { nrhName      :: !ByteString   -- ^ Remote machine name (@""@ for @default@-entries)
    , nrhLogin     :: !ByteString   -- ^ @login@ property (@""@ if missing)
    , nrhPassword  :: !ByteString   -- ^ @password@ property (@""@ if missing)
    , nrhAccount   :: !ByteString   -- ^ @account@ property (@""@ if missing)
    , nrhMacros    :: [NetRcMacDef] -- ^ associated @macdef@ entries
    } deriving (Eq, Ord, Show, Data, Generic)

instance NFData NetRcHost where rnf !_ = ()

-- | @macdef@ entries defining @ftp@ macros
data NetRcMacDef = NetRcMacDef
    { nrmName :: !ByteString -- ^ Name of @macdef@ entry
                             --
                             -- __Invariant__: must not contain any @TAB@s, @SPACE@, or @LF@s
    , nrmBody :: !ByteString -- ^ Raw @macdef@ body
                             --
                             -- __Invariant__: must not contain null-lines, i.e. consecutive @LF@s
    } deriving (Eq, Ord, Show, Data, Generic)

instance NFData NetRcMacDef where rnf !_ = ()

-- | Represents (semantic) contents of a @.netrc@ file
data NetRc = NetRc
    { nrHosts  :: [NetRcHost]   -- ^ @machine@\/@default@ entries
                                --
                                -- __Note__: If it exists, the
                                -- @default@ entry ought to be the
                                -- last entry, otherwise it can cause
                                -- later entries to become invisible
                                -- for some implementations
                                -- (e.g. @ftp(1)@)

    , nrMacros :: [NetRcMacDef] -- ^ Non-associated @macdef@ entries
                                --
                                -- __Note__: @macdef@ entries not
                                -- associated with host-entries are
                                -- invisible to some applications
                                -- (e.g. @ftp(1)@).
    } deriving (Eq, Ord, Show, Data, Generic)

instance NFData NetRc where
    rnf (NetRc ms ds) = ms `deepseq` ds `deepseq` ()

-- | Construct a 'ByteString' 'BB.Builder'
netRcToBuilder :: NetRc -> BB.Builder
netRcToBuilder (NetRc ms ds) =
    mconcat . intersperse nl $ map netRcMacDefToBuilder ds <> map netRcHostToBuilder ms
  where
    netRcHostToBuilder (NetRcHost {..})
        = mconcat $
          [ mline
          , prop "login"    nrhLogin
          , prop "password" nrhPassword
          , prop "account"  nrhAccount
          , nl
          ] <> (intersperse nl $ map netRcMacDefToBuilder nrhMacros)
      where
        mline | B.null nrhName = BB.byteString "default"
              | otherwise      = BB.byteString "machine" <> spc <> BB.byteString nrhName

        prop lab val | B.null val = mempty
                     | otherwise  = spc <> BB.byteString lab <> spc <> BB.byteString val

    netRcMacDefToBuilder (NetRcMacDef {..})
        = BB.byteString "macdef" <> spc <> BB.byteString nrmName <>
          (if B.null nrmBody then mempty else nl <> BB.byteString nrmBody) <>
          nl

    spc = BB.charUtf8 ' '
    nl  = BB.charUtf8 '\n'

-- | Format 'NetRc' into a 'ByteString'
--
-- This is currently just a convenience wrapper around 'netRcToBuilder'
netRcToByteString :: NetRc -> ByteString
netRcToByteString = LB.toStrict . BB.toLazyByteString . netRcToBuilder

-- | Convenience wrapper for 'netRcParsec' parser
--
-- This is basically just
--
-- @
-- 'parseNetRc' = 'P.parse' ('netRcParsec' <* 'P.eof')
-- @
--
-- This wrapper is mostly useful for avoiding to have to import Parsec
-- modules (and to build-depend explicitly on @parsec@).
--
parseNetRc :: P.SourceName -> ByteString -> Either P.ParseError NetRc
parseNetRc = P.parse (netRcParsec <* P.eof)

-- | Reads and parses default @$HOME/.netrc@
--
-- Returns 'Nothing' if @$HOME@ variable undefined and/or if @.netrc@ if missing.
-- Throws standard IO exceptions in case of other filesystem-errors.
--
-- __Note__: This function performs no permission sanity-checking on
--           the @.netrc@ file
readUserNetRc :: IO (Maybe (Either P.ParseError NetRc))
readUserNetRc = do
    mhome <- lookupEnv "HOME"
    case mhome of
        Nothing -> return Nothing
        Just "" -> return Nothing
        Just ho -> do
            let fn = ho ++ "/.netrc"
            ret <- tryIOError (B.readFile fn)
            case ret of
                Left e | isDoesNotExistError e -> return Nothing
                       | otherwise             -> ioError e
                Right b -> return $! Just $! parseNetRc fn b

-- | "Text.Parsec.ByteString" 'P.Parser' for @.netrc@ grammar
netRcParsec :: P.Parser NetRc
netRcParsec = do
    entries <- uncurry NetRc . normEnts . splitEithers <$> (wsOrComments0 *> P.sepEndBy netrcEnt wsOrComments1)

    return entries
  where
    wsOrComments0 = P.skipMany comment >> P.skipMany (wsChars1 >> P.skipMany comment)
    wsOrComments1 = P.skipMany1 (wsChars1 >> P.skipMany comment)

    netrcEnt = (Left <$> hostEnt) <|> (Right <$> macDefEnt)

    normEnts [] = ([], [])
    normEnts (([], ms):es) = (normEnts' es, ms)
    normEnts es = (normEnts' es, [])

    normEnts' :: [([NetRcHost],[NetRcMacDef])] -> [NetRcHost]
    normEnts' [] = []
    normEnts' (([], _):_) = error "netRcParsec internal error"
    normEnts' ((hs, ms):es) = init hs ++ ((last hs) { nrhMacros = ms } : normEnts' es)

macDefEnt :: P.Parser NetRcMacDef
macDefEnt = do
    void $ P.try (P.string "macdef")
    wsChars1
    n <- tok P.<?> "macdef-name"
    P.skipMany (P.oneOf "\t ")
    lf
    bodyLines <- P.sepEndBy neline lf
    return $! NetRcMacDef n (BC.pack $ unlines bodyLines)
  where
    neline = P.many1 (P.noneOf "\n")

hostEnt :: P.Parser NetRcHost
hostEnt = do
    nam <- mac <|> def
    ps <- P.many (P.try (wsChars1 *> pval))
    return $! foldl' setFld (NetRcHost nam "" "" "" []) ps
  where
    def = P.try (P.string "default") *> pure ""

    mac = do
        P.try (void $ P.string "machine")
        wsChars1
        tok P.<?> "hostname"

    -- pval := ((account|username|password) WS+ <value>)
    pval = hlp "login"    PValLogin   <|>
           hlp "account"  PValAccount <|>
           hlp "password" PValPassword
      where
        hlp tnam cons = P.try (P.string tnam) *> wsChars1 *>
                        (cons <$> tok P.<?> (tnam ++ "-value"))

    setFld n (PValLogin    v) = n { nrhLogin    = v }
    setFld n (PValAccount  v) = n { nrhAccount  = v }
    setFld n (PValPassword v) = n { nrhPassword = v }

tok :: P.Parser ByteString
tok = BC.pack <$> P.many1 notWsChar P.<?> "token"

data PVal = PValLogin    !ByteString
          | PValAccount  !ByteString
          | PValPassword !ByteString
          deriving Show

lf, wsChar, wsChars1 :: P.Parser ()
lf       = void (P.char '\n') P.<?> "line-feed"
wsChar   = void (P.oneOf "\t \n")
wsChars1 = P.skipMany1 wsChar

-- | Any 'Char' not parsed by 'wsChar'
notWsChar :: P.Parser Char
notWsChar = P.noneOf "\t \n"

-- | Comments (where allowed) go till rest of line
comment :: P.Parser ()
comment = (P.char '#' *> skipToEol) P.<?> "comment"

-- | Consume/skip rest of line
skipToEol :: P.Parser ()
skipToEol = P.skipMany notlf <* (lf <|> P.eof)
  where
    notlf = P.noneOf "\n"

-- | Regroup lst of 'Either's into pair of lists
splitEithers :: [Either l r] -> [([l], [r])]
splitEithers = goL
  where
    goL []    = []
    goL es    = let (pfx,es') = span isLeft  es in goR es' (lefts pfx)

    goR [] ls = [(ls,[])]
    goR es ls = let (pfx,es') = span isRight es in (ls,rights pfx) : goL es'

    isLeft (Left _)  = True
    isLeft (Right _) = False

    isRight = not . isLeft
