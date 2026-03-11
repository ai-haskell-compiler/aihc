{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception   (SomeException, try)
import           Control.Monad       (replicateM_)
import           Data.Maybe          (fromJust)
import qualified Data.Text           as T (pack)
import qualified Data.Text.IO        as TIO (hPutStrLn, putStr, putStrLn)
import           Data.Version        (showVersion)
import           Options.Applicative
import           Paths_ip6addr       (version)
import           System.Exit
import           System.IO           (stderr)
import           Text.IPv6Addr

showVer :: String
showVer = "ip6addr v"
       <> showVersion version
       <> " (c) Michel Boucey 2011-2025"

main :: IO ()
main = do
  Options{..} <- execParser opts
  if showver then failMsg $ T.pack showVer
    else
      case output of
        Canonical  -> out noNewline maybeIPv6Addr address unIPv6Addr
        NoIPv4     -> out noNewline maybePureIPv6Addr address unIPv6Addr
        FullLength -> out noNewline maybeFullIPv6Addr address unIPv6Addr
        PTR        -> out noNewline maybeIP6ARPA address id
        UNC        -> out noNewline maybeUNC address id
        Random     -> replicateM_ quantity (putRandAddr noNewline prefix) >> exitSuccess
  where
    putRandAddr n p = do
      let p' = T.pack p
      t <- try $ randIPv6AddrWithPrefix (if p == mempty then Nothing else Just p')
      case t of
        Right a                   -> put n (unIPv6Addr $ fromJust a)
        Left (_ :: SomeException) -> failMsg $ "'" <> p' <> "' is an invalid prefix"
    out n t i o =
      if i /= mempty
        then do
          let p = T.pack i
          case t p of
            Just a  -> put n (o a) >> exitSuccess
            Nothing -> failMsg $ "'" <> p <> "' is not an IPv6 address"
        else failMsg "See --help"
    maybeUNC t = toUNC <$> maybePureIPv6Addr t
    maybeIP6ARPA t = toIP6ARPA <$> maybeFullIPv6Addr t
    put n = if n then TIO.putStr else TIO.putStrLn
    failMsg m = TIO.hPutStrLn stderr m >> exitFailure

data Output
  = Canonical
  | NoIPv4
  | FullLength
  | PTR
  | UNC
  | Random
  deriving (Eq)

data Options =
  Options
    { showver   :: !Bool
    , output    :: !Output
    , quantity  :: !Int
    , prefix    :: !String
    , noNewline :: !Bool
    , address   :: !String
    }

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
  ( fullDesc
    <> progDesc "ip6addr"
    <> header showVer
  )

parseOptions :: Parser Options
parseOptions =
  Options
    <$>
      switch
        ( short 'v'
          <> long "version"
          <> help "Show version"
        )
    <*>
      (flag Canonical Canonical
        ( short 'c'
          <> long "canonical"
          <> help "In conformation with RFC 5952 (default output)"
        )
   <|>
      flag' NoIPv4
        ( short 'n'
          <> long "no-ipv4"
          <> help "Force the rewriting of the IPv4 address if present\
                   \ to get a pure IPv6 address made of nibbles only"
        )
   <|>
      flag' FullLength
        ( short 'f'
          <> long "full-length"
          <> help "Full IPv6 address length"
        )
   <|>
      flag' PTR
        ( short 'p'
          <> long "ptr"
          <> help "PTR reverse mapping"
        )
   <|>
      flag' UNC
        ( short 'w'
          <> long "windows-unc"
          <> help "Windows UNC path name"
        )
   <|>
      flag' Random
        ( short 'r'
          <> long "random"
          <> help "Random generation"
        )
     )
   <*>
      option auto
        ( short 'q'
          <> long "quantity"
          <> help "Amount of random addresses to generate"
          <> value 1
        )
   <*>
      option str
        ( short 's'
          <> long "prefix"
          <> help "Set a prefix for random addresses to generate"
          <> value ""
        )
   <*>
      flag False True
        ( short 'd'
          <> long "no-newline"
          <> help "Do not output trailing newlines" )
   <*>
      argument str (metavar "IPv6 address" <> value "")

