{-# LANGUAGE CPP #-}

module Options where

import qualified Data.ByteString.Char8 as C

#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid           ((<>))
#endif

import           Data.NanoID
import           Data.Version          (showVersion)
import           Options.Applicative
import           Paths_NanoID          (version)

data Options =
  Options
    { alphabet :: C.ByteString
    , length   :: Int
    , password :: Bool
    , quantity :: Int
    , newline  :: Bool
    , showver  :: Bool
    }

opts :: ParserInfo Options
opts = info (options <**> helper)
  ( fullDesc
    <> progDesc "NanoID generator"
    <> header ( "nanoid "
                <> showVer
                <> ", (c) Michel Boucey 2022-2025" ) )

options :: Parser Options
options =
  Options
    <$>
      strOption
        ( short 'a'
          <> long "alphabet"
          <> help "Use an alternative alphabet (ascii chars only)"
          <> value (unAlphabet defaultAlphabet))
    <*>
      option auto
        ( short 'l'
          <> long "length"
          <> help "Get a shorter NanoID (Default length is 21 chars)"
          <> value 21 )
    <*>
      flag False True
        ( short 'p'
          <> long "password"
          <> help "Special password generation" )
    <*>
      option auto
        ( short 'q'
          <> long "quantity"
          <> help "Quantity of NanoID to generate"
          <> value 1 )
    <*>
      flag True False
        ( short 'n'
          <> long "newline"
          <> help "Do not output the trailing newline" )
    <*>
      flag False True
        ( short 'v'
          <> long "version"
          <> help "Show version" )

showVer :: String
showVer = "v" <> showVersion version

