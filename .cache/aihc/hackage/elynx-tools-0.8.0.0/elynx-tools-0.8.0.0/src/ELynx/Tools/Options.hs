{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  ELynx.Tools.Options
-- Description :  General ELynx options
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  2 19:17:07 2021.
module ELynx.Tools.Options
  ( -- * Command options
    seedOpt,
    executionModeOpt,

    -- * Arguments
    GlobalArguments (..),
    Arguments (..),

    -- * Misc
    parseArguments,
    elynxParserInfo,
    createCommandReproducible,
    createCommand,
    elynxFooter,
  )
where

import Data.Aeson
import Data.List
import ELynx.Tools.InputOutput
import ELynx.Tools.Logger
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative hiding (empty)
import Options.Applicative.Help.Pretty

-- | Seed option. Defaults to random.
seedOpt :: Parser SeedOpt
seedOpt = toSeedOpt <$> seedParser

seedParser :: Parser (Maybe Int)
seedParser =
  optional
    $ option
      auto
    $ long "seed"
      <> short 'S'
      <> metavar "INT"
      <> help "Seed for random number generator (default: random)"

toSeedOpt :: Maybe Int -> SeedOpt
toSeedOpt Nothing = RandomUnset
toSeedOpt (Just w) = Fixed w

-- | Execution mode option parser.
executionModeOpt :: Parser ExecutionMode
executionModeOpt =
  flag
    Fail
    Overwrite
    -- DO NOT CHANGE. This option is used by 'elynx redo'.
    ( long "force"
        <> short 'f'
        <> help
          "Ignore previous analysis and overwrite existing output files."
    )

-- | A set of global arguments used by all programs. The idea is to provide a
-- common framework for shared arguments.
data GlobalArguments = GlobalArguments
  { verbosity :: Verbosity,
    outFileBaseName :: Maybe FilePath,
    executionMode :: ExecutionMode,
    writeElynxFile :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromJSON GlobalArguments

instance ToJSON GlobalArguments

-- | See 'GlobalArguments', parser function.
globalArguments :: Parser GlobalArguments
globalArguments =
  GlobalArguments
    <$> verbosityOpt
    <*> optional outFileBaseNameOpt
    <*> executionModeOpt
    <*> writeELynxOpt

-- Boolean option; be verbose; default NO.
verbosityOpt :: Parser Verbosity
verbosityOpt =
  option
    auto
    ( long "verbosity"
        <> short 'v'
        <> metavar "VALUE"
        <> value Info
        <> showDefault
        <> help ("Be verbose; one of: " ++ unwords (map show vs))
    )
  where
    vs = [minBound ..] :: [Verbosity]

-- Output filename.
outFileBaseNameOpt :: Parser FilePath
outFileBaseNameOpt =
  strOption
    ( long "output-file-basename"
        <> short 'o'
        <> metavar "NAME"
        <> help
          "Specify base name of output file"
    )

-- Write ELynx file at the end.
writeELynxOpt :: Parser Bool
writeELynxOpt =
  flag
    True
    False
    ( long "no-elynx-file"
        <> help "Do not write data required to reproduce an analysis."
    )

-- | Argument skeleton to be used with all commands.
data Arguments a = Arguments
  { global :: GlobalArguments,
    local :: a
  }
  deriving (Eq, Show, Generic)

instance (FromJSON a) => FromJSON (Arguments a)

instance (ToJSON a) => ToJSON (Arguments a)

instance (Reproducible a) => Reproducible (Arguments a) where
  inFiles = inFiles . local
  outSuffixes = outSuffixes . local
  getSeed = getSeed . local
  setSeed (Arguments g l) s = Arguments g $ setSeed l s
  parser = argumentsParser (parser @a)
  cmdName = cmdName @a
  cmdDsc = cmdDsc @a
  cmdFtr = cmdFtr @a

argumentsParser :: Parser a -> Parser (Arguments a)
argumentsParser p = Arguments <$> globalArguments <*> p

versionOpt :: Parser (a -> a)
versionOpt =
  infoOption
    (intercalate "\n" logHeader)
    ( long "version"
        -- Lower case 'v' clashes with verbosity.
        <> short 'V'
        <> help "Show version"
        <> hidden
    )

elynxParser :: Parser a -> Parser a
elynxParser p = helper <*> versionOpt <*> p

-- | Parse arguments. Provide a global description, header, footer, and so on.
-- Custom additional description (first argument) and footer (second argument)
-- can be provided. print help if needed.
parseArguments :: forall a. (Reproducible a) => IO (Arguments a)
parseArguments =
  execParser $
    elynxParserInfo (cmdDsc @a) (cmdFtr @a) (argumentsParser $ parser @a)

-- | ELynx parser info; convenience function.
elynxParserInfo :: [String] -> [String] -> Parser a -> ParserInfo a
elynxParserInfo dsc ftr = parserInfo dsc' ftr'
  where
    dsc' = if null dsc then Nothing else Just $ vsep $ map pretty dsc
    ftr' = Just . vsep $ map pretty ftr ++ elynxFooter

-- Short version of ELynx parser info for sub commands.
parserInfo :: Maybe Doc -> Maybe Doc -> Parser a -> ParserInfo a
parserInfo dsc ftr p =
  info
    (elynxParser p)
    (fullDesc <> headerDoc (Just hdr') <> progDescDoc dsc <> footerDoc ftr)
  where
    hdr' = vsep $ map pretty logHeader

-- | Create a command; convenience function.
createCommandReproducible ::
  forall a b. (Reproducible a) => (a -> b) -> Mod CommandFields b
createCommandReproducible f =
  command (cmdName @a) $
    f
      <$> parserInfo
        dsc'
        ftr'
        (parser @a)
  where
    dsc = cmdDsc @a
    ftr = cmdFtr @a
    dsc' = if null dsc then Nothing else Just $ vsep $ map pretty dsc
    ftr' = if null ftr then Nothing else Just $ vsep $ map pretty ftr

-- | Create a command; convenience function.
createCommand ::
  String ->
  [String] ->
  [String] ->
  Parser a ->
  (a -> b) ->
  Mod CommandFields b
createCommand nm dsc ftr p f = command nm $ f <$> parserInfo dsc' ftr' p
  where
    dsc' = if null dsc then Nothing else Just $ vsep $ map pretty dsc
    ftr' = if null ftr then Nothing else Just $ vsep $ map pretty ftr

-- Fill a string so that it becomes a paragraph with line breaks. Useful for
-- descriptions, headers and footers.
fillParagraph :: String -> Doc
fillParagraph = fillSep . map pretty . words

-- | Global ELynx footer.
elynxFooter :: [Doc]
elynxFooter =
  [ mempty,
    pretty "ELynx",
    pretty "-----",
    fillParagraph
      "A Haskell library and tool set for computational biology. The goal of ELynx is reproducible research. Evolutionary sequences and phylogenetic trees can be read, viewed, modified and simulated. The command line with all arguments is logged consistently, and automatically. Data integrity is verified using SHA256 sums so that validation of past analyses is possible without the need to recompute the result.",
    mempty,
    fill 9 (pretty "slynx")
      <+> pretty "Analyze, modify, and simulate evolutionary sequences.",
    fill 9 (pretty "tlynx")
      <+> pretty "Analyze, modify, and simulate phylogenetic trees.",
    fill 9 (pretty "elynx") <+> pretty "Validate and redo past analyses.",
    mempty,
    pretty "Get help for commands:",
    pretty "  slynx --help",
    mempty,
    pretty "Get help for sub commands:",
    pretty "  slynx examine --help"
  ]
