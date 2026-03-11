------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : redact: hide secret text on the terminal
-- Copyright   : Copyright (c) 2020-2024 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

#if defined(MIN_VERSION_ansi_wl_pprint)
#if MIN_VERSION_ansi_wl_pprint (1,0,2)
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
#endif

module Main (main) where

-- https://hackage.haskell.org/package/ansi-terminal
import qualified System.Console.ANSI as Term

-- https://hackage.haskell.org/package/base
import Control.Applicative ((<|>), optional)
import Control.Exception (onException)
import Control.Monad (foldM, forM_, when)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Char (isSpace, toLower)
import Data.Functor.Identity (Identity(runIdentity))
import Data.List (dropWhileEnd, intercalate)
import Data.Maybe (fromMaybe, isJust)
import Data.String (fromString)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr, stdin)

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- (redact)
import qualified Redact
import qualified Redact.Markdown as Redact
import qualified Redact.Monad.Terminal as RedactTerm

-- (redact:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Constants

defaultColor :: Term.Color
defaultColor = Term.Red

defaultIntensity :: Term.ColorIntensity
defaultIntensity = Term.Vivid

defaultLenient :: Bool
defaultLenient = False

envColorName, envIntensityName, envLenientName :: String
envColorName     = "REDACT_COLOR"
envIntensityName = "REDACT_INTENSITY"
envLenientName   = "REDACT_LENIENT"

configFileName :: FilePath
configFileName = "redact.ini"

------------------------------------------------------------------------------
-- $Colors

colorMap :: [(String, Term.Color)]
colorMap =
    [ (toLower <$> show color, color)
    | color <- [minBound ..]
    ]

parseColor
  :: String
  -> Either String Term.Color
parseColor s
    = maybe (Left $ "unknown color: " ++ s) Right
    $ lookup (toLower <$> s) colorMap

------------------------------------------------------------------------------
-- $Intensities

intensityMap :: [(String, Term.ColorIntensity)]
intensityMap =
    [ (toLower <$> show intensity, intensity)
    | intensity <- [minBound ..]
    ]

parseIntensity
  :: String
  -> Either String Term.ColorIntensity
parseIntensity s
    = maybe (Left $ "unknown intensity: " ++ s) Right
    $ lookup (toLower <$> s) intensityMap

------------------------------------------------------------------------------
-- $Lenient

parseLenient
  :: String
  -> Either String Bool
parseLenient s = case toLower <$> s of
    "true"  -> Right True
    "false" -> Right False
    _other  -> Left $ "lenient setting not true/false: " ++ s

------------------------------------------------------------------------------
-- $Options

-- | Program options
data Options f
  = Options
    { optColor     :: !(f Term.Color)
    , optIntensity :: !(f Term.ColorIntensity)
    , optFile      :: !(Maybe FilePath)
    , optLenient   :: !(f Bool)
    , optColors    :: !Bool
    , optTest      :: !Bool
    }

getOptions :: IO (Options Identity)
getOptions = do
    opts <-
      maybeLoadEnv getEnvColor optColor
        (\o x -> o { optColor = x }) =<<
      maybeLoadEnv getEnvIntensity optIntensity
        (\o x -> o { optIntensity = x }) =<<
      maybeLoadEnv getEnvLenient optLenient
        (\o x -> o { optLenient = x }) =<<
      parseArgs
    case (optColor opts, optIntensity opts, optLenient opts) of
      (Just color, Just intensity, Just lenient) -> return Options
        { optColor     = pure color
        , optIntensity = pure intensity
        , optFile      = optFile opts
        , optLenient   = pure lenient
        , optColors    = optColors opts
        , optTest      = optTest opts
        }
      _otherOptions -> do
        configOpts <- loadConfigFile
        return Options
          { optColor = pure . fromMaybe defaultColor $
              optColor opts <|> configColor configOpts
          , optIntensity = pure . fromMaybe defaultIntensity $
              optIntensity opts <|> configIntensity configOpts
          , optFile = optFile opts
          , optLenient = pure . fromMaybe defaultLenient $
              optLenient opts <|> configLenient configOpts
          , optColors = optColors opts
          , optTest = optTest opts
          }
  where
    maybeLoadEnv
      :: IO (Maybe a)
      -> (Options Maybe -> Maybe a)
      -> (Options Maybe -> Maybe a -> Options Maybe)
      -> Options Maybe
      -> IO (Options Maybe)
    maybeLoadEnv getEnv getOpt setOpt opts
      | isJust (getOpt opts) = pure opts
      | otherwise = setOpt opts <$> getEnv

------------------------------------------------------------------------------
-- $Arguments

parseArgs :: IO (Options Maybe)
parseArgs
    = OA.execParser
    . OA.info (LibOA.helper <*> LibOA.versioner Redact.version <*> options)
    $ mconcat
        [ OA.fullDesc
        , OA.progDesc "hide secret text on the terminal"
        , OA.failureCode 2
        , OA.footerDoc $ Just footer
        ]

options :: OA.Parser (Options Maybe)
options = Options
    <$> colorOption
    <*> intensityOption
    <*> fileOption
    <*> lenientOption
    <*> colorsOption
    <*> testOption
  where
    colorOption :: OA.Parser (Maybe Term.Color)
    colorOption = optional . OA.option (OA.eitherReader parseColor) $ mconcat
      [ OA.long "color"
      , OA.short 'c'
      , OA.metavar "COLOR"
      , OA.help "redacted text color"
      ]

    intensityOption :: OA.Parser (Maybe Term.ColorIntensity)
    intensityOption =
      optional . OA.option (OA.eitherReader parseIntensity) $ mconcat
        [ OA.long "intensity"
        , OA.short 'i'
        , OA.metavar "INTENSITY"
        , OA.help "redacted text color intensity"
        ]

    fileOption :: OA.Parser (Maybe FilePath)
    fileOption = optional . OA.strOption $ mconcat
      [ OA.long "file"
      , OA.short 'f'
      , OA.metavar "PATH"
      , OA.help "input file (default: STDIN)"
      ]

    lenientOption :: OA.Parser (Maybe Bool)
    lenientOption = OA.flag Nothing (Just True) $ mconcat
      [ OA.long "lenient"
      , OA.short 'l'
      , OA.help "do not exit on parse errors"
      ]

    colorsOption :: OA.Parser Bool
    colorsOption = OA.switch $ mconcat
      [ OA.long "colors"
      , OA.help "redact test text using all possible colors"
      ]

    testOption :: OA.Parser Bool
    testOption = OA.switch $ mconcat
      [ OA.long "test"
      , OA.help "redact test text using the configured color"
      ]

footer :: LibOA.Doc
footer = LibOA.vspace
    [ colorsHelp
    , intensitiesHelp
    , lenientHelp
    , settingsHelp
    ]
  where
    colorsHelp :: LibOA.Doc
    colorsHelp = LibOA.section "COLOR values:" . LibOA.string $
      intercalate ", " (fst <$> colorMap)

    intensitiesHelp :: LibOA.Doc
    intensitiesHelp = LibOA.section "INTENSITY values:" . LibOA.string $
      intercalate ", " (fst <$> intensityMap)

    lenientHelp :: LibOA.Doc
    lenientHelp =
      LibOA.section "LENIENT values:" $ LibOA.string "true, false"

    settingsHelp :: LibOA.Doc
    settingsHelp = LibOA.section "Settings priority:" . LibOA.vcat $
      [ LibOA.section' 5 "1. command-line options" $
          LibOA.vcat
            [ LibOA.string "--color"
            , LibOA.string "--intensity"
            , LibOA.string "--lenient"
            ]
      , LibOA.section' 5 "2. environment variables" $
          LibOA.vcat
            [ LibOA.string $ envColorName ++ "=COLOR"
            , LibOA.string $ envIntensityName ++ "=INTENSITY"
            , LibOA.string $ envLenientName ++ "=LENIENT"
            ]
      , LibOA.section' 5 ("3. settings file (" ++ configFileName ++ ")") $
          LibOA.vcat
            [ LibOA.string "color=COLOR"
            , LibOA.string "intensity=INTENSITY"
            , LibOA.string "lenient=LENIENT"
            ]
      , LibOA.section' 5 "4. defaults" $
          LibOA.table_ 2
            [ ["color:", toLower <$> show defaultColor]
            , ["intensity:", toLower <$> show defaultIntensity]
            , ["lenient:", toLower <$> show defaultLenient]
            ]
      ]

------------------------------------------------------------------------------
-- $EnvVars

getEnvColor :: IO (Maybe Term.Color)
getEnvColor = getEnv' envColorName parseColor

getEnvIntensity :: IO (Maybe Term.ColorIntensity)
getEnvIntensity = getEnv' envIntensityName parseIntensity

getEnvLenient :: IO (Maybe Bool)
getEnvLenient = getEnv' envLenientName parseLenient

getEnv'
  :: String
  -> (String -> Either String a)
  -> IO (Maybe a)
getEnv' envName parse = do
    meex <- fmap parse <$> lookupEnv envName
    case meex of
      Nothing -> return Nothing
      Just (Right x) -> return $ Just x
      Just (Left err) -> errorExit $
        "environment variable " ++ envName ++ ": " ++ err

------------------------------------------------------------------------------
-- $ConfigFile

data ConfigFileOptions
  = ConfigFileOptions
    { configColor     :: !(Maybe Term.Color)
    , configIntensity :: !(Maybe Term.ColorIntensity)
    , configLenient   :: !(Maybe Bool)
    }

loadConfigFile :: IO ConfigFileOptions
loadConfigFile = do
    path <- Dir.getXdgDirectory Dir.XdgConfig configFileName
    exists <- Dir.doesFileExist path
    if exists
      then foldM go defOpts . zip [1..] . map strip . lines =<< readFile path
      else return defOpts
  where
    defOpts :: ConfigFileOptions
    defOpts = ConfigFileOptions
      { configColor     = Nothing
      , configIntensity = Nothing
      , configLenient   = Nothing
      }

    go :: ConfigFileOptions -> (Int, String) -> IO ConfigFileOptions
    go opts (_, "") = return opts
    go opts (_, '#' : _) = return opts
    go opts (lineNum, line) = case bimap strip strip (break (== '=') line) of
      ("", _) -> errorExit $ "config file: empty key on line " ++ show lineNum
      (key, '=' : value) -> case key of
        "color" -> case parseColor value of
          Right color -> return opts { configColor = Just color }
          Left err -> errorExit $
            "config file: color error on line " ++ show lineNum ++ ": " ++ err
        "intensity" -> case parseIntensity value of
          Right intensity -> return opts { configIntensity = Just intensity }
          Left err -> errorExit $
            "config file: intensity error on line " ++ show lineNum ++ ": " ++
            err
        "lenient" -> case parseLenient value of
          Right lenient -> return opts { configLenient = Just lenient }
          Left err -> errorExit $
            "config file: lenient error on line " ++ show lineNum ++ ": " ++
            err
        _otherKey -> return opts
      _mistmatch ->
        errorExit $ "config file: unable to parse line " ++ show lineNum

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
{-# INLINE strip #-}

------------------------------------------------------------------------------
-- $Implementation

runTest :: Term.Color -> Term.ColorIntensity -> IO ()
runTest color intensity =
    RedactTerm.putLines (RedactTerm.redactSGRs color intensity)
      [ Redact.NormalLine
          [ Redact.Stet . fromString $
              (toLower <$> show color) ++ " " ++
              (toLower <$> show intensity) ++ ": `"
          , Redact.Redact "hidden"
          , Redact.Stet "`"
          ]
      ]

runColors :: IO ()
runColors =
    forM_ [minBound ..] $ \color ->
      forM_ [minBound ..] $ \intensity ->
        runTest color intensity

------------------------------------------------------------------------------
-- $CLI

errorExit :: String -> IO a
errorExit message = do
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure 1

main :: IO ()
main = flip onException RedactTerm.reset $ do
    Options{..} <- getOptions
    let color     = runIdentity optColor
        intensity = runIdentity optIntensity
        sgrs      = RedactTerm.redactSGRs color intensity
        lenient   = runIdentity optLenient
    when optColors $ runColors >> exitSuccess
    when optTest $ runTest color intensity >> exitSuccess
    either (errorExit . show) return =<< case optFile of
      Just path
        | lenient   ->           Redact.fileToTerminal'   sgrs path
        | otherwise ->           Redact.fileToTerminal    sgrs path
      Nothing
        | lenient   -> Right <$> Redact.handleToTerminal' sgrs stdin
        | otherwise ->           Redact.handleToTerminal  sgrs stdin
