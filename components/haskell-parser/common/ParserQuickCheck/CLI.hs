module ParserQuickCheck.CLI
  ( Options (..),
    parseOptionsIO,
    parseOptionsPure,
  )
where

import qualified Options.Applicative as OA

data Options = Options
  { optMaxSuccess :: Int,
    optSeed :: Maybe Int,
    optProperty :: Maybe String,
    optJson :: Bool
  }
  deriving (Eq, Show)

parseOptionsIO :: IO Options
parseOptionsIO = OA.execParser parserInfo

parseOptionsPure :: [String] -> Either String Options
parseOptionsPure args =
  case OA.execParserPure OA.defaultPrefs parserInfo args of
    OA.Success opts -> Right opts
    OA.Failure failure ->
      let (msg, _) = OA.renderFailure failure "parser-quickcheck-batch"
       in Left msg
    OA.CompletionInvoked _ ->
      Left "shell completion requested"

parserInfo :: OA.ParserInfo Options
parserInfo =
  OA.info
    (optionsParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Run parser QuickCheck properties once and print a JSON batch report"
        <> OA.header "parser-quickcheck-batch"
    )

optionsParser :: OA.Parser Options
optionsParser =
  Options
    <$> OA.option
      positiveIntReader
      ( OA.long "max-success"
          <> OA.metavar "N"
          <> OA.value 10000
          <> OA.showDefault
          <> OA.help "Run each selected property until it succeeds N times"
      )
    <*> OA.optional
      ( OA.option
          OA.auto
          ( OA.long "seed"
              <> OA.metavar "INT"
              <> OA.help "Deterministic batch seed"
          )
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "property"
              <> OA.metavar "NAME"
              <> OA.help "Run only the named property"
          )
      )
    <*> OA.switch
      ( OA.long "json"
          <> OA.help "Accepted for compatibility; JSON is always written to stdout"
      )

positiveIntReader :: OA.ReadM Int
positiveIntReader = OA.eitherReader $ \raw ->
  case reads raw of
    [(n, "")] | n > 0 -> Right n
    _ -> Left "must be a positive integer"
