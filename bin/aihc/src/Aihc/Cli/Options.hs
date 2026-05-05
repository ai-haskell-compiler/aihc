module Aihc.Cli.Options
  ( Command (..),
    InstallOptions (..),
    parseCommandIO,
    parseCommandPure,
    parserInfo,
  )
where

import Options.Applicative qualified as OA

data Command
  = CmdInstall !InstallOptions
  | CmdRepl
  deriving (Eq, Show)

data InstallOptions = InstallOptions
  { installPackageName :: !String,
    installPackageVersion :: !(Maybe String),
    installStoreRoot :: !(Maybe FilePath),
    installOffline :: !Bool
  }
  deriving (Eq, Show)

parseCommandIO :: IO Command
parseCommandIO = OA.execParser parserInfo

parseCommandPure :: [String] -> Either String Command
parseCommandPure args =
  case OA.execParserPure OA.defaultPrefs parserInfo args of
    OA.Success command -> Right command
    OA.Failure failure ->
      let (message, _) = OA.renderFailure failure "aihc"
       in Left message
    OA.CompletionInvoked _ -> Left "completion invoked"

parserInfo :: OA.ParserInfo Command
parserInfo =
  OA.info
    (commandParser OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.header "aihc - command-line interface for the aihc compiler"
    )

commandParser :: OA.Parser Command
commandParser =
  OA.subparser
    ( OA.command
        "install"
        ( OA.info
            (CmdInstall <$> installOptionsParser OA.<**> OA.helper)
            (OA.progDesc "Install a Hackage package into the aihc store scaffold")
        )
        <> OA.command
          "repl"
          ( OA.info
              (pure CmdRepl OA.<**> OA.helper)
              (OA.progDesc "Start the aihc expression REPL")
          )
    )

installOptionsParser :: OA.Parser InstallOptions
installOptionsParser =
  InstallOptions
    <$> OA.strArgument
      ( OA.metavar "PACKAGE"
          <> OA.help "Hackage package name"
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "version"
              <> OA.metavar "VERSION"
              <> OA.help "Exact Hackage package version"
          )
      )
    <*> OA.optional
      ( OA.strOption
          ( OA.long "store"
              <> OA.metavar "DIR"
              <> OA.help "Override the aihc store root"
          )
      )
    <*> OA.switch
      ( OA.long "offline"
          <> OA.help "Use only cached package data"
      )
