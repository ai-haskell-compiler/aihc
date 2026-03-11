-- |
--
-- Module      : Main
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Main
  ( main
  ) where

import Prelude

import Data.Foldable (fold)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.URI.Template
import Network.URI.Template.Expand
import Network.URI.Template.Internal
import Network.URI.Template.Internal.Parse (parseOpt)
import Network.URI.Template.Internal.Pretty hiding (Ann)
import Network.URI.Template.Internal.Pretty qualified as Template
import Options.Applicative
import Prettyprinter.Render.Terminal qualified as Ansi
import Prettyprinter.Render.Text qualified as Text
import System.IO (hIsTerminalDevice, stdout)

data Options = Options
  { variables :: Map VarName VarValue
  , noPretty :: Bool
  , template :: Template
  }

parseOptions :: IO Options
parseOptions =
  execParser
    $ info (optionsParser <**> helper)
    $ progDesc "Expand a URI template"

optionsParser :: Parser Options
optionsParser =
  Options . fold
    <$> many
      ( option (eitherReader $ parseOpt variableP)
          $ mconcat
            [ long "var"
            , helpDoc
                $ Just
                $ vsep
                  [ "Variables to use in expansion"
                  , indent 2
                      $ vsep
                        [ "--var 'name := \"scalar\"'"
                        , "--var 'name := (\"scalar\", ...)'"
                        , "--var 'name := [(\"scalar\",\"scalar\"), ...)'"
                        , ""
                        ]
                  ]
            , metavar "ARG"
            ]
      )
    <*> switch
      ( mconcat
          [ long "no-pretty"
          , help "Only output the resulting expansion and single newline"
          ]
      )
    <*> argument
      (eitherReader $ parseOpt templateP)
      ( mconcat
          [ metavar "TEMPLATE"
          , help "URI template to expand"
          ]
      )

main :: IO ()
main = do
  opts <- parseOptions
  prettyPrint opts $ expandTemplateDoc opts.variables opts.template

prettyPrint :: Options -> Doc Template.Ann -> IO ()
prettyPrint opts expanded = do
  tty <- hIsTerminalDevice stdout

  let
    render =
      if tty && not opts.noPretty
        then Ansi.renderStrict
        else Text.renderStrict

    maxWidth =
      maximum
        $ map (T.length . unVarName)
        $ Map.keys opts.variables

  T.putStrLn
    $ render
    $ layoutPretty defaultLayoutOptions
    $ reAnnotate annToAnsi
    $ vsep
      [ sectionPretty "Variables"
          $ reAnnotate AnnTemplate
          $ vsep
          $ map (uncurry $ variablePretty maxWidth)
          $ Map.toList opts.variables
      , ""
      , sectionPretty "Template"
          $ reAnnotate AnnTemplate
          $ templatePretty opts.template
      , ""
      , sectionPretty "Expanded" $ reAnnotate AnnTemplate expanded
      ]

sectionPretty :: Text -> Doc Ann -> Doc Ann
sectionPretty name d =
  vsep
    [ annotate AnnLabel (pretty name) <> ":"
    , indent 2 d
    ]

data Ann
  = AnnLabel
  | AnnTemplate Template.Ann

annToAnsi :: Ann -> Ansi.AnsiStyle
annToAnsi = \case
  AnnLabel -> Ansi.bold
  AnnTemplate x -> case x of
    AnnPunctuation -> Ansi.bold
    AnnOperator -> Ansi.color Ansi.Magenta
    AnnIntercalate -> Ansi.color Ansi.Magenta
    AnnVarName -> Ansi.colorDull Ansi.Cyan
    AnnVarValue -> Ansi.colorDull Ansi.Green
    AnnModifier -> Ansi.colorDull Ansi.Green
    AnnString -> Ansi.italicized <> Ansi.colorDull Ansi.Green
