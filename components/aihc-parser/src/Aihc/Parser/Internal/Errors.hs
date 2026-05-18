module Aihc.Parser.Internal.Errors
  ( parseErrorBundleToSpannedText,
    parseErrorsToSpannedText,
  )
where

import Aihc.Parser.Lex (LexToken (..), TokenOrigin (..))
import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Parser.Types (FoundToken (..), ParseErrorBundle, ParserErrorComponent (..), TokStream)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, pretty, vcat)
import Prettyprinter.Render.Text qualified as RText
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Error (ErrorFancy (..), ErrorItem (..))
import Text.Megaparsec.Error qualified as MPE

parseErrorBundleToSpannedText :: ParseErrorBundle -> [(SourceSpan, Text)]
parseErrorBundleToSpannedText bundle =
  parseErrorsToSpannedText (NE.toList (MPE.bundleErrors bundle))

parseErrorsToSpannedText :: [MPE.ParseError TokStream ParserErrorComponent] -> [(SourceSpan, Text)]
parseErrorsToSpannedText errs =
  [ (fromMaybe NoSourceSpan mSpan, RText.renderStrict (layoutPretty defaultLayoutOptions doc))
  | err <- List.sortOn MP.errorOffset errs,
    (mSpan, doc) <- renderParseErrors err
  ]

renderParseErrors :: MPE.ParseError TokStream ParserErrorComponent -> [(Maybe SourceSpan, Doc ann)]
renderParseErrors err =
  case err of
    MPE.TrivialError _ mUnexpected expected ->
      let mSpan = trivialUnexpectedSpan mUnexpected
       in [(mSpan, vcat (map pretty (renderTrivialError mUnexpected expected)))]
    MPE.FancyError _ fancySet ->
      map renderFancyError (Set.toAscList fancySet)
  where
    trivialUnexpectedSpan :: Maybe (ErrorItem LexToken) -> Maybe SourceSpan
    trivialUnexpectedSpan mItem =
      case mItem of
        Just (Tokens ts) -> Just (lexTokenSpan (NE.head ts))
        _ -> Nothing

    renderFancyError :: ErrorFancy ParserErrorComponent -> (Maybe SourceSpan, Doc ann)
    renderFancyError fancy =
      case fancy of
        ErrorCustom custom ->
          ( customFoundSpan custom,
            vcat (map pretty (customMessageLines custom))
          )
        ErrorFail message -> (Nothing, pretty message)
        _ ->
          ( Nothing,
            pretty (show fancy)
          )

    customFoundSpan :: ParserErrorComponent -> Maybe SourceSpan
    customFoundSpan (UnexpectedTokenExpecting (Just found) _ _) =
      Just (foundTokenSpan found)
    customFoundSpan _ = Nothing

    customMessageLines :: ParserErrorComponent -> [String]
    customMessageLines e@(UnexpectedTokenExpecting mFound _ contexts) =
      [maybe "unexpected end of input" renderUnexpectedToken mFound, MPE.showErrorComponent e]
        <> map (\context -> "context: " <> T.unpack context) contexts

renderTrivialError :: Maybe (ErrorItem LexToken) -> Set.Set (ErrorItem LexToken) -> [String]
renderTrivialError mUnexpected expected =
  maybe [] (\item -> ["unexpected " <> renderErrorItem item]) mUnexpected
    <> ["expecting " <> renderExpectedItems (Set.toAscList expected) | not (Set.null expected)]

renderErrorItem :: ErrorItem LexToken -> String
renderErrorItem item =
  case item of
    Tokens toks -> unwords (map (T.unpack . lexTokenText) (NE.toList toks))
    Label label -> NE.toList label
    EndOfInput -> "end of input"

renderExpectedItems :: [ErrorItem LexToken] -> String
renderExpectedItems items =
  case map renderErrorItem items of
    [] -> ""
    [item] -> item
    [itemA, itemB] -> itemA <> " or " <> itemB
    rendered -> List.intercalate ", " (init rendered) <> ", or " <> last rendered

renderUnexpectedToken :: FoundToken -> String
renderUnexpectedToken found =
  "unexpected " <> tokenDescriptor found

tokenDescriptor :: FoundToken -> String
tokenDescriptor found =
  case foundTokenOrigin found of
    InsertedLayout -> "end of input"
    FromSource ->
      "'" <> T.unpack (foundTokenText found) <> "'"
