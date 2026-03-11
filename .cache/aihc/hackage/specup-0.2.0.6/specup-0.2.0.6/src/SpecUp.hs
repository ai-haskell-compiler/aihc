-- |Manage the application of templates to custom yaml
module SpecUp(invokeTemplateOnSpec) where

import Data.Text (Text)
import Data.ByteString.Char8 (ByteString)
import Data.Yaml (decodeEither', ParseException, prettyPrintParseException)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Mustache (compileMustacheText, renderMustache)
import qualified Data.Text.Lazy as LazyText
import Data.Either.Extra (mapLeft)

-- |Apply a given template to the supplied yaml
invokeTemplateOnSpec :: Text ->
                        ByteString ->
                        Either String LazyText.Text
invokeTemplateOnSpec templ spec =
  renderMustache <$> compiledTemplate <*> compiledSpec
  where compiledTemplate = templateCompilationErrorHandler
                           $ compileMustacheText "base template" templ
        compiledSpec = yamlDecodingErrorHandler
                       $ decodeEither' spec
        yamlDecodingErrorHandler = mapLeft
          ( ("yaml decoding failed with the following error: " <>)
          . prettyPrintParseException
          )
        templateCompilationErrorHandler = mapLeft
          ( ("Texplate compilation failed with the following error: " <>)
          . errorBundlePretty
          )

