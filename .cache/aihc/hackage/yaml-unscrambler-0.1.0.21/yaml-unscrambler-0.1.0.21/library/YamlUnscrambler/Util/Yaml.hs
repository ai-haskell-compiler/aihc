module YamlUnscrambler.Util.Yaml where

import qualified Conduit
import qualified Data.Yaml.Parser as YamlParser
import qualified Text.Libyaml as Libyaml
import YamlUnscrambler.Prelude

parseByteStringToRawDoc :: ByteString -> Either Text YamlParser.RawDoc
parseByteStringToRawDoc input =
  first (mappend "YAML AST parsing: " . showAsText)
    $ unsafePerformIO
    $ try @SomeException
    $ Conduit.runConduitRes
    $ Conduit.fuse (Libyaml.decode input) (YamlParser.sinkRawDoc)
