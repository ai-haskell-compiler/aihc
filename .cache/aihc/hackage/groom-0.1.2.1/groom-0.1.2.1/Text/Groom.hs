module Text.Groom (groom, groomString) where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Pretty

groomString :: String -> String
groomString s = case parseExp s of
    ParseOk x -> prettyPrint x
    ParseFailed{} -> s

groom :: Show a => a -> String
groom = groomString . show
