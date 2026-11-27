-- The program runs the quotes of the package quote-below. That package
-- does not depend on template-haskell.
module Main (main) where

import Language.Haskell.TH.Syntax (unTypeCode)
import QuoteBelow (quoted, typed)

main :: IO ()
main = do
  quoted >>= print
  unTypeCode typed >>= print
