module Splices (application, text, unique, recovered, dependentFile) where

import Aihc.TH.Splice (runSplice)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)

application :: IO ()
application = runSplice (appE (varE (mkName "negate")) (litE (integerL (-7))))

text :: IO ()
text = runSplice (tupE [litE (stringL "h\233llo"), litE (charL 'x'), conE (mkName "Data.Maybe.Nothing")])

unique :: IO ()
unique =
  runSplice $ do
    reportWarning "a warning"
    x <- newName "x"
    lamE [varP x] (varE x)

recovered :: IO ()
recovered = runSplice (recover (litE (integerL 1)) (reportError "an error" >> fail "stop"))

-- | A splice names a file that the module depends on.
dependentFile :: IO ()
dependentFile = runSplice (addDependentFile "data/table.txt" >> litE (integerL 2))
