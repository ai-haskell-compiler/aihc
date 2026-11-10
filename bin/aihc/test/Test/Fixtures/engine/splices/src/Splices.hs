module Splices (application, text, unique, recovered) where

import Aihc.TH.Splice (runSplice)
import Language.Haskell.TH

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
