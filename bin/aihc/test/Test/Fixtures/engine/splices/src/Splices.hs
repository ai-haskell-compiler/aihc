module Splices (application, text) where

import Aihc.TH.Splice (runSplice)
import Language.Haskell.TH

application :: IO ()
application = runSplice (appE (varE (mkName "negate")) (litE (integerL (-7))))

text :: IO ()
text = runSplice (tupE [litE (stringL "h\233llo"), litE (charL 'x'), conE (mkName "Data.Maybe.Nothing")])
