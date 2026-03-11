module LazyOC where

import Data.Maybe
import Language.REST.WQOConstraints.Lazy as LC
import Language.REST.Internal.OpOrdering

oo :: String -> OpOrdering
oo = fromJust . parseOO

tests :: [(String, Bool)]
tests = [
  ("intersect",
    not $ LC.isSatisfiable $ LC.addConstraint (oo "g > f")
    (LC.addConstraint (oo "f > g ^ f > s") LC.noConstraints)
  )
  ]
