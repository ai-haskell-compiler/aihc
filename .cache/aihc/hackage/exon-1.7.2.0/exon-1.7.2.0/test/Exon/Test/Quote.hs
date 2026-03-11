module Exon.Test.Quote where

import Language.Haskell.TH.Quote (quoteExp, QuasiQuoter)
import Language.Haskell.TH (Q, Exp, runQ)
import Language.Haskell.TH.Ppr (pprint)

thLines :: Q Exp -> IO [Text]
thLines q =
  lines . toText <$> runQ (pprint <$> q)

qqLines :: QuasiQuoter -> String -> IO [Text]
qqLines qq code =
  thLines (quoteExp qq code)

runQuote :: QuasiQuoter -> String -> IO Exp
runQuote qq code =
  runQ (quoteExp qq code)
