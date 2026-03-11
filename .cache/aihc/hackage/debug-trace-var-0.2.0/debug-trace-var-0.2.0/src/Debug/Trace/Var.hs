{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Debug.Trace.Var
  ( traceMTH
  , traceVar
  , traceVarId
  , traceStackVar
  , traceVarIO
  , traceVarM
  ) where

import           Data.Maybe
import           Debug.Trace
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Show.Unicode

traceMTH :: Name -> Q Exp
traceMTH name = [|traceM $ $(stringE (nameBase name)) ++ $(stringE " = ") ++ ushow $(varE name)|]

traceVar :: QuasiQuoter
traceVar = traceVarBase [|trace|]

traceVarId :: QuasiQuoter
traceVarId = traceVarBase [|traceId|]

traceStackVar :: QuasiQuoter
traceStackVar = traceVarBase [|traceStack|]

traceVarIO :: QuasiQuoter
traceVarIO = traceVarBase [|traceIO|]

traceVarM :: QuasiQuoter
traceVarM = traceVarBase [|traceM|]

traceVarBase :: Q Exp -> QuasiQuoter
traceVarBase traceFuncEQ = QuasiQuoter
  { quoteExp = \str -> AppE <$> traceFuncEQ <*> traceFormat str
  , quotePat = error "no support"
  , quoteType = error "no support"
  , quoteDec = error "no support"
  }

traceFormat :: String -> Q Exp
traceFormat str = do
  valueName <- fromMaybe (error $ "variable " ++ str ++ " is not found") <$> lookupValueName str
  [| concat [$(stringE str), " = ", ushow $(varE valueName)] |]
