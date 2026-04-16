{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskellQuotes #-}

module THQ_All_Quote_Forms where

import Language.Haskell.TH (Dec, Name, Pat, Type)
import Language.Haskell.TH.Syntax (Code, Exp, Q)

exprSplice :: Q Exp
exprSplice = $x

typedExprSplice :: Code Q Int
typedExprSplice = $$x

exprQuote :: Q Exp
exprQuote = [|expr|]

typedExprQuote :: Code Q Int
typedExprQuote = [||expr||]

declQuote :: Q [Dec]
declQuote = [d|quotedDecl = 1|]

typeQuote :: Q Type
typeQuote = [t|Maybe Int|]

patQuote :: Q Pat
patQuote = [p|Just patName|]

nameQuote :: Name
nameQuote = 'id

typeNameQuote :: Name
typeNameQuote = ''Maybe

$(topLevelExprSplice)

$$(topLevelTypedExprSplice)
