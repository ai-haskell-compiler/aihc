module Text.XML.WraXML.Tree.LazyParser where

import qualified Text.ParserCombinators.Poly.Lazy as Parser
import Text.ParserCombinators.Poly.Lazy (Parser, )

import Control.Monad (liftM2, )


force :: Parser t a -> Parser t a
force = Parser.apply (return id)

{-
Like Parser.manyFinally,
but a matching terminator parser has precedence
to a successful element parser.
-}
manyFinallyAppend :: Parser t [a] -> a ->
   Parser t a -> Parser t [a]
manyFinallyAppend terminator eofWarn p =
   let recourse = force $
          terminator `Parser.onFail`
          liftM2 (:) p recourse `Parser.onFail`
          return [eofWarn]
   in  recourse

manyLazy :: Parser t a -> Parser t [a]
manyLazy p =
   let recourse = force $ liftM2 (:) p recourse `Parser.onFail` return []
   in  recourse
