-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Expr
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Author      :  derek.a.elkins@gmail.com
-- Ported by   :  Sebastiaan Visser <haskell@fvisser.nl>
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-- A helper module to parse \"expressions\".
-- Builds a parser given a table of operators and associativities.
-- 
-----------------------------------------------------------------------------

module Data.Attoparsec.Expr
    ( Assoc(..), Operator(..), OperatorTable
    , buildExpressionParser
    ) where

import Control.Applicative
import Data.Monoid
import Data.Attoparsec.Combinator
import Data.Attoparsec.Types

-----------------------------------------------------------
-- Assoc and OperatorTable
-----------------------------------------------------------

-- |  This data type specifies the associativity of operators: left, right
-- or none.

data Assoc                = AssocNone
                          | AssocLeft
                          | AssocRight

-- | This data type specifies operators that work on values of type @a@.
-- An operator is either binary infix or unary prefix or postfix. A
-- binary operator has also an associated associativity.

data Operator     t a   = Infix (Parser t (a -> a -> a)) Assoc
                        | Prefix (Parser t (a -> a))
                        | Postfix (Parser t (a -> a))

-- | An @OperatorTable@ is a list of @Operator@
-- lists. The list is ordered in descending
-- precedence. All operators in one list have the same precedence (but
-- may have a different associativity).

type OperatorTable t a = [[Operator t a]]

-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------

-- | @buildExpressionParser table term@ builds an expression parser for
-- terms @term@ with operators from @table@, taking the associativity
-- and precedence specified in @table@ into account. Prefix and postfix
-- operators of the same precedence can only occur once (i.e. @--2@ is
-- not allowed if @-@ is prefix negate). Prefix and postfix operators
-- of the same precedence associate to the left (i.e. if @++@ is
-- postfix increment, than @-2++@ equals @-1@, not @-3@).
--
-- The @buildExpressionParser@ takes care of all the complexity
-- involved in building expression parser. Here is an example of an
-- expression parser that handles prefix signs, postfix increment and
-- basic arithmetic.
--
-- >  expr    = buildExpressionParser table term
-- >          <?> "expression"
-- >
-- >  term    =  parens expr 
-- >          <|> natural
-- >          <?> "simple expression"
-- >
-- >  table   = [ [prefix "-" negate, prefix "+" id ]
-- >            , [postfix "++" (+1)]
-- >            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
-- >            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
-- >            ]
-- >          
-- >  binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
-- >  prefix  name fun       = Prefix (do{ reservedOp name; return fun })
-- >  postfix name fun       = Postfix (do{ reservedOp name; return fun })

buildExpressionParser :: Monoid t => [[Operator t b]] -> Parser t b -> Parser t b
buildExpressionParser operators simpleExpr
    = foldl makeParser simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc
               ,prefix,postfix)      = foldr splitOp ([],[],[],[],[]) ops

              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix
              postfixOp  = choice postfix

              ambigious assoc op= do{ _ <- op; fail ("ambiguous use of a " ++ assoc
                                                 ++ " associative operator")
                                    }

              ambigiousRight    = ambigious "right" rassocOp
              ambigiousLeft     = ambigious "left" lassocOp
              ambigiousNon      = ambigious "non" nassocOp

              termP      = do{ pre  <- prefixP
                             ; x    <- term
                             ; post <- postfixP
                             ; return (post (pre x))
                             }

              postfixP   = postfixOp <|> return id

              prefixP    = prefixOp <|> return id

              rassocP x  = do{ f <- rassocOp
                             ; y  <- do{ z <- termP; rassocP1 z }
                             ; return (f x y)
                             }
                           <|> ambigiousLeft
                           <|> ambigiousNon
                           -- <|> return x

              rassocP1 x = rassocP x  <|> return x

              lassocP x  = do{ f <- lassocOp
                             ; y <- termP
                             ; lassocP1 (f x y)
                             }
                           <|> ambigiousRight
                           <|> ambigiousNon
                           -- <|> return x

              lassocP1 x = lassocP x <|> return x

              nassocP x  = do{ f <- nassocOp
                             ; y <- termP
                             ;    ambigiousRight
                              <|> ambigiousLeft
                              <|> ambigiousNon
                              <|> return (f x y)
                             }
                           -- <|> return x

           in  do{ x <- termP
                 ; rassocP x <|> lassocP  x <|> nassocP x <|> return x
                 }


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)
