{-# LANGUAGE RebindableSyntax #-}
{- |
Convert a human readable string to a physical value.
-}

module Number.Physical.Read where

import qualified Number.Physical        as Value
import qualified Number.Physical.UnitDatabase as Db
import qualified Algebra.VectorSpace as VectorSpace
import qualified Algebra.Field       as Field
import qualified Data.Map as Map
import Data.Map (Map)
import Text.ParserCombinators.Parsec
import Control.Monad(liftM)

import NumericPrelude.Base
import NumericPrelude.Numeric

mulPrec :: Int
mulPrec = 7

-- How to handle the 'prec' argument?
readsNat :: (Enum i, Ord i, Read v, VectorSpace.C a v) =>
   Db.T i a -> Int -> ReadS (Value.T i v)
readsNat db prec =
   readParen (prec>=mulPrec)
      (map (\(x, rest) ->
             let (Value.Cons cu c, rest') = readUnitPart (createDict db) rest
             in  (Value.Cons cu (c *> x), rest'))
       .
       readsPrec mulPrec)

readUnitPart :: (Ord i, Field.C a) =>
   Map String (Value.T i a)
      -> String -> (Value.T i a, String)
readUnitPart dict str =
   let parseUnit =
          do p    <- parseProduct
             rest <- many anyChar
             return (product (map (\(unit,n) ->
                        Map.findWithDefault
                           (error ("unknown unit '" ++ unit ++ "'")) unit dict
                           ^ n) p),
                     rest)
   in  case parse parseUnit "unit" str of
          Left  msg -> error (show msg)
          Right val -> val


{-| This function could also return the value,
    but a list of pairs (String, Integer) is easier for testing. -}
parseProduct :: Parser [(String, Integer)]
parseProduct =
   skipMany space >>
      ((do p <- ignoreSpace parsePower
           t <- parseProductTail
           return (p : t)) <|>
       parseProductTail)

parseProductTail :: Parser [(String, Integer)]
parseProductTail =
   let parseTail c f = 
         do _ <- ignoreSpace (char c)
            p <- ignoreSpace parsePower
            t <- parseProductTail
            return (f p : t)
   in  parseTail '*' id <|>
       parseTail '/' (\(x,n) -> (x,-n)) <|>
       return []

parsePower :: Parser (String, Integer)
parsePower =
   do w <- ignoreSpace (many1 (letter <|> char '\181'))
      e <- liftM read (ignoreSpace (char '^') >> many1 digit) <|> return 1
      return (w,e)

{- Turns a parser into one that ignores subsequent whitespaces. -}
ignoreSpace :: Parser a -> Parser a
ignoreSpace p =
   do x <- p
      skipMany space
      return x


createDict :: Db.T i a -> Map String (Value.T i a)
createDict db =
   Map.fromList (concatMap
      (\Db.UnitSet {Db.unit = xu, Db.scales = s}
           -> map (\Db.Scale {Db.symbol = sym, Db.magnitude = x}
                       -> (sym, Value.Cons xu x)) s) db)
