module Text.HTML.Tagchup.Parser.Combinator (
   Parser.T, Full, Emitting, Fallible, Plain,
   char, voidChar, dropSpaces, getPos,
   many, many1, manyS, many1S, manyNull, many1Null, many0toN, many1toN,
   many1Satisfy, manySatisfy, readUntil,
   satisfy, string, voidString,
   emit, modifyEmission,
   eval, run, write, gets,
   withDefault, allowFail, allowEmit,
   Identity, runIdentity, )
  where


import qualified Text.XML.Basic.Position as Position
import qualified Text.HTML.Tagchup.Parser.Status as Status
import qualified Text.HTML.Tagchup.Parser.Stream as Stream

import qualified Text.HTML.Tagchup.Parser.Core as Parser
import Text.HTML.Tagchup.Parser.Core hiding (run, )

import Control.Monad.Trans.State (StateT(..), evalStateT, )
import Control.Monad (liftM, liftM2, guard, )

import Data.Monoid (Monoid)

import Data.Char (isSpace)



type Full     input w = Parser.T input [w] Maybe
type Fallible input   = Parser.T input ()  Maybe
type Emitting input w = Parser.T input [w] Identity
type Plain    input   = Parser.T input ()  Identity


write :: Monad fail =>
   FilePath -> Parser.T input output fail () -> input -> fail output
write fileName p =
   liftM (\ ~(_,_,ws) -> ws) .
   Parser.run p .
   Status.Cons (Position.initialize fileName)

run :: Monad fail =>
   FilePath -> Parser.T input output fail a -> input -> fail (a, output)
run fileName p =
   liftM (\ ~(a,_,ws) -> (a,ws)) .
   Parser.run p .
   Status.Cons (Position.initialize fileName)

eval ::  Monad fail =>
   FilePath -> StateT (Status.T input) fail a -> input -> fail a
eval fileName p =
   evalStateT p .
   Status.Cons (Position.initialize fileName)



getPos ::
   (Monoid output, Monad fail) =>
   Parser.T input output fail Position.T
getPos = gets Status.sourcePos

satisfy ::
   (Monoid output, Stream.C input) =>
   (Char -> Bool) -> Parser.T input output Maybe Char
satisfy p =
   do c <- nextChar
      if p c
        then return c
        else fail "character not matched"

-- | does never fail
many :: Monoid output =>
   Parser.T input output Maybe a -> Parser.T input output Identity [a]
many x =
   {- It is better to have 'force' at the place it is,
      instead of writing it to the recursive call,
      because 'x' can cause an infinite loop. -}
   withDefault (many1 x) (return [])

-- | fails when trying the sub-parser the first time or never
many1 :: Monoid output =>
   Parser.T input output Maybe a -> Parser.T input output Maybe [a]
many1 x = liftM2 (:) x (allowFail $ many x)


-- | does never fail
manyS ::
   StateT s Maybe a -> StateT s Identity [a]
manyS x =
   withDefault' (many1S x) (return [])

-- | fails when trying the sub-parser the first time or never
many1S ::
   StateT s Maybe a -> StateT s Maybe [a]
many1S x = liftM2 (:) x (allowFail' $ manyS x)


manyNull :: Monoid output =>
   Parser.T input output Maybe () -> Parser.T input output Identity ()
manyNull x =
   withDefault (many1Null x) (return ())

many1Null :: Monoid output =>
   Parser.T input output Maybe () -> Parser.T input output Maybe ()
many1Null x = x >> (allowFail $ manyNull x)


many0toN :: Monoid output =>
   Int -> Parser.T input output Maybe a -> Parser.T input output Identity [a]
many0toN n x =
   withDefault (guard (n>0) >> many1toN n x) (return [])

-- | condition: n>0, this will not be checked
many1toN :: Monoid output =>
   Int -> Parser.T input output Maybe a -> Parser.T input output Maybe [a]
many1toN n x = liftM2 (:) x (allowFail $ many0toN (pred n) x)


manySatisfy ::
   (Monoid output, Stream.C input) =>
   (Char -> Bool) -> Parser.T input output Identity String
manySatisfy =
   allowEmit . many . satisfy

many1Satisfy ::
   (Monoid output, Stream.C input) =>
   (Char -> Bool) -> Parser.T input output Maybe String
many1Satisfy =
   allowEmit . many1 . satisfy

dropSpaces ::
   (Monoid output, Stream.C input) =>
   Parser.T input output Identity ()
dropSpaces =
   manySatisfy isSpace >> return ()


char ::
   (Monoid output, Stream.C input) =>
   Char -> Parser.T input output Maybe Char
char c = satisfy (c==)

string ::
   (Monoid output, Stream.C input) =>
   String -> Parser.T input output Maybe String
string = allowEmit . mapM char


voidChar ::
   (Monoid output, Stream.C input) =>
   Char -> Parser.T input output Maybe ()
voidChar c = fmap (const ()) $ char c

voidString ::
   (Monoid output, Stream.C input) =>
   String -> Parser.T input output Maybe ()
voidString = allowEmit . mapM_ voidChar


readUntil ::
   (Monoid output, Stream.C input) =>
   String -> Parser.T input output Identity (Bool,String)
readUntil pattern =
   let recourse =
          foldr withDefault (return (False,[])) $
          liftM (const (True,[])) (mapM char pattern) :
          (do c <- nextChar
              ~(found,str) <- allowFail recourse
              return (found,c:str)) :
          []
   in  allowEmit recourse
{-
runStateT (readUntil "-->") (Position.initialize "input", "<!-- comment --> other stuff")
-}



emit :: Monad fail =>
   w -> Parser.T input [w] fail ()
emit w = tell [w]

modifyEmission ::
   (Monad fail, Monoid output) =>
   (output -> output) -> Parser.T input output fail a -> Parser.T input output fail a
modifyEmission f = censor f
