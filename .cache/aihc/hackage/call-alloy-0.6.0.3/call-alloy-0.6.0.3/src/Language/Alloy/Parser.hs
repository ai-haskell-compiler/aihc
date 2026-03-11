{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-|
Module      : Language.Alloy.Parser
Description : A generic Alloy instance parser for Call Alloy library
Copyright   : (c) Marcellus Siegburg, 2019
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module allows for parsing and converting instances into Haskell data
structures.
Basically all modules are parsed into a Map of Map of Set allowing easy lookup
of every returned set and relation.
-}
module Language.Alloy.Parser (
  parseInstance,
  ) where

import qualified Data.ByteString.Char8            as BS (putStrLn, writeFile)
import qualified Data.Set                         as S (fromList)
import qualified Data.Map                         as M
  (alter, empty, insert, singleton)

import Control.Applicative              ((<|>))
import Control.Monad                    (void)
import Control.Monad.Catch              (MonadThrow (throwM))
import Control.Monad.IO.Class           (MonadIO (liftIO))
import Data.ByteString                  (ByteString)
import Data.Functor                     (($>))
import Data.List                        (intercalate)
import Data.List.Extra                  (unsnoc)
import Data.Maybe                       (fromJust)
import Data.Set                         (Set)
import Text.Trifecta

import Language.Alloy.Exceptions        (AlloyResponseFailure (..))
import Language.Alloy.Types (
  AlloyInstance,
  Annotation (..),
  Entries,
  Entry (..),
  Object (..),
  Relation (..),
  Signature (..),
  )

{-|
Parse an Alloy instance from a given String.
May fail with 'ParsingAlloyResponseFailed'.
-}
parseInstance
  :: (MonadIO m, MonadThrow m)
  => ByteString
  -> m AlloyInstance
parseInstance inst = case parseByteString alloyInstance mempty inst of
  Failure l -> do
    liftIO $ BS.putStrLn "Failed parsing Alloys response as AlloyInstance:"
    liftIO $ BS.putStrLn inst
    liftIO $ BS.writeFile "/tmp/BOOM" inst
    throwM $ ParsingAlloyResponseFailed l
  Success r -> return $ combineEntries r

combineEntries :: [Entries (,)] -> AlloyInstance
combineEntries = foldr createOrInsert M.empty
  where
    createOrInsert (s, e) = M.alter (Just . alterSig e) s
    alterSig e Nothing  = e { relation = uncurry M.singleton $ relation e}
    alterSig e (Just y) = y {
      relation = uncurry M.insert (relation e) (relation y)
      }

crlf :: Parser Char
crlf = char '\r' *> char '\n'

eof' :: Parser ()
eof' = optional (char '\r') *> eof

endOfLine :: Parser Char
endOfLine = newline <|> crlf

alloyInstance :: Parser [Entries (,)]
alloyInstance =
  stateZeroLoop <|> stateZero
  where
    stateZeroLoop =
      string "------State 0 (loop)-------"
      *> endOfLine
      *> entries
    stateZero = (++)
      <$> (
        many entry
        <* optional (string "------State 0-------" <* endOfLine)
        )
      <*> entries
    entries =
      many (try entry)
      <* many alloyInfo
      <* eof'
    alloyInfo =
      string "c last restart ## conflicts  :  "
      *> many digit
      *> space
      *> many digit
      *> space
      *> (void (try endOfLine) <|> eof')

entry :: Parser (Entries (,))
entry = do
  entryAnnotation <- try (string "skolem " $> Just Skolem) <|> pure Nothing
  entrySignature <- sig
  (entrySignature,) . Entry entryAnnotation <$> ((,)
              <$> ((string "<:" *> word) <|> pure "")
              <*> parseRelations <* (void (try endOfLine) <|> eof'))

sig :: Parser Signature
sig = do
  xs' <- slashedWord
  return $ case fromJust $ unsnoc xs' of
    ([], x) -> Signature Nothing x
    (xs, x) -> Signature (Just $ intercalate "/" $ take (length xs') xs) x

parseRelations :: Parser (Relation Set)
parseRelations = char '='
  *> (try (string "{}" $> EmptyRelation)
      <|> (Id <$> try object)
      <|> fmap Triple (try $ sep tripleRel)
      <|> fmap Double (try $ sep doubleRel)
      <|> fmap Single (sep singleRel))
  where
    sep rel = S.fromList
      <$> between (char '{') (char '}') (rel `sepBy` string ", ")
    tripleRel = (,,) <$> nextObject <*> nextObject <*> object
    doubleRel = (,) <$> nextObject <*> object
    singleRel = object
    nextObject = object <* string "->"

object :: Parser Object
object =
  try (
    Object . intercalate "/"
    <$> slashedWord
    <* char '$'
    <*> (read <$> some digit)
    )
  <|> try (NumberObject <$> int)
  <|> NamedObject <$> word

int :: Parser Int
int = fmap read $ (++)
  <$> (try (string "-") <|> pure "")
  <*> some digit

slashedWord :: Parser [String]
slashedWord = word `sepBy1` char '/'

word :: Parser String
word = (:)
  <$> (letter <|> char '$')
  <*> many (letter <|> digit <|> char '_' <|> char '\'')
