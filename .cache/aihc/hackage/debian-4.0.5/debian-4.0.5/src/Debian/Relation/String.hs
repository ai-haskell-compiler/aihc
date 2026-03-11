{-# LANGUAGE FlexibleInstances, PackageImports, TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-unused-do-bind -fno-warn-orphans #-}
-- |A module for working with debian relationships <http://www.debian.org/doc/debian-policy/ch-relationships.html>
module Debian.Relation.String
    ( -- * Types
      AndRelation
    , OrRelation
    , Relations
    , Relation(..)
    , ArchitectureReq(..)
    , VersionReq(..)
    -- * Helper Functions
    , checkVersionReq
    -- * Relation Parser
    , RelParser
    , ParseRelations(..)
    , pRelations
    ) where

-- Standard GHC Modules

import "mtl" Control.Monad.Identity (Identity)
import Data.Set (fromList)
import Text.ParserCombinators.Parsec
import Text.Parsec.Prim (ParsecT)

-- Local Modules

import Debian.Arch (Arch, parseArch)
import Debian.Relation.Common
import Debian.Version

-- * ParseRelations

instance ParseRelations String where
    parseRelations str =
        let str' = scrub str in
        case parse pRelations str' str' of
          Right relations -> Right (filter (/= []) relations)
          x -> x
        where
          scrub = unlines . filter (not . comment) . lines
          comment s = case dropWhile (`elem` [' ', '\t']) s of
                           ('#' : _) -> True
                           _ -> False

-- * Relation Parser

type RelParser a = CharParser () a

-- "Correct" dependency lists are separated by commas, but sometimes they
-- are omitted and it is possible to parse relations without them.
pRelations :: RelParser Relations
pRelations = do -- rel <- sepBy pOrRelation (char ',')
                rel <- many pOrRelation
                eof
                return rel

pOrRelation :: RelParser OrRelation
pOrRelation = do skipMany (char ',' <|> whiteChar)
                 rel <- sepBy1 pRelation (char '|')
                 skipMany (char ',' <|> whiteChar)
                 return rel

whiteChar :: ParsecT String u Identity Char
whiteChar = oneOf [' ','\t','\n']

pRelation :: RelParser Relation
pRelation =
    do skipMany whiteChar
       pkgName <- many1 (noneOf [' ',',','|','\t','\n','('])
       skipMany whiteChar
       mVerReq <- pMaybeVerReq
       skipMany whiteChar
       mArch <- pMaybeArch
       return $ Rel (BinPkgName pkgName) mVerReq mArch

pMaybeVerReq :: RelParser (Maybe VersionReq)
pMaybeVerReq =
    do char '('
       skipMany whiteChar
       op <- pVerReq
       skipMany whiteChar
       ver <- many1 (noneOf [' ',')','\t','\n'])
       skipMany whiteChar
       char ')'
       return $ Just (op (parseDebianVersion' ver))
    <|>
    do return $ Nothing

pVerReq :: ParsecT [Char] u Identity (DebianVersion -> VersionReq)
pVerReq =
    do char '<'
       (do char '<' <|> char ' ' <|> char '\t'
           return $ SLT
        <|>
        do char '='
           return $ LTE)
    <|>
    do string "="
       return $ EEQ
    <|>
    do char '>'
       (do char '='
           return $ GRE
        <|>
        do char '>' <|> char ' ' <|> char '\t'
           return $ SGR)

pMaybeArch :: RelParser (Maybe ArchitectureReq)
pMaybeArch =
    do char '['
       (do archs <- pArchExcept
           char ']'
           skipMany whiteChar
           return (Just (ArchExcept (fromList . map parseArchExcept $ archs)))
        <|>
        do archs <- pArchOnly
           char ']'
           skipMany whiteChar
           return (Just (ArchOnly (fromList . map parseArch $ archs)))
        )
    <|>
    return Nothing

-- Some packages (e.g. coreutils) have architecture specs like [!i386
-- !hppa], even though this doesn't really make sense: once you have
-- one !, anything else you include must also be (implicitly) a !.
pArchExcept :: RelParser [String]
pArchExcept = sepBy (char '!' >> many1 (noneOf [']',' '])) (skipMany1 whiteChar)

pArchOnly :: RelParser [String]
pArchOnly = sepBy (many1 (noneOf [']',' '])) (skipMany1 whiteChar)

-- | Ignore the ! if it is present, we already know this list has at
-- least one, and the rest are implicit.
parseArchExcept :: String -> Arch
parseArchExcept ('!' : s) = parseArch s
parseArchExcept s = parseArch s
