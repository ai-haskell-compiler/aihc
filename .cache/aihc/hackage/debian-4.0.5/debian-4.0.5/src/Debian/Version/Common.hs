-- |A module for parsing, comparing, and (eventually) modifying debian version
-- numbers. <http://www.debian.org/doc/debian-policy/ch-controlfields.html#s-f-Version>
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -fno-warn-orphans -fno-warn-unused-do-bind #-}
module Debian.Version.Common
    ( DebianVersion -- |Exported abstract because the internal representation is likely to change
    , prettyDebianVersion
    , ParseDebianVersion(..)
    , parseDebianVersion'
    , evr               -- DebianVersion -> (Maybe Int, String, Maybe String)
    , epoch
    , version
    , revision
    , buildDebianVersion
    , parseDV
    ) where

import Data.Char (ord, isDigit, isAlpha)
import Debian.Pretty (PP(..))
import Debian.Version.Internal
import Text.ParserCombinators.Parsec
import Text.Regex
import Text.PrettyPrint (Doc, render, text)
import Distribution.Pretty (Pretty(pretty))

prettyDebianVersion :: DebianVersion -> Doc
prettyDebianVersion (DebianVersion s _) = text s

instance Pretty (PP DebianVersion) where
    pretty = prettyDebianVersion . unPP

instance Eq DebianVersion where
    (DebianVersion _ v1) == (DebianVersion _ v2) = v1 == v2

instance Ord DebianVersion where
    compare (DebianVersion _ v1) (DebianVersion _ v2) = compare v1 v2

instance Show DebianVersion where
    show v = "(Debian.Version.parseDebianVersion (" ++ show (render (prettyDebianVersion v)) ++ " :: String))"

-- make ~ less than everything, and everything else higher that letters
order :: Char -> Int
order c
    | isDigit c = 0
    | isAlpha c = ord c
    | c == '~' = -1
    | otherwise = (ord c) + 256

-- |We have to do this wackiness because ~ is less than the empty string
compareNonNumeric :: [Char] -> [Char] -> Ordering
compareNonNumeric "" "" = EQ
compareNonNumeric "" ('~':_cs) = GT
compareNonNumeric ('~':_cs) "" = LT
compareNonNumeric "" _ = LT
compareNonNumeric _ "" = GT
compareNonNumeric (c1:cs1) (c2:cs2) =
    if (order c1) == (order c2)
       then compareNonNumeric cs1 cs2
       else compare (order c1) (order c2)

instance Eq NonNumeric where
    (NonNumeric s1 n1) == (NonNumeric s2 n2) =
        case compareNonNumeric s1 s2 of
          EQ -> n1 == n2
          _o -> False

instance Ord NonNumeric where
    compare (NonNumeric s1 n1) (NonNumeric s2 n2) =
        case compareNonNumeric s1 s2 of
          EQ -> compare n1 n2
          o -> o

instance Eq Numeric where
    (Numeric n1 mnn1) == (Numeric n2 mnn2) =
        case compare n1 n2 of
          EQ -> case compareMaybeNonNumeric mnn1 mnn2 of
                  EQ -> True
                  _ -> False
          _ -> False

compareMaybeNonNumeric :: Maybe NonNumeric -> Maybe NonNumeric -> Ordering
compareMaybeNonNumeric mnn1 mnn2 =
    case (mnn1, mnn2) of
      (Nothing, Nothing) -> EQ
      (Just (NonNumeric nn _), Nothing) -> compareNonNumeric nn ""
      (Nothing, Just (NonNumeric nn _)) -> compareNonNumeric "" nn
      (Just nn1, Just nn2) -> compare nn1 nn2

instance Ord Numeric where
    compare (Numeric n1 mnn1) (Numeric n2 mnn2) =
        case compare n1 n2 of
          EQ -> compareMaybeNonNumeric mnn1 mnn2
          o -> o

-- * Parser

class ParseDebianVersion a where
    parseDebianVersion :: a-> Either ParseError DebianVersion
-- |Convert a string to a debian version number. May throw an
-- exception if the string is unparsable -- but I am not sure if that
-- can currently happen. Are there any invalid version strings?
-- Perhaps ones with underscore, or something?

parseDebianVersion' :: ParseDebianVersion string => string -> DebianVersion
parseDebianVersion' str = either (\e -> error (show e)) id (parseDebianVersion str)

{-
showNN :: NonNumeric -> String
showNN (NonNumeric s n) = s ++ showN n

showN :: Found Numeric -> String
showN (Found (Numeric n nn)) = show n ++ maybe "" showNN nn
showN (Simulated _) = ""
-}

parseDV :: CharParser () (Found Int, NonNumeric, Found NonNumeric)
parseDV =
    do skipMany $ oneOf " \t"
       e <- parseEpoch
       upstreamVersion <- parseNonNumeric True True
       debianRevision <- option (Simulated (NonNumeric "" (Simulated (Numeric 0 Nothing)))) (char '-' >> parseNonNumeric True False >>= return . Found)
       return (e, upstreamVersion, debianRevision)

parseEpoch :: CharParser () (Found Int)
parseEpoch =
    option (Simulated 0) (try (many1 digit >>= \d -> char ':' >> return (Found (read d))))


parseNonNumeric :: Bool -> Bool -> CharParser () NonNumeric
parseNonNumeric zeroOk upstream =
    do nn <- (if zeroOk then many else many1) ((noneOf "-0123456789") <|> (if upstream then upstreamDash else pzero))
       n <- parseNumeric upstream
       return $ NonNumeric nn n
    where
      upstreamDash :: CharParser () Char
      upstreamDash = try $ do char '-'
                              lookAhead $ (many (noneOf "- \n\t") >> char '-')
                              return '-'

parseNumeric :: Bool -> CharParser () (Found Numeric)
parseNumeric upstream =
    do n <- many1 (satisfy isDigit)
       nn <- option Nothing  (parseNonNumeric False upstream >>= return . Just)
       return $ Found (Numeric (read n) nn)
    <|>
    return (Simulated (Numeric 0 Nothing))

{-
compareTest :: String -> String -> Ordering
compareTest str1 str2 =
    let v1 = either (error . show) id $ parse parseDV str1 str1
        v2 = either (error . show) id $ parse parseDV str2 str2
        in
          compare v1 v2
-}

-- |Split a DebianVersion into its three components: epoch, version,
-- revision.  It is not safe to use the parsed version number for
-- this because you will lose information, such as leading zeros.
evr :: DebianVersion -> (Maybe Int, String, Maybe String)
evr (DebianVersion s _) =
    let re = mkRegex "^(([0-9]+):)?(([^-]*)|((.*)-([^-]*)))$" in
    --                 (         ) (        (            ))
    --                  (   e  )    (  v  )  (v2) (  r  )
    case matchRegex re s of
      Just ["", _, _, v, "", _, _] -> (Nothing, v, Nothing)
      Just ["", _, _, _, _,  v, r] -> (Nothing, v, Just r)
      Just [_,  e, _, v, "", _, _] -> (Just (read e), v, Nothing)
      Just [_,  e, _, _, _,  v, r] -> (Just (read e), v, Just r)
      -- I really don't think this can happen.
      _ -> error ("Invalid Debian Version String: " ++ s)

epoch :: DebianVersion -> Maybe Int
epoch v = case evr v of (x, _, _) -> x
version :: DebianVersion -> String
version v = case evr v of (_, x, _) -> x
revision :: DebianVersion -> Maybe String
revision v = case evr v of (_, _, x) -> x

-- Build a Debian version number from epoch, version, revision
buildDebianVersion :: Maybe Int -> String -> Maybe String -> DebianVersion
buildDebianVersion e v r =
    either (error . show) (DebianVersion str) $ parse parseDV str str
    where
      str = (maybe "" (\ n -> show n ++ ":") e ++ v ++ maybe "" (\ s -> "-" ++ s) r)
