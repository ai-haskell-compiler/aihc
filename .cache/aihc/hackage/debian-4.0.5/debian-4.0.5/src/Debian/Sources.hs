{-# LANGUAGE FlexibleInstances, OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Debian.Sources
    {- ( SourceType(..)
    , SourceOption(..)
    , SourceOp(..)
    , DebSource(..)
    , parseSourceLine
    , parseSourceLine'
    , parseSourcesList
    ) -} where

import Control.Lens (makeLenses, review, view)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import Debian.Codename (Codename, codename, parseCodename)
import Debian.Pretty (PP(..))
import Debian.Release
import Debian.TH (here, Loc)
import Debian.VendorURI (parseVendorURI, VendorURI, vendorURI)
import Network.URI (parseURI, unEscapeString, escapeURIString, isAllowedInURI)
import Test.HUnit
import Text.ParserCombinators.Parsec
import Text.PrettyPrint (hcat, punctuate, render, text)
import Distribution.Pretty (Pretty(pretty), prettyShow)

data SourceType
    = Deb | DebSrc
    deriving (Eq, Ord, Show)

-- arch
-- lang
-- target
-- pdiffs
-- by-hash
-- allow-insecure=no
-- allow-weak=no
-- allow-downgrade-to-insecure=no
-- trusted=no
-- signed-by
-- check-valid-until
-- valid-until-min
-- valid-until-max
data SourceOption
    = SourceOption String SourceOp [String]
    deriving (Eq, Ord, Show)

data SourceOp = OpSet | OpAdd | OpDel deriving (Eq, Ord, Show)

instance Pretty SourceOp where
    pretty OpSet = text "="
    pretty OpAdd = text "+="
    pretty OpDel = text "-="

data DebSource
    = DebSource
    { _sourceType :: SourceType
    , _sourceOptions :: [SourceOption]
    , _sourceUri :: VendorURI
    , _sourceDist :: Either String (Codename, [Section])
    } deriving (Eq, Ord, Show)

instance Pretty SourceType where
    pretty Deb = text "deb"
    pretty DebSrc = text "deb-src"

instance Pretty SourceOption where
    pretty (SourceOption k op vs) = text k <> pretty op <> hcat (punctuate (text ",") (map text vs))

instance Pretty DebSource where
    pretty (DebSource thetype theoptions theuri thedist) =
        hcat (punctuate (text " ")
                ([pretty thetype] ++
                 (case theoptions of
                    [] -> []
                    _ -> [text "[" <> hcat (punctuate (text ", ") (map pretty theoptions)) <> text "]"]) ++
                 [text (show (view vendorURI theuri))] ++
                 case thedist of
                   Left exactPath -> [text (escapeURIString isAllowedInURI exactPath)]
                   Right (dist, sections) ->
                       map text (codename dist : map sectionName' sections)))

instance Pretty (PP [DebSource]) where
    pretty = hcat . map (\ x -> pretty x <> text "\n") . unPP

{-

deb uri distribution [component1] [componenent2] [...]

The URI for the deb type must specify the base of the Debian
distribution, from which APT will find the information it needs.

distribution can specify an exact path, in which case the components
must be omitted and distribution must end with a slash (/).

If distribution does not specify an exact path, at least one component
must be present.

Distribution may also contain a variable, $(ARCH), which expands to
the Debian architecture (i386, m68k, powerpc, ...)  used on the
system.

The rest of the line can be marked as a comment by using a #.

Additional Notes:

 + Lines can begin with leading white space.

 + If the dist ends with slash (/), then it must be an absolute path
   and it is an error to specify components after it.

-}

-- |quoteWords - similar to words, but with special handling of
-- double-quotes and brackets.
--
-- The handling double quotes and [] is supposed to match:
-- apt-0.6.44.2\/apt-pkg\/contrib\/strutl.cc:ParseQuoteWord()
--
-- The behaviour can be defined as:
--
--  Break the string into space seperated words ignoring spaces that
--  appear between \"\" or []. Strip trailing and leading white space
--  around words. Strip out double quotes, but leave the square
--  brackets intact.
quoteWords :: String -> [String]
quoteWords [] = []
quoteWords s = quoteWords' (dropWhile (==' ') s)
    where
      quoteWords' :: String -> [String]
      quoteWords' [] = []
      quoteWords' str =
          case break (flip elem (" [\"" :: String)) str of
            ([],[]) -> []
            (w, []) -> [w]
            (w, (' ':rest)) -> w : (quoteWords' (dropWhile (==' ') rest))
            (w, ('"':rest)) ->
                case break (== '"') rest of
                  (w',('"':rest)) ->
                      case quoteWords' rest of
                        [] ->  [w ++ w']
                        (w'':ws) -> ((w ++ w' ++ w''): ws)
                  (_w',[]) -> error ("quoteWords: missing \" in the string: "  ++ s)
                  _ -> error ("the impossible happened in SourcesList.quoteWords")
            (w, ('[':rest)) ->
                case break (== ']') rest of
                  (w',(']':rest)) ->
                      case quoteWords' rest of
                        []       -> [w ++ "[" ++ w' ++ "]"]
                        (w'':ws) -> ((w ++ "[" ++ w' ++ "]" ++ w''): ws)
                  (_w',[]) -> error ("quoteWords: missing ] in the string: "  ++ s)
                  _ -> error ("the impossible happened in SourcesList.quoteWords")
            _ -> error ("the impossible happened in SourcesList.quoteWords")

stripLine :: String -> String
stripLine = takeWhile (/= '#') . dropWhile (== ' ')

sourceLines :: String -> [String]
sourceLines = filter (not . null) . map stripLine . lines

-- |parseSourceLine -- parses a source line
-- the argument must be a non-empty, valid source line with comments stripped
-- see: 'sourceLines'
parseSourceLine :: [Loc] -> String -> DebSource
parseSourceLine locs str = either error id (parseSourceLine' locs str)
{-
    case quoteWords str of
      (theTypeStr : theUriStr : theDistStr : sectionStrs) ->
          let sections = map parseSection' sectionStrs
              theType = case unEscapeString theTypeStr of
                          "deb" -> Deb
                          "deb-src" -> DebSrc
                          o -> error ("parseSourceLine: invalid type " ++ o ++ " in line:\n" ++ str)
              theUri = case parseURI theUriStr of
                         Nothing -> error ("parseSourceLine: invalid uri " ++ theUriStr ++ " in the line:\n" ++ str)
                         Just u -> u
              theDist = unEscapeString theDistStr
          in
            case last theDist of
              '/' -> if null sections
                      then DebSource { sourceType = theType, sourceOptions = [], sourceUri = theUri, sourceDist = Left theDist }
                      else error ("parseSourceLine: Dist is an exact path, so sections are not allowed on the line:\n" ++ str)
              _ -> if null sections
                    then error ("parseSourceLine: Dist is not an exact path, so at least one section is required on the line:\n" ++ str)
                    else DebSource { sourceType = theType, sourceOptions = [], sourceUri = theUri, sourceDist = Right (parseReleaseName theDist, sections) }
      _ -> error ("parseSourceLine: invalid line in sources.list:\n" ++ str)
-}

parseOptions :: String -> Either ParseError [SourceOption]
parseOptions s = parse pOptions s s

pOptions :: CharParser () [SourceOption]
pOptions = do _ <- char '['
              skipMany (oneOf [' ','\t'])
              opts <- sepBy1 pOption (char ',')
              skipMany (oneOf [' ','\t'])
              _ <- char ']'
              return opts

pOption :: CharParser () SourceOption
pOption = do skipMany (oneOf [' ','\t'])
             key <- many1 (noneOf ['+','-','=',' ','\t'])
             skipMany (oneOf [' ','\t'])
             op <- pOp
             skipMany (oneOf [' ','\t'])
             values <- sepBy1 (many1 (noneOf [',',']',' ','\t'])) (char ',')
             skipMany (oneOf [' ','\t'])
             return $ SourceOption key op values

pOp :: CharParser () SourceOp
pOp = do (char '+' >> char '=' >> return OpAdd)
         <|>
         (char '-' >> char '=' >> return OpDel)
         <|>
         (char '=' >> return OpSet)

parseSourceLine' :: [Loc] -> String -> Either String DebSource
parseSourceLine' locs str =
    case quoteWords str of
      theTypeStr : theOptionStr@('[' : _) : theURIStr : theDistStr : sectionStrs ->
          either
            (Left . show)
            (\opts -> go theTypeStr opts theURIStr theDistStr sectionStrs)
            (parseOptions theOptionStr)
      theTypeStr : theURIStr : theDistStr : sectionStrs ->
          go theTypeStr [] theURIStr theDistStr sectionStrs
      _ -> Left ("parseSourceLine: invalid line in sources.list:\n" ++ str)
    where
      go :: String -> [SourceOption] -> String -> String -> [String] -> Either String DebSource
      go theTypeStr theOptions theURIStr theDistStr sectionStrs =
          let sections = map parseSection' sectionStrs
              theType = case unEscapeString theTypeStr of
                          "deb" -> Right Deb
                          "deb-src" -> Right DebSrc
                          s -> Left ("parseSourceLine" ++ prettyShow ($here : locs) ++ ": invalid type " ++ s ++ " in line:\n" ++ str ++ " str=" ++ show str)
              theURI = case parseVendorURI ($here : locs) theURIStr of
                         Nothing -> Left ("parseSourceLine' " ++ prettyShow ($here : locs) ++ ": invalid uri " ++ theURIStr ++ " str=" ++ show str)
                         Just u -> Right u
              theDist = unEscapeString theDistStr
          in
            case (last theDist, theType, theURI) of
              ('/', Right typ, Right uri) -> if null sections
                      then Right $ DebSource { _sourceType = typ, _sourceOptions = theOptions, _sourceUri = uri, _sourceDist = Left theDist }
                      else Left ("parseSourceLine: Dist is an exact path, so sections are not allowed on the line:\n" ++ str)
              (_, Right typ, Right uri) -> if null sections
                    then Left ("parseSourceLine: Dist is not an exact path, so at least one section is required on the line:\n" ++ str)
                    else Right $ DebSource { _sourceType = typ, _sourceOptions = theOptions, _sourceUri = uri, _sourceDist = Right ((parseCodename theDist), sections) }
              (_, Left msg, _) -> Left msg
              (_, _, Left msg) -> Left msg

parseSourcesList :: [Loc] -> String -> [DebSource]
parseSourcesList locs = map (parseSourceLine locs) . sourceLines

-- * Unit Tests

-- TODO: add test cases that test for unterminated double-quote or bracket
testQuoteWords :: Test
testQuoteWords =
    test [ assertEqual "Space seperate words, no quoting" ["hello", "world","!"] (quoteWords "  hello    world !  ")
         , assertEqual "Space seperate words, double quotes" ["hello  world","!"] (quoteWords "  hel\"lo  world\" !  ")
         , assertEqual "Space seperate words, square brackets" ["hel[lo  worl]d","!"] (quoteWords "  hel[lo  worl]d ! ")
         , assertEqual "Space seperate words, square-bracket at end" ["hel[lo world]"] (quoteWords " hel[lo world]")
         , assertEqual "Space seperate words, double quote at end" ["hello world"] (quoteWords " hel\"lo world\"")
         , assertEqual "Space seperate words, square-bracket at beginning" ["[hello wo]rld","!"] (quoteWords "[hello wo]rld !")
         , assertEqual "Space seperate words, double quote at beginning" ["hello world","!"] (quoteWords "\"hello wor\"ld !")
         ]

testSourcesList :: Test
testSourcesList =
    test [ assertEqual "parse and pretty sources.list" validSourcesListExpected (render . pretty . PP . parseSourcesList [$here] $ validSourcesListStr) ]

testSourcesList2 :: Test
testSourcesList2 =
    test [ assertEqual "pretty sources.list" validSourcesListExpected (render . pretty . PP $ validSourcesList) ]

validSourcesListStr :: String
validSourcesListStr =
          unlines $ [ " # A comment only line "
                    , " deb ftp://ftp.debian.org/debian unstable main contrib non-free # typical deb line"
                    , " deb-src ftp://ftp.debian.org/debian unstable main contrib non-free # typical deb-src line"
                    , ""
                    , "# comment line"
                    , "deb http://pkg-kde.alioth.debian.org/kde-3.5.0/ ./ # exact path"
                    , "deb [trusted=yes] http://ftp.debian.org/whee \"space dist\" main"
                    , "deb [trusted=yes] http://ftp.debian.org/whee dist space%20section"
                    ]

validSourcesList :: [DebSource]
validSourcesList =
    [DebSource {_sourceType = Deb, _sourceOptions = [], _sourceUri = (review vendorURI . fromJust) (parseURI "ftp://ftp.debian.org/debian"), _sourceDist = Right (parseCodename "unstable",[Section "main",Section "contrib",Section "non-free"])},
     DebSource {_sourceType = DebSrc, _sourceOptions = [], _sourceUri = (review vendorURI . fromJust) (parseURI "ftp://ftp.debian.org/debian"), _sourceDist = Right (parseCodename "unstable",[Section "main",Section "contrib",Section "non-free"])},
     DebSource {_sourceType = Deb, _sourceOptions = [], _sourceUri = (review vendorURI . fromJust) (parseURI "http://pkg-kde.alioth.debian.org/kde-3.5.0/"), _sourceDist = Left "./"},
     DebSource {_sourceType = Deb, _sourceOptions = [SourceOption "trusted" OpSet ["yes"]], _sourceUri = (review vendorURI . fromJust) (parseURI "http://ftp.debian.org/whee"), _sourceDist = Right (parseCodename "space dist",[Section "main"])},
     DebSource {_sourceType = Deb, _sourceOptions = [SourceOption "trusted" OpSet ["yes"]], _sourceUri = (review vendorURI . fromJust) (parseURI "http://ftp.debian.org/whee"), _sourceDist = Right (parseCodename "dist",[Section "space section"])}]

validSourcesListExpected :: String
validSourcesListExpected =
          unlines $ [ "deb ftp://ftp.debian.org/debian unstable main contrib non-free"
                    , "deb-src ftp://ftp.debian.org/debian unstable main contrib non-free"
                    , "deb http://pkg-kde.alioth.debian.org/kde-3.5.0/ ./"
                    , "deb [trusted=yes] http://ftp.debian.org/whee space%20dist main"
                    , "deb [trusted=yes] http://ftp.debian.org/whee dist space%20section"
                    ]
_invalidSourcesListStr1 :: Text
_invalidSourcesListStr1 = "deb http://pkg-kde.alioth.debian.org/kde-3.5.0/ ./ main contrib non-free # exact path with sections"

testSourcesListParse :: Test
testSourcesListParse =
    test [ assertEqual "" gutsy (concat . map (<> "\n") . map (render . pretty) . parseSourcesList [$here] $ gutsy) ]
    where
      gutsy = concat ["deb http://us.archive.ubuntu.com/ubuntu/ gutsy main restricted universe multiverse\n",
                      "deb-src http://us.archive.ubuntu.com/ubuntu/ gutsy main restricted universe multiverse\n",
                      "deb http://us.archive.ubuntu.com/ubuntu/ gutsy-updates main restricted universe multiverse\n",
                      "deb-src http://us.archive.ubuntu.com/ubuntu/ gutsy-updates main restricted universe multiverse\n",
                      "deb http://us.archive.ubuntu.com/ubuntu/ gutsy-backports main restricted universe multiverse\n",
                      "deb-src http://us.archive.ubuntu.com/ubuntu/ gutsy-backports main restricted universe multiverse\n",
                      "deb http://security.ubuntu.com/ubuntu/ gutsy-security main restricted universe multiverse\n",
                      "deb-src http://security.ubuntu.com/ubuntu/ gutsy-security main restricted universe multiverse\n"]

sourcesListTests :: Test
sourcesListTests =
    TestList [ testQuoteWords, testSourcesList, testSourcesList2, testSourcesListParse ]

$(makeLenses ''DebSource)
