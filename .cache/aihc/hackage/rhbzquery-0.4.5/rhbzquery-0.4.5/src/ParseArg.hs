-- SPDX-License-Identifier: GPL-2.0-or-later

module ParseArg (
  argHelp,
  readBzQueryArg,
  ArgType(..),
  Operator(..),
  operators,
  showOp,
  showOpHelp,
  ProductVersion(..),
  statusList
  )
where

import Control.Applicative ((<|>))
import Data.Char
import Data.List.Extra as L
import Data.Version.Extra
import Numeric.Natural

import Common

argHelp :: String
argHelp = "[COMPONENT|STATUS|PRODUCTVERSION|FIELD=VALUE|FIELDopVALUE]..."

data Operator = Equals | NotEquals
              | Substring | NotSubstring
              | Regexp |NotRegexp
              | CaseSubstring
              | AnyWordsSubstr | AllWordsSubstr | NoWordsSubstr
              | AnyExact
              | AnyWords | AllWords | NoWords
              | IsEmpty | IsNotEmpty
              | NoOp
              -- | ContentMatches | ContentNotMatch
  deriving (Eq,Show,Enum,Bounded)

showOp :: Operator -> String
showOp = lower . show

operators :: [Operator]
operators = enumFromTo minBound maxBound

data OperatorData = OpUnary String String | OpNull String String
  deriving Eq

opSyntax :: OperatorData -> String
opSyntax (OpUnary s _) = s
opSyntax (OpNull s _) = s

opDescribe :: OperatorData -> String
opDescribe (OpUnary _ h) = h
opDescribe (OpNull _ h) = h

showOpHelp :: Operator -> String
showOpHelp op =
  let opd = opData op
  in padSyntax opd ++ showOp op ++ " (" ++ opDescribe opd ++ ")"
  where
    padSyntax opd =
      let ops = opSyntax opd
      in replicate (10 - length ops) ' ' ++ "'" ++ ops ++ "' : "

instance Ord OperatorData where
  compare op1 op2 =
    let sop1 = opSyntax op1
        sop2 = opSyntax op2
    in
      if sop1 `isInfixOf` sop2 then GT
      else if sop2 `isInfixOf` sop1 then LT
           else if sop1 == sop2
                then error' $ sop1 ++ " listed twice in 'operators'"
                else compare (reverse sop1) (reverse sop2)

-- FIXME check all unique
opData :: Operator -> OperatorData
opData NoOp = OpNull "~noop~" "ignore (comment out query field)"
opData Equals = OpUnary "=" "is equal to"
opData NotEquals = OpUnary "!=" "is not equal to"
opData AnyExact = OpUnary "~anyexact=" "is equal to any of the strings"
opData Substring = OpUnary "~" "contains the string"
opData NotSubstring = OpUnary "!~" "does not contain the string"
opData CaseSubstring = OpUnary "~case~" "contains the string (exact case)"
opData AnyWordsSubstr = OpUnary "~any~" "contains any of the strings"
opData AllWordsSubstr = OpUnary "~all~" "contains all of the strings"
opData NoWordsSubstr = OpUnary "~no~" "contains none of the strings"
opData Regexp = OpUnary "=~" "matches regular expression"
opData NotRegexp = OpUnary "!=~" "does not match regular expression"
opData AnyWords = OpUnary "~anywords~" "contains any of the words"
opData AllWords = OpUnary "~allwords~" "contains all of the words"
opData NoWords = OpUnary "~nowords~" "contains none of the words"
opData IsEmpty = OpNull "~empty~" "is empty"
opData IsNotEmpty = OpNull "~notempty~" "is not empty"
-- -- only for 'content':
-- opData Matches = OpUnary "~m~"
-- opData NotMatches = OpUnary "!m~"

data ArgType = ArgProdVer ProductVersion
             | ArgStatusAll
             | ArgStatusBefore String
             | ArgStatusAfter String
             -- FIXME NonEmpty String
             | ArgParameter String Operator String
             | ArgParameterEmpty String Bool
             | ArgComponent String

readBzQueryArg :: String -> Maybe ArgType
readBzQueryArg s =
  ArgProdVer <$>
  readProductVersion s <|>
  parseStatus s <|>
  parseField s <|>
  parseComponent s

data ProductVersion = Fedora (Maybe Natural)
                    | Rawhide
                    | EPEL (Maybe Natural)
                    | RHEL Version

readProductVersion :: String -> Maybe ProductVersion
readProductVersion "fedora" = Just (Fedora Nothing)
readProductVersion "rawhide" = Just Rawhide
readProductVersion ('f':ver) | all isDigit ver = Just $ Fedora (Just (read ver :: Natural))
readProductVersion "epel" = Just (EPEL Nothing)
readProductVersion ('e':'p':'e':'l':v) | all isDigit v = Just (EPEL (Just (read v :: Natural)))
readProductVersion ('r':'h':'e':'l':ver) = Just $ RHEL (readVersion ver)
readProductVersion _ = Nothing

statusList :: [String]
statusList = ["NEW", "ASSIGNED", "POST", "MODIFIED", "ON_QA", "VERIFIED", "RELEASE_PENDING", "CLOSED"]

parseStatus :: String -> Maybe ArgType
parseStatus s =
  case upper s of
    "" -> error' "malformed status string"
    "ALL" -> Just ArgStatusAll
    ('<':st) -> ArgStatusBefore <$> find (== st) statusList
    ('>':st) -> ArgStatusAfter <$> find (== st) statusList
    caps -> ArgParameter "bug_status" Equals <$> find (== caps) statusList

parseField :: String -> Maybe ArgType
parseField ps =
  foldr ((<|>) . parseParam) Nothing $ sortOn opData operators
  where
    parseParam :: Operator -> Maybe ArgType
    parseParam oper =
      case opData oper of
        OpUnary op _ ->
          if op `isInfixOf` ps then
            case splitOn op ps of
              (f:val) ->
                if null f
                then error' $ "cannot have empty parameter before " ++ show ps
                else Just (ArgParameter f oper (intercalate op val))
              _ -> Nothing
          else Nothing
        OpNull op _ ->
          case stripSuffix op ps of
            Just a ->
              if null a
              then error' $ "bad parameter: " ++ ps
              else Just (ArgParameterEmpty a (oper == IsEmpty))
            _ -> Nothing

-- https://fedoraproject.org/wiki/Packaging:Naming?rd=Packaging:NamingGuidelines#Common_Character_Set_for_Package_Naming
-- abcdefghijklmnopqrstuvwxyz
-- ABCDEFGHIJKLMNOPQRSTUVWXYZ
-- 0123456789-._+
parseComponent :: String -> Maybe ArgType
parseComponent ps =
  if all isPackageChar ps
  then Just (ArgComponent ps)
  else error' $ "Invalid component name: " ++ ps
  where
    isPackageChar :: Char -> Bool
    isPackageChar c =
      -- Bugzilla components can contain a space though
      isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` " -._+"
