{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Use foldr" #-}

module Data.Version
  ( Version (Version),
    versionBranch,
    versionTags,
    showVersion,
    parseVersion,
    makeVersion,
  )
where

import Data.Char (isAlphaNum, isDigit, ord)
import GHC.Read ()
import Text.ParserCombinators.ReadP
  ( ReadP,
    readS_to_P,
  )
import Prelude

data Version = Version [Int] [String]

versionBranch :: Version -> [Int]
versionBranch (Version branch _) = branch

versionTags :: Version -> [String]
versionTags (Version _ tags) = tags

instance Eq Version where
  left == right =
    versionBranch left
      == versionBranch right
      && equalTagBags (versionTags left) (versionTags right)

  left /= right = not (left == right)

instance Ord Version where
  compare left right = compare (versionBranch left) (versionBranch right)
  left < right = versionBranch left < versionBranch right
  left <= right = versionBranch left <= versionBranch right
  left > right = versionBranch left > versionBranch right
  left >= right = versionBranch left >= versionBranch right
  max left right =
    case compare left right of
      GT -> left
      _ -> right
  min left right =
    case compare left right of
      GT -> right
      _ -> left

instance Show Version where
  showsPrec precedence (Version branch tags) =
    showParen
      (precedence > 10)
      ( showString "Version {versionBranch = "
          . shows branch
          . showString ", versionTags = "
          . shows tags
          . showChar '}'
      )

instance Read Version where
  readsPrec precedence = readParen (precedence > 10) readVersionRecord
  readList = readVersionList

readVersionRecord :: ReadS Version
readVersionRecord input =
  bindVersionRead (matchLexeme "Version" input) afterVersion
  where
    afterVersion _ rest = bindVersionRead (matchLexeme "{" rest) afterOpen
    afterOpen _ rest = bindVersionRead (matchLexeme "versionBranch" rest) afterBranchName
    afterBranchName _ rest = bindVersionRead (matchLexeme "=" rest) afterBranchEquals
    afterBranchEquals _ rest = bindVersionRead (reads rest) afterBranch
    afterBranch branch rest = bindVersionRead (matchLexeme "," rest) (afterComma branch)
    afterComma branch _ rest = bindVersionRead (matchLexeme "versionTags" rest) (afterTagsName branch)
    afterTagsName branch _ rest = bindVersionRead (matchLexeme "=" rest) (afterTagsEquals branch)
    afterTagsEquals branch _ rest = bindVersionRead (reads rest) (afterTags branch)
    afterTags branch tags rest = bindVersionRead (matchLexeme "}" rest) (afterClose branch tags)
    afterClose branch tags _ rest = [(Version branch tags, rest)]

readVersionList :: ReadS [Version]
readVersionList input =
  bindVersionRead (matchLexeme "[" input) afterOpen
  where
    afterOpen _ rest =
      case matchLexeme "]" rest of
        (_, remaining) : _ -> [([], remaining)]
        [] -> bindVersionRead (reads rest) afterValue
    afterValue value rest = bindVersionRead (readVersionListTail rest) (afterTail value)
    afterTail value values rest = [(value : values, rest)]

readVersionListTail :: ReadS [Version]
readVersionListTail input =
  case matchLexeme "]" input of
    (_, rest) : _ -> [([], rest)]
    [] -> bindVersionRead (matchLexeme "," input) afterComma
  where
    afterComma _ rest = bindVersionRead (reads rest) afterValue
    afterValue value rest = bindVersionRead (readVersionListTail rest) (afterTail value)
    afterTail value values rest = [(value : values, rest)]

matchLexeme :: String -> ReadS String
matchLexeme expected input =
  case lex input of
    (actual, rest) : _ ->
      case equalStrings actual expected of
        True -> [(actual, rest)]
        False -> []
    [] -> []

bindVersionRead :: [(a, String)] -> (a -> String -> [(b, String)]) -> [(b, String)]
bindVersionRead [] _ = []
bindVersionRead ((value, rest) : results) next =
  next value rest ++ bindVersionRead results next

showVersion :: Version -> String
showVersion (Version branch tags) =
  showBranch branch ++ showTags tags

showBranch :: [Int] -> String
showBranch [] = []
showBranch (component : components) = show component ++ showBranchTail components

showBranchTail :: [Int] -> String
showBranchTail [] = []
showBranchTail (component : components) = '.' : show component ++ showBranchTail components

showTags :: [String] -> String
showTags [] = []
showTags (tag : tags) = '-' : tag ++ showTags tags

parseVersion :: ReadP Version
parseVersion = readS_to_P parseVersionString

parseVersionString :: ReadS Version
parseVersionString input =
  case spanVersion isDigit input of
    ([], _) -> []
    (digits, rest) -> parseBranch [versionDigitsToInt 0 digits] rest

parseBranch :: [Int] -> ReadS Version
parseBranch branch input =
  (Version branch [], input)
    : case input of
      '.' : rest ->
        case spanVersion isDigit rest of
          ([], _) -> []
          (digits, remaining) ->
            parseBranch (appendVersionList branch [versionDigitsToInt 0 digits]) remaining
      _ -> parseTags branch [] input

parseTags :: [Int] -> [String] -> ReadS Version
parseTags branch tags input =
  case input of
    '-' : rest ->
      case spanVersion isAlphaNum rest of
        ([], _) -> []
        (tag, remaining) ->
          let nextTags = appendVersionList tags [tag]
           in (Version branch nextTags, remaining) : parseTags branch nextTags remaining
    _ -> []

spanVersion :: (Char -> Bool) -> String -> (String, String)
spanVersion _ [] = ([], [])
spanVersion predicate input@(value : values) =
  case predicate value of
    False -> ([], input)
    True ->
      case spanVersion predicate values of
        (matched, rest) -> (value : matched, rest)

versionDigitsToInt :: Int -> String -> Int
versionDigitsToInt value [] = value
versionDigitsToInt value (digit : digits) =
  versionDigitsToInt (value * 10 + ord digit - ord '0') digits

appendVersionList :: [a] -> [a] -> [a]
appendVersionList [] suffix = suffix
appendVersionList (value : values) suffix = value : appendVersionList values suffix

makeVersion :: [Int] -> Version
makeVersion branch = Version branch []

equalTagBags :: [String] -> [String] -> Bool
equalTagBags [] [] = True
equalTagBags [] (_ : _) = False
equalTagBags (_ : _) [] = False
equalTagBags (tag : tags) candidates =
  case removeTag tag candidates of
    Nothing -> False
    Just remaining -> equalTagBags tags remaining

removeTag :: String -> [String] -> Maybe [String]
removeTag _ [] = Nothing
removeTag tag (candidate : candidates) =
  case equalStrings tag candidate of
    True -> Just candidates
    False ->
      case removeTag tag candidates of
        Nothing -> Nothing
        Just remaining -> Just (candidate : remaining)

equalStrings :: String -> String -> Bool
equalStrings [] [] = True
equalStrings [] (_ : _) = False
equalStrings (_ : _) [] = False
equalStrings (left : lefts) (right : rights) =
  left == right && equalStrings lefts rights
