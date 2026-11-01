-- | Read the text interfaces that the GHC executable produces.
module Aihc.Dev.ExtractHi.Ghc
  ( GhcInterface (..),
    readGhcInterface,
    readGhcBuiltins,
    readGhcModule,
    unqualify,
  )
where

import Aihc.Dev.ExtractHi.Types
import Control.Monad (unless)
import Data.Char (isAlphaNum, isHexDigit, isUpper)
import Data.List (nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Exit (ExitCode (..))
import System.Process (readProcess, readProcessWithExitCode)
import Text.Read (readMaybe)

data GhcInterface = GhcInterface
  { ghcExports :: [(Text, [Text])],
    ghcDeclarations :: Map Text Text,
    ghcFixities :: Map Text FixityInfo
  }

-- | Keep module prefixes here. The export references need these prefixes.
readGhcInterface :: FilePath -> IO GhcInterface
readGhcInterface path = do
  output <- T.pack <$> readProcess "ghc" (printOptions <> ["--show-iface", path]) ""
  let ls = T.lines output
  unless ("exports:" `elem` ls) (ioError (userError ("GHC interface has no export section: " <> path)))
  let -- A type and its pattern can have the same text reference.
      -- Keep the type entry, which follows the term entries.
      exports = reverse (nubBy (\first second -> fst first == fst second) (reverse (map parseExport (section "exports:" ls))))
      blocks = hashBlocks ls
      declarations = Map.fromListWith preferType (mapMaybe (declaration . map (T.drop 2) . takeWhile (T.isPrefixOf "  ")) blocks)
      fixities = case dropWhile (not . T.isPrefixOf "fixities ") ls of
        line : _ -> mapMaybe parseFixity (T.splitOn ", " (T.drop 9 line))
        [] -> []
  pure (GhcInterface exports declarations (Map.fromList [(fiName fixity, fixity) | fixity <- fixities]))
  where
    section heading = map T.strip . takeWhile (T.isPrefixOf "  ") . drop 1 . dropWhile (/= heading)
    parseExport line =
      let (name, members) = T.breakOn "{" line
       in (name, splitNames (T.dropEnd 1 (T.drop 1 members)))
    hashBlocks [] = []
    hashBlocks (line : rest)
      | T.length line == 32 && T.all isHexDigit line =
          let (block, remaining) = span (T.isPrefixOf "  ") rest
           in block : hashBlocks remaining
      | otherwise = hashBlocks rest

splitNames :: Text -> [Text]
splitNames = map T.pack . go 0 [] . T.unpack
  where
    go :: Int -> String -> String -> [String]
    go _ word [] = [reverse word | not (null word)]
    go depth word (char : rest)
      | char == ' ' && depth == 0 = [reverse word | not (null word)] <> go depth [] rest
      | otherwise = go (depth + if char == '(' then 1 else if char == ')' then -1 else 0) (char : word) rest

printOptions :: [String]
printOptions =
  [ "-XNoStarIsType",
    "-XMagicHash",
    "-XLinearTypes",
    "-package-env",
    "-",
    "-XUnboxedTuples",
    "-XUnboxedSums",
    "-fprint-redundant-promotion-ticks",
    "-fprint-explicit-runtime-reps",
    "-dppr-cols=1000000"
  ]

-- | Built-in declarations have no interface file. GHCi can print them.
readGhcBuiltins :: IO GhcInterface
readGhcBuiltins = do
  output <- runGhcQuery (unlines [":browse GHC.Prim", ":quit"])
  let primitiveDeclarations = mapMaybe declaration (topLevelBlocks (T.lines output))
      declarations = Map.fromList primitiveDeclarations
      values = ["GHC.Prim." <> T.unpack name | (name, raw) <- primitiveDeclarations, not ("type " `T.isPrefixOf` raw)]
  info <- runGhcQuery (unlines [":info " <> unwords values, ":quit"])
  -- GHCi also prints the default fixity. The report counts explicit fixities.
  let fixities = filter (\fixity -> fiDirection fixity /= InfixL || fiPrecedence fixity /= 9) (mapMaybe parseFixity (T.lines info))
  pure (GhcInterface [(name, []) | (name, _) <- primitiveDeclarations] declarations (Map.fromList [(fiName fixity, fixity) | fixity <- fixities]))

readGhcModule :: String -> IO GhcInterface
readGhcModule name = do
  output <- runGhcQuery (unlines [":browse " <> name, ":quit"])
  pure (GhcInterface [] (Map.fromListWith preferType (mapMaybe declaration (topLevelBlocks (T.lines output)))) Map.empty)

runGhcQuery :: String -> IO Text
runGhcQuery commands = do
  (code, output, errors) <- readProcessWithExitCode "ghc" (printOptions <> ["-dsuppress-module-prefixes", "--interactive", "-ignore-dot-ghci", "-v0"]) commands
  case code of
    ExitSuccess | null errors -> pure (T.pack output)
    _ -> ioError (userError ("GHC interface query failed: " <> errors))

-- | A kind signature and its declaration form one block.
topLevelBlocks :: [Text] -> [[Text]]
topLevelBlocks = combine . blocks
  where
    blocks [] = []
    blocks (line : rest) =
      let (body, remaining) = span (\text -> T.null text || T.isPrefixOf " " text) rest
       in (line : body) : blocks remaining
    combine (kind : body : rest)
      | isKind kind = (kind <> body) : combine rest
      | otherwise = kind : combine (body : rest)
    combine remaining = remaining
    isKind (line : _) = "type " `T.isPrefixOf` line && " :: " `T.isInfixOf` line && not ("type role " `T.isPrefixOf` line)
    isKind [] = False

declaration :: [Text] -> Maybe (Text, Text)
declaration input = do
  let ls = dropWhile (T.isPrefixOf "type role ") input
  first <- case ls of
    line : _ -> Just line
    [] -> Nothing
  let (lhs, signature) = T.breakOn " :: " first
      name = stripParens (unqualify (T.dropWhileEnd (== ' ') (dropKeyword lhs)))
      body = T.unlines (takeWhile (\line -> not ("[" `T.isPrefixOf` line || "  C type:" `T.isPrefixOf` line)) ls)
  if T.null signature || T.null name || "instance " `T.isPrefixOf` first
    then Nothing
    else Just (name, stripQualifiers (T.stripEnd body))
  where
    dropKeyword text = case T.stripPrefix "type " text of
      Just rest -> rest
      Nothing -> fromMaybe text (T.stripPrefix "pattern " text)
    stripParens text = fromMaybe text (T.stripPrefix "(" text >>= T.stripSuffix ")")

preferType :: Text -> Text -> Text
preferType first second = if "type " `T.isPrefixOf` first then first else second

parseFixity :: Text -> Maybe FixityInfo
parseFixity line = case T.words line of
  [direction, precedence, name] -> do
    assoc <- case direction of
      "infixl" -> Just InfixL
      "infixr" -> Just InfixR
      "infix" -> Just InfixN
      _ -> Nothing
    level <- readMaybe (T.unpack precedence)
    pure (FixityInfo (T.dropAround (== '`') (unqualify name)) assoc level)
  _ -> Nothing

-- | Remove only module prefixes. Keep dots in operators and string literals.
stripQualifiers :: Text -> Text
stripQualifiers = T.pack . go . T.unpack
  where
    go [] = []
    go ('"' : rest) = '"' : quoted rest
    go input@(char : rest)
      | isUpper char =
          let (part, remaining) = span (\c -> isAlphaNum c || c == '_' || c == '\'') input
           in case remaining of
                '.' : suffix -> go suffix
                _ -> part <> go remaining
      | otherwise = char : go rest
    quoted [] = []
    quoted ('\\' : char : rest) = '\\' : char : quoted rest
    quoted ('"' : rest) = '"' : go rest
    quoted (char : rest) = char : quoted rest

unqualify :: Text -> Text
unqualify = stripQualifiers
