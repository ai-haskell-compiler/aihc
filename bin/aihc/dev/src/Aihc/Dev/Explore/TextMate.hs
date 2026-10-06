{-# LANGUAGE TemplateHaskell #-}

-- | A small interpreter for the TextMate grammars of the AIHC intermediate
-- languages in @editors/grammars/syntaxes@.
--
-- The interpreter supports the part of the TextMate format that the grammars
-- use: @match@ rules, @begin@ and @end@ rules, capture names, nested
-- patterns, and includes from the repository. PCRE runs the regular
-- expressions. Its syntax is near to the Oniguruma syntax of the grammars.
module Aihc.Dev.Explore.TextMate
  ( Grammar,
    Token (..),
    fcGrammar,
    grinGrammar,
    lirGrammar,
    tokenizeLines,
  )
where

import Control.Monad (forM)
import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.FileEmbed (embedFile)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import System.IO.Unsafe (unsafePerformIO)
import Text.Regex.PCRE.Wrap (Regex, compUTF8, execBlank, wrapCompile, wrapMatch)

-- | A token of a line: its text and its scopes, from the outermost to the
-- innermost.
data Token = Token
  { tokenText :: !Text,
    tokenScopes :: ![Text]
  }
  deriving (Eq, Show)

data Grammar = Grammar
  { grammarPatterns :: ![Rule],
    grammarRepository :: !(Map Text Rule)
  }

data Rule
  = RuleMatch !Pattern !(Maybe Text) !Captures
  | RuleBeginEnd !BeginEnd
  | RuleInclude !Text
  | RuleGroup ![Rule]

data BeginEnd = BeginEnd
  { beginPattern :: !Pattern,
    endPattern :: !Pattern,
    beginName :: !(Maybe Text),
    beginContentName :: !(Maybe Text),
    beginCaptures :: !Captures,
    endCaptures :: !Captures,
    beginPatterns :: ![Rule]
  }

-- | The scope name of each capture group, by group number.
type Captures = Map Int Text

-- | A compiled regular expression. A pattern that PCRE does not accept
-- never matches.
newtype Pattern = Pattern (Maybe Regex)

fcGrammar :: Grammar
fcGrammar = parseGrammar $(embedFile "../../editors/grammars/syntaxes/fc.tmLanguage.json")
{-# NOINLINE fcGrammar #-}

grinGrammar :: Grammar
grinGrammar = parseGrammar $(embedFile "../../editors/grammars/syntaxes/grin.tmLanguage.json")
{-# NOINLINE grinGrammar #-}

lirGrammar :: Grammar
lirGrammar = parseGrammar $(embedFile "../../editors/grammars/syntaxes/lir.tmLanguage.json")
{-# NOINLINE lirGrammar #-}

-- | Read a grammar. A grammar that cannot be read has no rules, so its text
-- shows without colors.
parseGrammar :: ByteString -> Grammar
parseGrammar bytes =
  case Aeson.decodeStrict bytes of
    Just (Object object) ->
      Grammar
        { grammarPatterns = rulesOf object,
          grammarRepository =
            case KeyMap.lookup "repository" object of
              Just (Object repository) -> Map.fromList [(Key.toText key, ruleOf value) | (key, value) <- KeyMap.toList repository]
              _ -> Map.empty
        }
    _ -> Grammar [] Map.empty

rulesOf :: Aeson.Object -> [Rule]
rulesOf object =
  case KeyMap.lookup "patterns" object of
    Just (Array values) -> map ruleOf (foldr (:) [] values)
    _ -> []

ruleOf :: Value -> Rule
ruleOf value =
  case value of
    Object object
      | Just (String target) <- KeyMap.lookup "include" object -> RuleInclude target
      | Just (String source) <- KeyMap.lookup "match" object ->
          RuleMatch (compilePattern source) (textField "name" object) (capturesField "captures" object)
      | Just (String begin) <- KeyMap.lookup "begin" object,
        Just (String end) <- KeyMap.lookup "end" object ->
          let captures = capturesField "captures" object
           in RuleBeginEnd
                BeginEnd
                  { beginPattern = compilePattern begin,
                    endPattern = compilePattern end,
                    beginName = textField "name" object,
                    beginContentName = textField "contentName" object,
                    beginCaptures = capturesField "beginCaptures" object <> captures,
                    endCaptures = capturesField "endCaptures" object <> captures,
                    beginPatterns = rulesOf object
                  }
      | otherwise -> RuleGroup (rulesOf object)
    _ -> RuleGroup []

textField :: Text -> Aeson.Object -> Maybe Text
textField key object =
  case KeyMap.lookup (Key.fromText key) object of
    Just (String text) -> Just text
    _ -> Nothing

capturesField :: Text -> Aeson.Object -> Captures
capturesField key object =
  case KeyMap.lookup (Key.fromText key) object of
    Just (Object captures) ->
      Map.fromList
        [ (number, name)
        | (group, Object capture) <- KeyMap.toList captures,
          Just number <- [readInt (Key.toText group)],
          Just name <- [textField "name" capture]
        ]
    _ -> Map.empty
  where
    readInt text = case reads (T.unpack text) of
      [(number, "")] -> Just number
      _ -> Nothing

-- | Compile a pattern once. The grammars are constant, so the compile is
-- pure.
compilePattern :: Text -> Pattern
compilePattern source =
  unsafePerformIO $
    BS.useAsCString (TE.encodeUtf8 source) $ \source' -> do
      compiled <- wrapCompile compUTF8 execBlank source'
      pure (Pattern (either (const Nothing) Just compiled))
{-# NOINLINE compilePattern #-}

-- | An active @begin@ rule and the scopes of its content.
data Frame = Frame
  { frameRule :: !BeginEnd,
    frameScopes :: ![Text]
  }

-- | Tokenize the lines of a text that starts at the top level of the
-- grammar. The rules of a @begin@ that does not end on its line continue on
-- the next lines.
tokenizeLines :: Grammar -> [Text] -> [[Token]]
tokenizeLines grammar = go []
  where
    go _ [] = []
    go stack (line : rest) =
      let (tokens, stack') = tokenizeLine grammar stack line
       in tokens : go stack' rest

-- | One candidate match: its start and end byte offsets, its groups, and
-- what it does.
data Candidate = Candidate
  { candidateStart :: !Int,
    candidateEnd :: !Int,
    candidateGroups :: ![(Int, Int)],
    candidateAction :: !Action
  }

data Action
  = ActionEnd
  | ActionMatch !(Maybe Text) !Captures
  | ActionBegin !BeginEnd

tokenizeLine :: Grammar -> [Frame] -> Text -> ([Token], [Frame])
tokenizeLine grammar initialStack line =
  unsafePerformIO $
    BS.useAsCStringLen bytes $ \cstring -> do
      let search pos (Pattern compiled) =
            case compiled of
              Nothing -> pure Nothing
              Just regex -> do
                result <- wrapMatch pos regex cstring
                pure $ case result of
                  Right (Just groups@((start, end) : _)) -> Just (start, end, groups)
                  _ -> Nothing
          loop pos stack acc
            | pos > len = pure (reverse acc, stack)
            | otherwise = do
                let currentScopes = maybe baseScopes frameScopes (headMaybe stack)
                    rules = maybe (grammarPatterns grammar) (beginPatterns . frameRule) (headMaybe stack)
                    endCandidates = case stack of
                      frame : _ -> [(endPattern (frameRule frame), ActionEnd)]
                      [] -> []
                    ruleCandidates = concatMap (ruleActions grammar) rules
                found <- forM (endCandidates <> ruleCandidates) $ \(regex, action) -> do
                  matched <- search pos regex
                  pure (fmap (\(start, end, groups) -> Candidate start end groups action) matched)
                case earliest (catMaybes found) of
                  Nothing -> pure (reverse (segment pos len currentScopes : acc), stack)
                  Just candidate -> do
                    let start = candidateStart candidate
                        end = candidateEnd candidate
                        before = segment pos start currentScopes
                    case candidateAction candidate of
                      ActionEnd -> case stack of
                        frame : outer -> do
                          let outerScopes = maybe baseScopes frameScopes (headMaybe outer)
                              scopes = outerScopes <> scopeNames (beginName (frameRule frame))
                              tokens = captured scopes (endCaptures (frameRule frame)) candidate
                          -- An empty end, such as @$@, ends the rule and
                          -- leaves the rest of the line to the outer rule.
                          loop end outer (reverse tokens <> (before : acc))
                        [] -> pure (reverse (segment pos len currentScopes : acc), stack)
                      ActionMatch name captures -> do
                        let scopes = currentScopes <> scopeNames name
                            tokens = captured scopes captures candidate
                        if end == start
                          then loop (start + 1) stack (segment start (min len (start + 1)) currentScopes : before : acc)
                          else loop end stack (reverse tokens <> (before : acc))
                      ActionBegin rule -> do
                        let outerScopes = currentScopes <> scopeNames (beginName rule)
                            contentScopes = outerScopes <> scopeNames (beginContentName rule)
                            tokens = captured outerScopes (beginCaptures rule) candidate
                        -- An empty begin could start again at the same
                        -- position, so it gives one character to the
                        -- current rule instead.
                        if end == start
                          then loop (start + 1) stack (segment start (min len (start + 1)) currentScopes : before : acc)
                          else loop end (Frame rule contentScopes : stack) (reverse tokens <> (before : acc))
      (tokens, stack) <- loop 0 initialStack []
      pure (merge (filter (not . T.null . tokenText) tokens), stack)
  where
    bytes = TE.encodeUtf8 line
    len = BS.length bytes
    baseScopes = []
    segment from to scopes
      | to <= from = Token "" scopes
      | otherwise = Token (slice from to) scopes
    slice from to = TE.decodeUtf8With TE.lenientDecode (BSU.unsafeTake (to - from) (BSU.unsafeDrop from bytes))
    captured scopes captures candidate =
      paint scopes captures (candidateStart candidate) (candidateEnd candidate) (candidateGroups candidate)
    paint scopes captures start end groups =
      let named =
            [ (from, to, name)
            | (number, (from, to)) <- zip [0 ..] groups,
              from >= 0,
              to > from,
              Just name <- [Map.lookup number captures]
            ]
          points = sortOn id (foldl' (\acc (from, to, _) -> from : to : acc) [start, end] named)
          pieces = zip points (drop 1 points)
          scopesAt from = scopes <> concat [T.words name | (groupFrom, groupTo, name) <- named, groupFrom <= from, from < groupTo]
       in [segment from to (scopesAt from) | (from, to) <- pieces, from >= start, to <= end, to > from]
    earliest candidates =
      case candidates of
        [] -> Nothing
        _ -> Just (foldr1 (\candidate best -> if candidateStart candidate <= candidateStart best then candidate else best) candidates)

-- | The patterns and actions of a rule, with the includes expanded.
ruleActions :: Grammar -> Rule -> [(Pattern, Action)]
ruleActions grammar = go (0 :: Int)
  where
    go depth rule
      | depth > 32 = []
      | otherwise = case rule of
          RuleMatch regex name captures -> [(regex, ActionMatch name captures)]
          RuleBeginEnd beginEnd -> [(beginPattern beginEnd, ActionBegin beginEnd)]
          RuleGroup rules -> concatMap (go (depth + 1)) rules
          RuleInclude target
            | target == "$self" -> concatMap (go (depth + 1)) (grammarPatterns grammar)
            | Just name <- T.stripPrefix "#" target ->
                maybe [] (go (depth + 1)) (Map.lookup name (grammarRepository grammar))
            | otherwise -> []

-- | The scopes of a @name@: it can give more than one scope, separated by
-- spaces.
scopeNames :: Maybe Text -> [Text]
scopeNames = maybe [] T.words

-- | Join the neighbouring tokens that have the same scopes.
merge :: [Token] -> [Token]
merge tokens =
  case tokens of
    first : second : rest
      | tokenScopes first == tokenScopes second -> merge (Token (tokenText first <> tokenText second) (tokenScopes first) : rest)
      | otherwise -> first : merge (second : rest)
    _ -> tokens

headMaybe :: [a] -> Maybe a
headMaybe values = case values of
  value : _ -> Just value
  [] -> Nothing
