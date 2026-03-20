{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Parser.Lexer
-- Description : Lex Haskell source into span-annotated tokens, then apply extensions and layout.
--
-- This module performs the pre-parse tokenization step for Haskell source code.
-- It turns raw text into 'LexToken's that preserve:
--
-- * a semantic token classification ('LexTokenKind')
-- * the original token text ('lexTokenText')
-- * source location information ('lexTokenSpan')
--
-- The lexer runs in three phases:
--
-- 1. /Raw tokenization/ with a custom incremental scanner that consumes one or more
--    input chunks and emits tokens lazily.
-- 2. /Extension rewrites/ ('applyExtensions'), for example @NegativeLiterals@ which
--    folds @-@ plus an adjacent numeric literal into one literal token.
-- 3. /Layout insertion/ ('applyLayoutTokens') that inserts virtual @{@, @;@ and @}@
--    according to indentation (the offside rule), so the parser can treat implicit
--    layout like explicit braces and semicolons.
--
-- Scanning is incremental and error-tolerant:
--
-- * token production starts as soon as enough input is available
-- * malformed lexemes produce 'TkError' tokens instead of aborting lexing
-- * @# ...@, @#line ...@, @{-# LINE #-}@ and @{-# COLUMN #-}@ are handled in-band by
--   the lexer and update subsequent token spans without being exposed as normal tokens
--
-- Layout-sensitive syntax is the tricky part. The implementation tracks a stack of
-- layout contexts and mirrors the @haskell-src-exts@ model summarized in
-- @docs/hse-indentation-layout.md@:
--
-- * after layout-introducing keywords (currently @do@ and @of@, plus optional module
--   body layout), mark a pending implicit block
-- * if the next token is an explicit @{@, disable implicit insertion for that block
-- * otherwise, open an implicit layout context at the next token column
-- * at beginning-of-line tokens, dedent emits virtual @}@, equal-indent emits virtual
--   @;@ (with a small suppression rule for @then@/@else@)
--
-- Keyword classification is intentionally lexical and exact. 'lexIdentifier'
-- produces a keyword token /only/ when the full identifier text exactly matches a
-- reserved word in 'keywordTokenKind'. That means:
--
-- * @where@ becomes 'TkKeywordWhere'
-- * @where'@, @_where@, and @M.where@ remain identifiers
--
-- In other words, use keyword tokens only for exact reserved lexemes; contextual
-- validity is left to the parser.
module Parser.Lexer
  ( LexToken (..),
    LexTokenKind (..),
    isReservedIdentifier,
    readModuleHeaderExtensions,
    readModuleHeaderExtensionsFromChunks,
    lexTokensFromChunks,
    lexModuleTokensFromChunks,
    lexTokensWithExtensions,
    lexModuleTokensWithExtensions,
    lexTokens,
    lexModuleTokens,
  )
where

import Data.Char (digitToInt, isAlphaNum, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, isOctDigit, isSpace)
import qualified Data.List as List
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (readHex, readInt, readOct)
import Parser.Ast

data LexTokenKind
  = TkKeywordModule
  | TkKeywordWhere
  | TkKeywordDo
  | TkKeywordData
  | TkKeywordImport
  | TkKeywordQualified
  | TkKeywordAs
  | TkKeywordHiding
  | TkKeywordCase
  | TkKeywordOf
  | TkKeywordLet
  | TkKeywordIn
  | TkKeywordIf
  | TkKeywordThen
  | TkKeywordElse
  | TkPragmaLanguage [ExtensionSetting]
  | TkPragmaWarning Text
  | TkPragmaDeprecated Text
  | TkIdentifier Text
  | TkOperator Text
  | TkInteger Integer
  | TkIntegerBase Integer Text
  | TkFloat Double Text
  | TkChar Char
  | TkString Text
  | TkSymbol Text
  | TkQuasiQuote Text Text
  | TkError Text
  deriving (Eq, Ord, Show, Read)

data LexToken = LexToken
  { lexTokenKind :: !LexTokenKind,
    lexTokenText :: !Text,
    lexTokenSpan :: !SourceSpan
  }
  deriving (Eq, Ord, Show)

data LexerState = LexerState
  { lexerInput :: String,
    lexerLine :: !Int,
    lexerCol :: !Int,
    lexerAtLineStart :: !Bool,
    lexerPending :: [LexToken]
  }
  deriving (Eq, Show)

data LayoutContext
  = LayoutExplicit
  | LayoutImplicit !Int
  deriving (Eq, Show)

data ModuleLayoutMode
  = ModuleLayoutOff
  | ModuleLayoutSeekStart
  | ModuleLayoutAwaitWhere
  | ModuleLayoutDone
  deriving (Eq, Show)

data LayoutState = LayoutState
  { layoutContexts :: [LayoutContext],
    layoutPendingLayout :: !Bool,
    layoutPrevLine :: !(Maybe Int),
    layoutModuleMode :: !ModuleLayoutMode
  }
  deriving (Eq, Show)

data DirectiveUpdate = DirectiveUpdate
  { directiveLine :: !(Maybe Int),
    directiveCol :: !(Maybe Int)
  }
  deriving (Eq, Show)

-- | Convenience lexer entrypoint: no extensions, parse as expression/declaration stream.
--
-- This variant consumes a single strict 'Text' chunk and returns a lazy list of
-- tokens. Lexing errors are preserved as 'TkError' tokens instead of causing
-- lexing to fail.
lexTokens :: Text -> [LexToken]
lexTokens = lexTokensFromChunks . (: [])

-- | Convenience lexer entrypoint for full modules: no explicit extension list.
--
-- Leading header pragmas are scanned first so module-enabled extensions can be
-- applied before token rewrites and top-level layout insertion.
lexModuleTokens :: Text -> [LexToken]
lexModuleTokens input =
  lexModuleTokensFromChunks
    (enabledExtensionsFromSettings (readModuleHeaderExtensionsFromChunks [input]))
    [input]

-- | Lex an expression/declaration stream from one or more input chunks.
--
-- Tokens are produced lazily, so downstream consumers can begin parsing before
-- the full source has been scanned.
lexTokensFromChunks :: [Text] -> [LexToken]
lexTokensFromChunks = lexTokensFromChunksWithExtensions []

-- | Lex a full module from one or more input chunks with explicit extensions.
--
-- This variant enables module-body layout insertion in addition to the normal
-- token scan and extension rewrites.
lexModuleTokensFromChunks :: [Extension] -> [Text] -> [LexToken]
lexModuleTokensFromChunks = lexChunksWithExtensions True

-- | Lex source text using explicit lexer extensions.
--
-- This runs raw tokenization, extension rewrites, and implicit-layout insertion.
-- Module-top layout is /not/ enabled here. The result currently cannot fail:
-- malformed lexemes become 'TkError' tokens in the token stream.
lexTokensWithExtensions :: [Extension] -> Text -> Either String [LexToken]
lexTokensWithExtensions exts input = Right (lexTokensFromChunksWithExtensions exts [input])

-- | Lex module source text using explicit lexer extensions.
--
-- Like 'lexTokensWithExtensions', but also enables top-level module-body layout:
-- when the source omits explicit braces, virtual layout tokens are inserted
-- after @module ... where@ (or from the first non-pragma token in module-less files).
lexModuleTokensWithExtensions :: [Extension] -> Text -> Either String [LexToken]
lexModuleTokensWithExtensions exts input = Right (lexModuleTokensFromChunks exts [input])

-- | Internal chunked lexer entrypoint for non-module inputs.
--
-- This exists so callers can stream input through the same scanner while still
-- selecting extension-driven token rewrites.
lexTokensFromChunksWithExtensions :: [Extension] -> [Text] -> [LexToken]
lexTokensFromChunksWithExtensions = lexChunksWithExtensions False

-- | Run the full lexer pipeline over chunked input.
--
-- The scanner operates over the concatenated chunk stream, then the resulting token
-- stream is rewritten for enabled extensions and finally passed through the layout
-- insertion step.
lexChunksWithExtensions :: Bool -> [Extension] -> [Text] -> [LexToken]
lexChunksWithExtensions enableModuleLayout exts chunks =
  applyLayoutTokens enableModuleLayout (applyExtensions exts (scanTokens initialLexerState))
  where
    initialLexerState =
      LexerState
        { lexerInput = concatMap T.unpack chunks,
          lexerLine = 1,
          lexerCol = 1,
          lexerAtLineStart = True,
          lexerPending = []
        }

-- | Read leading module-header pragmas and return parsed LANGUAGE settings.
--
-- This scans only the pragma/header prefix (allowing whitespace and comments)
-- and stops at the first non-pragma token or lexer error token.
readModuleHeaderExtensions :: Text -> [ExtensionSetting]
readModuleHeaderExtensions input = readModuleHeaderExtensionsFromChunks [input]

-- | Read leading module-header pragmas from one or more input chunks.
--
-- This scans only the pragma/header prefix (allowing whitespace and comments)
-- and stops at the first non-pragma token or lexer error token.
readModuleHeaderExtensionsFromChunks :: [Text] -> [ExtensionSetting]
readModuleHeaderExtensionsFromChunks chunks = go (lexTokensFromChunks chunks)
  where
    go toks =
      case toks of
        LexToken {lexTokenKind = TkPragmaLanguage settings} : rest -> settings <> go rest
        LexToken {lexTokenKind = TkPragmaWarning _} : rest -> go rest
        LexToken {lexTokenKind = TkPragmaDeprecated _} : rest -> go rest
        LexToken {lexTokenKind = TkError _} : _ -> []
        _ -> []

enabledExtensionsFromSettings :: [ExtensionSetting] -> [Extension]
enabledExtensionsFromSettings = List.foldl' apply []
  where
    apply exts setting =
      case setting of
        EnableExtension ext
          | ext `elem` exts -> exts
          | otherwise -> exts <> [ext]
        DisableExtension ext -> filter (/= ext) exts

-- | Produce the lazy stream of raw lexical tokens.
--
-- Pending synthetic tokens are emitted first, then trivia is skipped, and finally
-- the next real token is scanned from the remaining input.
scanTokens :: LexerState -> [LexToken]
scanTokens st0 =
  case lexerPending st0 of
    tok : rest -> tok : scanTokens st0 {lexerPending = rest}
    [] ->
      let st = skipTrivia st0
       in case lexerPending st of
            tok : rest -> tok : scanTokens st {lexerPending = rest}
            []
              | null (lexerInput st) -> []
              | otherwise ->
                  let (tok, st') = nextToken st
                   in tok : scanTokens st'

-- | Skip ignorable trivia until the next token boundary.
--
-- Control directives are treated specially: valid directives update lexer position
-- state without emitting a token, while malformed directives enqueue 'TkError'
-- tokens for later emission.
skipTrivia :: LexerState -> LexerState
skipTrivia st = maybe st skipTrivia (consumeTrivia st)

consumeTrivia :: LexerState -> Maybe LexerState
consumeTrivia st
  | null (lexerInput st) = Nothing
  | otherwise =
      case lexerInput st of
        c : _
          | c == ' ' || c == '\t' || c == '\r' -> Just (consumeWhile (\x -> x == ' ' || x == '\t' || x == '\r') st)
          | c == '\n' -> Just (advanceChars "\n" st)
        '-' : '-' : _ -> Just (consumeLineComment st)
        '{' : '-' : '#' : _ ->
          case tryConsumeControlPragma st of
            Just (Nothing, st') -> Just st'
            Just (Just tok, st') -> Just st' {lexerPending = lexerPending st' <> [tok]}
            Nothing ->
              case tryConsumeKnownPragma st of
                Just _ -> Nothing
                Nothing ->
                  consumeUnknownPragma st
        '{' : '-' : _ ->
          Just (consumeBlockCommentOrError st)
        _ ->
          case tryConsumeLineDirective st of
            Just (Nothing, st') -> Just st'
            Just (Just tok, st') -> Just st' {lexerPending = lexerPending st' <> [tok]}
            Nothing -> Nothing

nextToken :: LexerState -> (LexToken, LexerState)
nextToken st =
  fromMaybe (lexErrorToken st "unexpected character") (firstJust tokenParsers)
  where
    tokenParsers =
      [ lexKnownPragma,
        lexQuasiQuote,
        lexHexFloat,
        lexFloat,
        lexIntBase,
        lexInt,
        lexChar,
        lexString,
        lexSymbol,
        lexIdentifier,
        lexOperator
      ]

    firstJust [] = Nothing
    firstJust (parser : rest) =
      case parser st of
        Just out -> Just out
        Nothing -> firstJust rest

-- | Apply all extension-driven post-lexing rewrites in a deterministic order.
applyExtensions :: [Extension] -> [LexToken] -> [LexToken]
applyExtensions exts toks
  | NegativeLiterals `elem` exts = applyNegativeLiterals toks
  | otherwise = toks

-- | Implement @NegativeLiterals@ by merging @-@ and immediately adjacent numerics.
--
-- The merge only happens when there is no intervening whitespace/comments, and only
-- for integer/base-integer/float tokens.
applyNegativeLiterals :: [LexToken] -> [LexToken]
applyNegativeLiterals toks =
  case toks of
    minusTok : numTok : rest
      | lexTokenKind minusTok == TkOperator "-",
        tokensAdjacent minusTok numTok ->
          case lexTokenKind numTok of
            TkInteger n ->
              negativeIntegerToken minusTok numTok n : applyNegativeLiterals rest
            TkIntegerBase n repr ->
              negativeIntegerBaseToken minusTok numTok n repr : applyNegativeLiterals rest
            TkFloat n repr ->
              negativeFloatToken minusTok numTok n repr : applyNegativeLiterals rest
            _ -> minusTok : applyNegativeLiterals (numTok : rest)
    tok : rest -> tok : applyNegativeLiterals rest
    [] -> []

-- | True when the second token starts exactly where the first one ends.
tokensAdjacent :: LexToken -> LexToken -> Bool
tokensAdjacent first second =
  case (lexTokenSpan first, lexTokenSpan second) of
    (SourceSpan _ _ firstEndLine firstEndCol, SourceSpan secondStartLine secondStartCol _ _) ->
      firstEndLine == secondStartLine && firstEndCol == secondStartCol
    _ -> False

-- | Build a negative decimal integer token from @-@ and a positive literal token.
negativeIntegerToken :: LexToken -> LexToken -> Integer -> LexToken
negativeIntegerToken minusTok numTok n =
  LexToken
    { lexTokenKind = TkInteger (negate n),
      lexTokenText = lexTokenText minusTok <> lexTokenText numTok,
      lexTokenSpan = combinedSpan minusTok numTok
    }

-- | Build a negative non-decimal integer token from @-@ and a positive literal token.
negativeIntegerBaseToken :: LexToken -> LexToken -> Integer -> Text -> LexToken
negativeIntegerBaseToken minusTok numTok n repr =
  LexToken
    { lexTokenKind = TkIntegerBase (negate n) ("-" <> repr),
      lexTokenText = lexTokenText minusTok <> lexTokenText numTok,
      lexTokenSpan = combinedSpan minusTok numTok
    }

-- | Build a negative float token from @-@ and a positive literal token.
negativeFloatToken :: LexToken -> LexToken -> Double -> Text -> LexToken
negativeFloatToken minusTok numTok n repr =
  LexToken
    { lexTokenKind = TkFloat (negate n) ("-" <> repr),
      lexTokenText = lexTokenText minusTok <> lexTokenText numTok,
      lexTokenSpan = combinedSpan minusTok numTok
    }

-- | Span that starts at the first token and ends at the second token.
combinedSpan :: LexToken -> LexToken -> SourceSpan
combinedSpan first second =
  case (lexTokenSpan first, lexTokenSpan second) of
    (SourceSpan sl sc _ _, SourceSpan _ _ el ec) -> SourceSpan sl sc el ec
    _ -> NoSourceSpan

applyLayoutTokens :: Bool -> [LexToken] -> [LexToken]
applyLayoutTokens enableModuleLayout =
  go
    LayoutState
      { layoutContexts = [],
        layoutPendingLayout = False,
        layoutPrevLine = Nothing,
        layoutModuleMode =
          if enableModuleLayout
            then ModuleLayoutSeekStart
            else ModuleLayoutOff
      }
  where
    go st toks =
      case toks of
        [] -> closeAllImplicit (layoutContexts st) NoSourceSpan
        tok : rest ->
          let stModule = noteModuleLayoutBeforeToken st tok
              (pendingInserted, stAfterPending, skipBOL) = openPendingLayout stModule tok
              (bolInserted, stAfterBOL) = if skipBOL then ([], stAfterPending) else bolLayout stAfterPending tok
              stAfterToken = noteModuleLayoutAfterToken (stepTokenContext stAfterBOL tok) tok
              stNext = stAfterToken {layoutPrevLine = Just (tokenStartLine tok)}
           in pendingInserted <> bolInserted <> (tok : go stNext rest)

noteModuleLayoutBeforeToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutBeforeToken st tok =
  case layoutModuleMode st of
    ModuleLayoutSeekStart ->
      case lexTokenKind tok of
        TkPragmaLanguage _ -> st
        TkPragmaWarning _ -> st
        TkPragmaDeprecated _ -> st
        TkKeywordModule -> st {layoutModuleMode = ModuleLayoutAwaitWhere}
        _ -> st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = True}
    _ -> st

noteModuleLayoutAfterToken :: LayoutState -> LexToken -> LayoutState
noteModuleLayoutAfterToken st tok =
  case layoutModuleMode st of
    ModuleLayoutAwaitWhere
      | lexTokenKind tok == TkKeywordWhere ->
          st {layoutModuleMode = ModuleLayoutDone, layoutPendingLayout = True}
    _ -> st

openPendingLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState, Bool)
openPendingLayout st tok
  | not (layoutPendingLayout st) = ([], st, False)
  | otherwise =
      case lexTokenKind tok of
        TkSymbol "{" -> ([], st {layoutPendingLayout = False}, False)
        _ ->
          let col = tokenStartCol tok
              parentIndent = currentLayoutIndent (layoutContexts st)
              openTok = virtualSymbolToken "{" (lexTokenSpan tok)
              closeTok = virtualSymbolToken "}" (lexTokenSpan tok)
           in if col <= parentIndent
                then ([openTok, closeTok], st {layoutPendingLayout = False}, False)
                else
                  ( [openTok],
                    st
                      { layoutPendingLayout = False,
                        layoutContexts = LayoutImplicit col : layoutContexts st
                      },
                    True
                  )

bolLayout :: LayoutState -> LexToken -> ([LexToken], LayoutState)
bolLayout st tok
  | not (isBOL st tok) = ([], st)
  | otherwise =
      let col = tokenStartCol tok
          (inserted, contexts') = closeForDedent col (lexTokenSpan tok) (layoutContexts st)
          eqSemi =
            case contexts' of
              LayoutImplicit indent : _
                | col == indent,
                  not (suppressesVirtualSemicolon tok) ->
                    [virtualSymbolToken ";" (lexTokenSpan tok)]
              _ -> []
       in (inserted <> eqSemi, st {layoutContexts = contexts'})

suppressesVirtualSemicolon :: LexToken -> Bool
suppressesVirtualSemicolon tok =
  case lexTokenKind tok of
    TkKeywordThen -> True
    TkKeywordElse -> True
    _ -> False

closeForDedent :: Int -> SourceSpan -> [LayoutContext] -> ([LexToken], [LayoutContext])
closeForDedent col anchor = go []
  where
    go acc contexts =
      case contexts of
        LayoutImplicit indent : rest
          | col < indent -> go (virtualSymbolToken "}" anchor : acc) rest
          | otherwise -> (reverse acc, contexts)
        _ -> (reverse acc, contexts)

closeAllImplicit :: [LayoutContext] -> SourceSpan -> [LexToken]
closeAllImplicit contexts anchor =
  [virtualSymbolToken "}" anchor | LayoutImplicit _ <- contexts]

stepTokenContext :: LayoutState -> LexToken -> LayoutState
stepTokenContext st tok =
  case lexTokenKind tok of
    TkKeywordDo -> st {layoutPendingLayout = True}
    TkKeywordOf -> st {layoutPendingLayout = True}
    TkSymbol "{" -> st {layoutContexts = LayoutExplicit : layoutContexts st}
    TkSymbol "}" -> st {layoutContexts = popOneContext (layoutContexts st)}
    _ -> st

popOneContext :: [LayoutContext] -> [LayoutContext]
popOneContext contexts =
  case contexts of
    _ : rest -> rest
    [] -> []

currentLayoutIndent :: [LayoutContext] -> Int
currentLayoutIndent contexts =
  case contexts of
    LayoutImplicit indent : _ -> indent
    _ -> 0

isBOL :: LayoutState -> LexToken -> Bool
isBOL st tok =
  case layoutPrevLine st of
    Just prevLine -> tokenStartLine tok > prevLine
    Nothing -> False

tokenStartLine :: LexToken -> Int
tokenStartLine tok =
  case lexTokenSpan tok of
    SourceSpan line _ _ _ -> line
    NoSourceSpan -> 1

tokenStartCol :: LexToken -> Int
tokenStartCol tok =
  case lexTokenSpan tok of
    SourceSpan _ col _ _ -> col
    NoSourceSpan -> 1

virtualSymbolToken :: Text -> SourceSpan -> LexToken
virtualSymbolToken sym span' =
  LexToken
    { lexTokenKind = TkSymbol sym,
      lexTokenText = sym,
      lexTokenSpan = span'
    }

lexKnownPragma :: LexerState -> Maybe (LexToken, LexerState)
lexKnownPragma st
  | Just ((raw, kind), st') <- parsePragmaLike parseLanguagePragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseOptionsPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseWarningPragma st = Just (mkToken st st' raw kind, st')
  | Just ((raw, kind), st') <- parsePragmaLike parseDeprecatedPragma st = Just (mkToken st st' raw kind, st')
  | otherwise = Nothing

parsePragmaLike :: (String -> Maybe (Int, (Text, LexTokenKind))) -> LexerState -> Maybe ((Text, LexTokenKind), LexerState)
parsePragmaLike parser st = do
  (n, out) <- parser (lexerInput st)
  pure (out, advanceChars (take n (lexerInput st)) st)

lexIdentifier :: LexerState -> Maybe (LexToken, LexerState)
lexIdentifier st =
  case lexerInput st of
    c : rest
      | isIdentStart c ->
          let (seg, rest0) = span isIdentTail rest
              firstChunk = c : seg
              (consumed, _) = gatherQualified firstChunk rest0
              ident = T.pack consumed
              kind = fromMaybe (TkIdentifier ident) (keywordTokenKind ident)
              st' = advanceChars consumed st
           in Just (mkToken st st' ident kind, st')
    _ -> Nothing
  where
    gatherQualified acc chars =
      case chars of
        '.' : c : more
          | isIdentStart c ->
              let (seg, rest) = span isIdentTail more
               in gatherQualified (acc <> "." <> [c] <> seg) rest
        _ -> (acc, chars)

lexOperator :: LexerState -> Maybe (LexToken, LexerState)
lexOperator st =
  case span isSymbolicOpChar (lexerInput st) of
    ("", _) -> Nothing
    (op, _) ->
      let txt = T.pack op
          st' = advanceChars op st
       in Just (mkToken st st' txt (TkOperator txt), st')

lexSymbol :: LexerState -> Maybe (LexToken, LexerState)
lexSymbol st =
  firstJust
    [ ("..", TkSymbol ".."),
      ("`", TkSymbol "`"),
      ("@", TkSymbol "@"),
      ("(", TkSymbol "("),
      (")", TkSymbol ")"),
      ("[", TkSymbol "["),
      ("]", TkSymbol "]"),
      ("{", TkSymbol "{"),
      ("}", TkSymbol "}"),
      (",", TkSymbol ","),
      (";", TkSymbol ";")
    ]
  where
    firstJust xs =
      case xs of
        [] -> Nothing
        (txt, kind) : rest ->
          if txt `List.isPrefixOf` lexerInput st
            then
              let st' = advanceChars txt st
               in Just (mkToken st st' (T.pack txt) kind, st')
            else firstJust rest

lexIntBase :: LexerState -> Maybe (LexToken, LexerState)
lexIntBase st =
  case lexerInput st of
    '0' : base : rest
      | base `elem` ("xXoObB" :: String) ->
          let isDigitChar
                | base `elem` ("xX" :: String) = isHexDigit
                | base `elem` ("oO" :: String) = isOctDigit
                | otherwise = (`elem` ("01" :: String))
              (digitsRaw, _) = takeDigitsWithUnderscores isDigitChar rest
           in if null digitsRaw
                then Nothing
                else
                  let raw = '0' : base : digitsRaw
                      txt = T.pack raw
                      n
                        | base `elem` ("xX" :: String) = readHexLiteral txt
                        | base `elem` ("oO" :: String) = readOctLiteral txt
                        | otherwise = readBinLiteral txt
                      st' = advanceChars raw st
                   in Just (mkToken st st' txt (TkIntegerBase n txt), st')
    _ -> Nothing

lexHexFloat :: LexerState -> Maybe (LexToken, LexerState)
lexHexFloat st = do
  ('0' : x : rest) <- Just (lexerInput st)
  if x `notElem` ("xX" :: String)
    then Nothing
    else do
      let (intDigits, rest1) = span isHexDigit rest
      if null intDigits
        then Nothing
        else do
          let (mFracDigits, rest2) =
                case rest1 of
                  '.' : more ->
                    let (frac, rest') = span isHexDigit more
                     in (Just frac, rest')
                  _ -> (Nothing, rest1)
          expo@(_ : expoRest) <- takeHexExponent rest2
          let fracDigits = fromMaybe "" mFracDigits
          if null expoRest
            then Nothing
            else
              let dotAndFrac =
                    case mFracDigits of
                      Just ds -> '.' : ds
                      Nothing -> ""
                  raw = '0' : x : intDigits <> dotAndFrac <> expo
                  txt = T.pack raw
                  value = parseHexFloatLiteral intDigits fracDigits expo
                  st' = advanceChars raw st
               in Just (mkToken st st' txt (TkFloat value txt), st')

lexFloat :: LexerState -> Maybe (LexToken, LexerState)
lexFloat st =
  let (lhsRaw, rest) = takeDigitsWithUnderscores isDigit (lexerInput st)
   in if null lhsRaw
        then Nothing
        else case rest of
          '.' : d : more
            | isDigit d ->
                let (rhsRaw, rest') = takeDigitsWithUnderscores isDigit (d : more)
                    (expo, _) = takeExponent rest'
                    raw = lhsRaw <> "." <> rhsRaw <> expo
                    txt = T.pack raw
                    normalized = filter (/= '_') raw
                    st' = advanceChars raw st
                 in Just (mkToken st st' txt (TkFloat (read normalized) txt), st')
          _ ->
            case takeExponent rest of
              ("", _) -> Nothing
              (expo, _) ->
                let raw = lhsRaw <> expo
                    txt = T.pack raw
                    normalized = filter (/= '_') raw
                    st' = advanceChars raw st
                 in Just (mkToken st st' txt (TkFloat (read normalized) txt), st')

lexInt :: LexerState -> Maybe (LexToken, LexerState)
lexInt st =
  let (digitsRaw, _) = takeDigitsWithUnderscores isDigit (lexerInput st)
   in if null digitsRaw
        then Nothing
        else
          let txt = T.pack digitsRaw
              digits = filter (/= '_') digitsRaw
              st' = advanceChars digitsRaw st
           in Just (mkToken st st' txt (TkInteger (read digits)), st')

lexChar :: LexerState -> Maybe (LexToken, LexerState)
lexChar st =
  case lexerInput st of
    '\'' : rest ->
      case scanQuoted '\'' rest of
        Right (body, _) ->
          let raw = '\'' : body <> "'"
              st' = advanceChars raw st
           in case readMaybeChar raw of
                Just c -> Just (mkToken st st' (T.pack raw) (TkChar c), st')
                Nothing -> Just (mkErrorToken st st' (T.pack raw) "invalid char literal", st')
        Left raw ->
          let full = '\'' : raw
              st' = advanceChars full st
           in Just (mkErrorToken st st' (T.pack full) "unterminated char literal", st')
    _ -> Nothing

lexString :: LexerState -> Maybe (LexToken, LexerState)
lexString st =
  case lexerInput st of
    '"' : rest ->
      case scanQuoted '"' rest of
        Right (body, _) ->
          let raw = "\"" <> body <> "\""
              decoded =
                case reads raw of
                  [(str, "")] -> T.pack str
                  _ -> T.pack body
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) (TkString decoded), st')
        Left raw ->
          let full = '"' : raw
              st' = advanceChars full st
           in Just (mkErrorToken st st' (T.pack full) "unterminated string literal", st')
    _ -> Nothing

lexQuasiQuote :: LexerState -> Maybe (LexToken, LexerState)
lexQuasiQuote st =
  case lexerInput st of
    '[' : rest ->
      case parseQuasiQuote rest of
        Just (quoter, body, _) ->
          let raw = "[" <> quoter <> "|" <> body <> "|]"
              st' = advanceChars raw st
           in Just (mkToken st st' (T.pack raw) (TkQuasiQuote (T.pack quoter) (T.pack body)), st')
        Nothing -> Nothing
    _ -> Nothing
  where
    parseQuasiQuote chars =
      let (quoter, rest0) = takeQuoter chars
       in case rest0 of
            '|' : rest1
              | not (null quoter) ->
                  let (body, rest2) = breakOnMarker "|]" rest1
                   in case rest2 of
                        '|' : ']' : _ -> Just (quoter, body, take (length body + 2) rest1)
                        _ -> Nothing
            _ -> Nothing

lexErrorToken :: LexerState -> Text -> (LexToken, LexerState)
lexErrorToken st msg =
  let raw = take 1 (lexerInput st)
      rawTxt = if null raw then "<eof>" else T.pack raw
      st' = if null raw then st else advanceChars raw st
   in ( mkErrorToken st st' rawTxt msg,
        st'
      )

mkErrorToken :: LexerState -> LexerState -> Text -> Text -> LexToken
mkErrorToken start end rawTxt msg =
  mkToken start end rawTxt (TkError msg)

tryConsumeLineDirective :: LexerState -> Maybe (Maybe LexToken, LexerState)
tryConsumeLineDirective st
  | not (lexerAtLineStart st) = Nothing
  | otherwise =
      let (spaces, rest) = span (\c -> c == ' ' || c == '\t') (lexerInput st)
       in case rest of
            '#' : more ->
              let lineText = '#' : takeLineRemainder more
                  consumed = spaces <> lineText
               in case parseHashLineDirective lineText of
                    Just update ->
                      Just (Nothing, applyDirectiveAdvance consumed update st)
                    Nothing ->
                      let st' = advanceChars consumed st
                       in Just (Just (mkToken st st' (T.pack consumed) (TkError "malformed line directive")), st')
            _ -> Nothing

tryConsumeControlPragma :: LexerState -> Maybe (Maybe LexToken, LexerState)
tryConsumeControlPragma st =
  case parseControlPragma (lexerInput st) of
    Just (consumed0, Right update0) ->
      let (consumed, update) =
            case directiveLine update0 of
              Just lineNo ->
                case drop (length consumed0) (lexerInput st) of
                  '\n' : _ ->
                    (consumed0 <> "\n", update0 {directiveLine = Just lineNo, directiveCol = Just 1})
                  _ -> (consumed0, update0)
              Nothing -> (consumed0, update0)
       in Just (Nothing, applyDirectiveAdvance consumed update st)
    Just (consumed, Left msg) ->
      let st' = advanceChars consumed st
       in Just (Just (mkToken st st' (T.pack consumed) (TkError msg)), st')
    Nothing -> Nothing

applyDirectiveAdvance :: String -> DirectiveUpdate -> LexerState -> LexerState
applyDirectiveAdvance consumed update st =
  let hasTrailingNewline =
        case reverse consumed of
          '\n' : _ -> True
          _ -> False
   in st
        { lexerInput = drop (length consumed) (lexerInput st),
          lexerLine = maybe (lexerLine st) (max 1) (directiveLine update),
          lexerCol = maybe (lexerCol st) (max 1) (directiveCol update),
          lexerAtLineStart = hasTrailingNewline || (Just 1 == directiveCol update)
        }

consumeLineComment :: LexerState -> LexerState
consumeLineComment st = advanceChars consumed st
  where
    consumed = takeCommentRemainder (drop 2 (lexerInput st))
    prefix = "--"
    takeCommentRemainder xs =
      prefix <> takeWhile (/= '\n') xs

consumeUnknownPragma :: LexerState -> Maybe LexerState
consumeUnknownPragma st =
  case breakOnMarker "#-}" (lexerInput st) of
    (_, "") -> Nothing
    (body, marker) ->
      let consumed = body <> take 3 marker
       in Just (advanceChars consumed st)

consumeBlockComment :: LexerState -> Maybe LexerState
consumeBlockComment st =
  case scanNestedBlockComment 1 (drop 2 (lexerInput st)) of
    Just consumedTail -> Just (advanceChars ("{-" <> consumedTail) st)
    Nothing -> Nothing

consumeBlockCommentOrError :: LexerState -> LexerState
consumeBlockCommentOrError st =
  case consumeBlockComment st of
    Just st' -> st'
    Nothing ->
      let consumed = lexerInput st
          st' = advanceChars consumed st
          tok = mkToken st st' (T.pack consumed) (TkError "unterminated block comment")
       in st' {lexerPending = lexerPending st' <> [tok]}

scanNestedBlockComment :: Int -> String -> Maybe String
scanNestedBlockComment depth chars
  | depth <= 0 = Just ""
  | otherwise =
      case chars of
        [] -> Nothing
        '{' : '-' : rest -> ("{-" <>) <$> scanNestedBlockComment (depth + 1) rest
        '-' : '}' : rest ->
          if depth == 1
            then Just "-}"
            else ("-}" <>) <$> scanNestedBlockComment (depth - 1) rest
        c : rest -> (c :) <$> scanNestedBlockComment depth rest

tryConsumeKnownPragma :: LexerState -> Maybe ()
tryConsumeKnownPragma st =
  case lexKnownPragma st of
    Just _ -> Just ()
    Nothing -> Nothing

parseLanguagePragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseLanguagePragma input = do
  (_, body, consumed) <- stripNamedPragma ["LANGUAGE"] input
  let names = parseLanguagePragmaNames (T.pack body)
      raw = "{-# LANGUAGE " <> T.unpack (T.intercalate ", " (map extensionSettingName names)) <> " #-}"
  pure (length consumed, (T.pack raw, TkPragmaLanguage names))

parseOptionsPragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseOptionsPragma input = do
  (pragmaName, body, consumed) <- stripNamedPragma ["OPTIONS_GHC", "OPTIONS"] input
  let settings = parseOptionsPragmaSettings (T.pack body)
      raw = "{-# " <> pragmaName <> " " <> T.unpack (T.strip (T.pack body)) <> " #-}"
  pure (length consumed, (T.pack raw, TkPragmaLanguage settings))

parseWarningPragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseWarningPragma input = do
  (_, body, consumed) <- stripNamedPragma ["WARNING"] input
  let txt = T.strip (T.pack body)
      (msg, rawMsg) =
        case body of
          '"' : _ ->
            case reads body of
              [(decoded, "")] -> (T.pack decoded, T.pack body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# WARNING " <> rawMsg <> " #-}"
  pure (length consumed, (raw, TkPragmaWarning msg))

parseDeprecatedPragma :: String -> Maybe (Int, (Text, LexTokenKind))
parseDeprecatedPragma input = do
  (_, body, consumed) <- stripNamedPragma ["DEPRECATED"] input
  let txt = T.strip (T.pack body)
      (msg, rawMsg) =
        case body of
          '"' : _ ->
            case reads body of
              [(decoded, "")] -> (T.pack decoded, T.pack body)
              _ -> (txt, txt)
          _ -> (txt, txt)
      raw = "{-# DEPRECATED " <> rawMsg <> " #-}"
  pure (length consumed, (raw, TkPragmaDeprecated msg))

stripPragma :: String -> String -> Maybe String
stripPragma name input = (\(_, body, _) -> body) <$> stripNamedPragma [name] input

stripNamedPragma :: [String] -> String -> Maybe (String, String, String)
stripNamedPragma names input = do
  rest0 <- List.stripPrefix "{-#" input
  let rest1 = dropWhile isSpace rest0
  name <- List.find (`List.isPrefixOf` rest1) names
  rest2 <- List.stripPrefix name rest1
  let rest3 = dropWhile isSpace rest2
      (body, marker) = breakOnMarker "#-}" rest3
  guardPrefix "#-}" marker
  let consumedLen = length input - length (drop 3 marker)
  pure (name, trimRight body, take consumedLen input)

parseLanguagePragmaNames :: Text -> [ExtensionSetting]
parseLanguagePragmaNames body =
  mapMaybe (parseExtensionSettingName . T.strip . T.takeWhile (/= '#')) (T.splitOn "," body)

parseOptionsPragmaSettings :: Text -> [ExtensionSetting]
parseOptionsPragmaSettings body = go (pragmaWords body)
  where
    go ws =
      case ws of
        [] -> []
        "-cpp" : rest -> EnableExtension CPP : go rest
        "-fffi" : rest -> EnableExtension ForeignFunctionInterface : go rest
        "-fglasgow-exts" : rest -> glasgowExtsSettings <> go rest
        opt : rest
          | Just ext <- T.stripPrefix "-X" opt,
            not (T.null ext) ->
              case parseExtensionSettingName ext of
                Just setting -> setting : go rest
                Nothing -> go rest
        _ : rest -> go rest

glasgowExtsSettings :: [ExtensionSetting]
glasgowExtsSettings =
  map
    EnableExtension
    [ ConstrainedClassMethods,
      DeriveDataTypeable,
      DeriveFoldable,
      DeriveFunctor,
      DeriveGeneric,
      DeriveTraversable,
      EmptyDataDecls,
      ExistentialQuantification,
      ExplicitNamespaces,
      FlexibleContexts,
      FlexibleInstances,
      ForeignFunctionInterface,
      FunctionalDependencies,
      GeneralizedNewtypeDeriving,
      ImplicitParams,
      InterruptibleFFI,
      KindSignatures,
      LiberalTypeSynonyms,
      MagicHash,
      MultiParamTypeClasses,
      ParallelListComp,
      PatternGuards,
      PostfixOperators,
      RankNTypes,
      RecursiveDo,
      ScopedTypeVariables,
      StandaloneDeriving,
      TypeOperators,
      TypeSynonymInstances,
      UnboxedTuples,
      UnicodeSyntax,
      UnliftedFFITypes
    ]

pragmaWords :: Text -> [Text]
pragmaWords txt = go [] [] Nothing (T.unpack txt)
  where
    go acc current quote chars =
      case chars of
        [] ->
          let acc' = pushCurrent acc current
           in reverse acc'
        c : rest ->
          case quote of
            Just q
              | c == q -> go acc current Nothing rest
              | c == '\\' ->
                  case rest of
                    escaped : rest' -> go acc (escaped : current) quote rest'
                    [] -> go acc current quote []
              | otherwise -> go acc (c : current) quote rest
            Nothing
              | c == '"' || c == '\'' -> go acc current (Just c) rest
              | c == '\\' ->
                  case rest of
                    escaped : rest' -> go acc (escaped : current) Nothing rest'
                    [] -> go acc current Nothing []
              | c `elem` [' ', '\n', '\r', '\t'] ->
                  let acc' = pushCurrent acc current
                   in go acc' [] Nothing rest
              | otherwise -> go acc (c : current) Nothing rest

    pushCurrent acc current =
      case reverse current of
        [] -> acc
        token -> T.pack token : acc

parseHashLineDirective :: String -> Maybe DirectiveUpdate
parseHashLineDirective raw =
  let trimmed = trimLeft (drop 1 (trimLeft raw))
      trimmed' =
        if "line" `List.isPrefixOf` trimmed
          then dropWhile isSpace (drop 4 trimmed)
          else trimmed
      (digits, _) = span isDigit trimmed'
   in if null digits
        then Nothing
        else Just DirectiveUpdate {directiveLine = Just (read digits), directiveCol = Just 1}

parseControlPragma :: String -> Maybe (String, Either Text DirectiveUpdate)
parseControlPragma input
  | Just body <- stripPragma "LINE" input =
      let trimmed = words body
       in case trimmed of
            lineNo : _
              | all isDigit lineNo ->
                  Just
                    ( fullPragmaConsumed "LINE" body,
                      Right DirectiveUpdate {directiveLine = Just (read lineNo), directiveCol = Just 1}
                    )
            _ -> Just (fullPragmaConsumed "LINE" body, Left "malformed LINE pragma")
  | Just body <- stripPragma "COLUMN" input =
      let trimmed = words body
       in case trimmed of
            colNo : _
              | all isDigit colNo ->
                  Just
                    ( fullPragmaConsumed "COLUMN" body,
                      Right DirectiveUpdate {directiveLine = Nothing, directiveCol = Just (read colNo)}
                    )
            _ -> Just (fullPragmaConsumed "COLUMN" body, Left "malformed COLUMN pragma")
  | otherwise = Nothing

fullPragmaConsumed :: String -> String -> String
fullPragmaConsumed name body = "{-# " <> name <> " " <> trimRight body <> " #-}"

mkToken :: LexerState -> LexerState -> Text -> LexTokenKind -> LexToken
mkToken start end tokTxt kind =
  LexToken
    { lexTokenKind = kind,
      lexTokenText = tokTxt,
      lexTokenSpan = mkSpan start end
    }

mkSpan :: LexerState -> LexerState -> SourceSpan
mkSpan start end =
  SourceSpan
    { sourceSpanStartLine = lexerLine start,
      sourceSpanStartCol = lexerCol start,
      sourceSpanEndLine = lexerLine end,
      sourceSpanEndCol = lexerCol end
    }

advanceChars :: String -> LexerState -> LexerState
advanceChars chars st = foldl advanceOne st chars
  where
    advanceOne acc ch =
      case ch of
        '\n' ->
          acc
            { lexerInput = drop 1 (lexerInput acc),
              lexerLine = lexerLine acc + 1,
              lexerCol = 1,
              lexerAtLineStart = True
            }
        _ ->
          acc
            { lexerInput = drop 1 (lexerInput acc),
              lexerCol = lexerCol acc + 1,
              lexerAtLineStart = False
            }

consumeWhile :: (Char -> Bool) -> LexerState -> LexerState
consumeWhile f st = advanceChars (takeWhile f (lexerInput st)) st

takeDigitsWithUnderscores :: (Char -> Bool) -> String -> (String, String)
takeDigitsWithUnderscores isDigitChar chars =
  let (firstChunk, rest) = span isDigitChar chars
   in if null firstChunk
        then ("", chars)
        else go firstChunk rest
  where
    go acc xs =
      case xs of
        '_' : rest ->
          let (chunk, rest') = span isDigitChar rest
           in if null chunk
                then (acc, xs)
                else go (acc <> "_" <> chunk) rest'
        _ -> (acc, xs)

takeExponent :: String -> (String, String)
takeExponent chars =
  case chars of
    marker : rest
      | marker `elem` ("eE" :: String) ->
          let (signPart, rest1) =
                case rest of
                  sign : more | sign `elem` ("+-" :: String) -> ([sign], more)
                  _ -> ("", rest)
              (digits, rest2) = takeDigitsWithUnderscores isDigit rest1
           in if null digits then ("", chars) else (marker : signPart <> digits, rest2)
    _ -> ("", chars)

takeHexExponent :: String -> Maybe String
takeHexExponent chars =
  case chars of
    marker : rest
      | marker `elem` ("pP" :: String) ->
          let (signPart, rest1) =
                case rest of
                  sign : more | sign `elem` ("+-" :: String) -> ([sign], more)
                  _ -> ("", rest)
              (digits, _) = span isDigit rest1
           in if null digits then Nothing else Just (marker : signPart <> digits)
    _ -> Nothing

scanQuoted :: Char -> String -> Either String (String, String)
scanQuoted endCh = go []
  where
    go acc chars =
      case chars of
        [] -> Left (reverse acc)
        c : rest
          | c == endCh -> Right (reverse acc, rest)
          | c == '\\' ->
              case rest of
                escaped : rest' -> go (escaped : c : acc) rest'
                [] -> Left (reverse (c : acc))
          | otherwise -> go (c : acc) rest

takeQuoter :: String -> (String, String)
takeQuoter chars =
  case chars of
    c : rest
      | isIdentStart c ->
          let (tailChars, rest0) = span isIdentTailOrStart rest
           in go (c : tailChars) rest0
    _ -> ("", chars)
  where
    go acc chars' =
      case chars' of
        '.' : c : rest
          | isIdentStart c ->
              let (tailChars, rest0) = span isIdentTailOrStart rest
               in go (acc <> "." <> [c] <> tailChars) rest0
        _ -> (acc, chars')

breakOnMarker :: String -> String -> (String, String)
breakOnMarker marker = go []
  where
    go acc chars
      | marker `List.isPrefixOf` chars = (reverse acc, chars)
      | otherwise =
          case chars of
            [] -> (reverse acc, [])
            c : rest -> go (c : acc) rest

takeLineRemainder :: String -> String
takeLineRemainder chars =
  let (prefix, rest) = break (== '\n') chars
   in prefix <> take 1 rest

trimLeft :: String -> String
trimLeft = dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse

guardPrefix :: (Eq a) => [a] -> [a] -> Maybe ()
guardPrefix prefix actual =
  if prefix `List.isPrefixOf` actual
    then Just ()
    else Nothing

readMaybeChar :: String -> Maybe Char
readMaybeChar raw =
  case reads raw of
    [(c, "")] -> Just c
    _ -> Nothing

readHexLiteral :: Text -> Integer
readHexLiteral txt =
  case readHex (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

readOctLiteral :: Text -> Integer
readOctLiteral txt =
  case readOct (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

readBinLiteral :: Text -> Integer
readBinLiteral txt =
  case readInt 2 (`elem` ("01" :: String)) digitToInt (T.unpack (T.filter (/= '_') (T.drop 2 txt))) of
    [(n, "")] -> n
    _ -> 0

parseHexFloatLiteral :: String -> String -> String -> Double
parseHexFloatLiteral intDigits fracDigits expo =
  (parseHexDigits intDigits + parseHexFraction fracDigits) * (2 ^^ exponentValue expo)

parseHexDigits :: String -> Double
parseHexDigits = foldl (\acc d -> acc * 16 + fromIntegral (digitToInt d)) 0

parseHexFraction :: String -> Double
parseHexFraction ds =
  sum [fromIntegral (digitToInt d) / (16 ^^ i) | (d, i) <- zip ds [1 :: Int ..]]

exponentValue :: String -> Int
exponentValue expo =
  case expo of
    _ : '-' : ds -> negate (read ds)
    _ : '+' : ds -> read ds
    _ : ds -> read ds
    _ -> 0

isIdentStart :: Char -> Bool
isIdentStart c = isAsciiUpper c || isAsciiLower c || c == '_'

isIdentTail :: Char -> Bool
isIdentTail c = isAlphaNum c || c == '_' || c == '\''

isSymbolicOpChar :: Char -> Bool
isSymbolicOpChar c = c `elem` (":!#$%&*+./<=>?\\^|-~" :: String)

isIdentTailOrStart :: Char -> Bool
isIdentTailOrStart c = isAlphaNum c || c == '_' || c == '\''

isReservedIdentifier :: Text -> Bool
isReservedIdentifier = isJust . keywordTokenKind

keywordTokenKind :: Text -> Maybe LexTokenKind
keywordTokenKind txt = case txt of
  "module" -> Just TkKeywordModule
  "where" -> Just TkKeywordWhere
  "do" -> Just TkKeywordDo
  "data" -> Just TkKeywordData
  "import" -> Just TkKeywordImport
  "qualified" -> Just TkKeywordQualified
  "as" -> Just TkKeywordAs
  "hiding" -> Just TkKeywordHiding
  "case" -> Just TkKeywordCase
  "of" -> Just TkKeywordOf
  "let" -> Just TkKeywordLet
  "in" -> Just TkKeywordIn
  "if" -> Just TkKeywordIf
  "then" -> Just TkKeywordThen
  "else" -> Just TkKeywordElse
  _ -> Nothing
