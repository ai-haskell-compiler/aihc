{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Parser.Lex.Types
  ( LexTokenKind (..),
    pattern TkVarRole,
    pattern TkVarFamily,
    pattern TkVarPattern,
    pattern TkVarInstance,
    pattern TkVarAs,
    pattern TkVarHiding,
    pattern TkVarQualified,
    TokenOrigin (..),
    LexToken (..),
    LexerEnv (..),
    hasExt,
    LexerState (..),
    LayoutContext (..),
    ImplicitLayoutKind (..),
    PendingLayout (..),
    ModuleLayoutMode (..),
    LayoutState (..),
    DirectiveUpdate (..),
    HashLineTrivia (..),
    mkLexerEnv,
    mkInitialLexerState,
    mkInitialLayoutState,
    mkToken,
    mkErrorToken,
    mkSpan,
    advanceChars,
    advanceN,
    consumeWhile,
    tokenStartLine,
    tokenStartCol,
    virtualSymbolToken,
  )
where

import Aihc.Parser.Syntax
import Control.DeepSeq (NFData)
import Data.Char (ord)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

data LexTokenKind
  = -- Keywords (reserved identifiers per Haskell Report Section 2.4)
    TkKeywordCase
  | TkKeywordClass
  | TkKeywordData
  | TkKeywordDefault
  | TkKeywordDeriving
  | TkKeywordDo
  | TkKeywordElse
  | TkKeywordForall
  | TkKeywordForeign
  | TkKeywordIf
  | TkKeywordImport
  | TkKeywordIn
  | TkKeywordInfix
  | TkKeywordInfixl
  | TkKeywordInfixr
  | TkKeywordInstance
  | TkKeywordLet
  | TkKeywordModule
  | TkKeywordNewtype
  | TkKeywordOf
  | TkKeywordThen
  | TkKeywordType
  | TkKeywordWhere
  | TkKeywordUnderscore
  | -- Extension-conditional keywords
    TkKeywordProc
  | TkKeywordRec
  | TkKeywordMdo
  | -- Reserved operators (per Haskell Report Section 2.4)
    TkReservedDotDot
  | TkReservedColon
  | TkReservedDoubleColon
  | TkReservedEquals
  | TkReservedBackslash
  | TkReservedPipe
  | TkReservedLeftArrow
  | TkReservedRightArrow
  | TkReservedAt
  | TkReservedDoubleArrow
  | -- Arrow notation reserved operators (Arrows extension)
    TkArrowTail
  | TkArrowTailReverse
  | TkDoubleArrowTail
  | TkDoubleArrowTailReverse
  | TkBananaOpen
  | TkBananaClose
  | -- Identifiers (per Haskell Report Section 2.4)
    TkVarId Text
  | TkConId Text
  | TkQVarId Text
  | TkQConId Text
  | TkImplicitParam Text
  | -- Operators (per Haskell Report Section 2.4)
    TkVarSym Text
  | TkConSym Text
  | TkQVarSym Text
  | TkQConSym Text
  | -- Literals
    TkInteger Integer
  | TkIntegerHash Integer Text
  | TkIntegerBase Integer Text
  | TkIntegerBaseHash Integer Text
  | TkFloat Double Text
  | TkFloatHash Double Text
  | TkChar Char
  | TkCharHash Char Text
  | TkString Text
  | TkStringHash Text Text
  | TkOverloadedLabel Text Text
  | -- Special characters (per Haskell Report Section 2.2)
    TkSpecialLParen
  | TkSpecialRParen
  | TkSpecialUnboxedLParen
  | TkSpecialUnboxedRParen
  | TkSpecialComma
  | TkSpecialSemicolon
  | TkSpecialLBracket
  | TkSpecialRBracket
  | TkSpecialBacktick
  | TkSpecialLBrace
  | TkSpecialRBrace
  | -- LexicalNegation support
    TkMinusOperator
  | TkPrefixMinus
  | -- Whitespace-sensitive operator support (GHC proposal 0229)
    TkPrefixBang
  | TkPrefixTilde
  | -- TypeApplications support
    TkTypeApp
  | -- Pragmas
    TkPragma Pragma
  | -- TemplateHaskellQuotes bracket tokens
    TkTHExpQuoteOpen
  | TkTHExpQuoteClose
  | TkTHTypedQuoteOpen
  | TkTHTypedQuoteClose
  | TkTHDeclQuoteOpen
  | TkTHTypeQuoteOpen
  | TkTHPatQuoteOpen
  | TkTHQuoteTick
  | TkTHTypeQuoteTick
  | -- TemplateHaskell splice tokens (prefix $ and $$)
    TkTHSplice
  | TkTHTypedSplice
  | -- Other
    TkQuasiQuote Text Text
  | TkError Text
  | TkEOF
  deriving (Eq, Ord, Show, Read, Generic, NFData)

pattern TkVarRole :: LexTokenKind
pattern TkVarRole = TkVarId "role"

pattern TkVarFamily :: LexTokenKind
pattern TkVarFamily = TkVarId "family"

pattern TkVarPattern :: LexTokenKind
pattern TkVarPattern = TkVarId "pattern"

pattern TkVarInstance :: LexTokenKind
pattern TkVarInstance = TkVarId "instance"

pattern TkVarAs :: LexTokenKind
pattern TkVarAs = TkVarId "as"

pattern TkVarHiding :: LexTokenKind
pattern TkVarHiding = TkVarId "hiding"

pattern TkVarQualified :: LexTokenKind
pattern TkVarQualified = TkVarId "qualified"

data TokenOrigin
  = FromSource
  | InsertedLayout
  deriving (Eq, Ord, Show, Read, Generic, NFData)

data LexToken = LexToken
  { lexTokenKind :: !LexTokenKind,
    lexTokenText :: !Text,
    lexTokenSpan :: !SourceSpan,
    lexTokenOrigin :: !TokenOrigin
  }
  deriving (Eq, Ord, Show, Generic, NFData)

newtype LexerEnv = LexerEnv
  { lexerExtensions :: Set Extension
  }
  deriving (Eq, Show)

hasExt :: Extension -> LexerEnv -> Bool
hasExt ext env = Set.member ext (lexerExtensions env)

data LexerState = LexerState
  { lexerInput :: !Text,
    lexerLogicalSourceName :: !FilePath,
    lexerLine :: !Int,
    lexerCol :: !Int,
    lexerByteOffset :: !Int,
    lexerAtLineStart :: !Bool,
    lexerPrevTokenKind :: !(Maybe LexTokenKind),
    lexerHadTrivia :: !Bool
  }
  deriving (Eq, Show)

data LayoutContext
  = LayoutExplicit
  | LayoutImplicit !Int !ImplicitLayoutKind
  | LayoutDelimiter
  deriving (Eq, Show)

data ImplicitLayoutKind
  = LayoutOrdinary
  | LayoutLetBlock
  | LayoutMultiWayIf
  | LayoutAfterThenElse
  deriving (Eq, Show)

data PendingLayout
  = PendingImplicitLayout !ImplicitLayoutKind
  | PendingMaybeMultiWayIf
  deriving (Eq, Show)

data ModuleLayoutMode
  = ModuleLayoutOff
  | ModuleLayoutSeekStart
  | ModuleLayoutAwaitWhere
  | ModuleLayoutAwaitBody
  | ModuleLayoutDone
  deriving (Eq, Show)

data LayoutState = LayoutState
  { layoutContexts :: [LayoutContext],
    layoutPendingLayout :: !(Maybe PendingLayout),
    layoutPrevLine :: !(Maybe Int),
    layoutPrevTokenKind :: !(Maybe LexTokenKind),
    layoutModuleMode :: !ModuleLayoutMode,
    layoutPrevTokenEndSpan :: !(Maybe SourceSpan),
    layoutBuffer :: [LexToken]
  }
  deriving (Eq, Show)

data DirectiveUpdate = DirectiveUpdate
  { directiveLine :: !(Maybe Int),
    directiveCol :: !(Maybe Int),
    directiveSourceName :: !(Maybe FilePath)
  }
  deriving (Eq, Show)

data HashLineTrivia
  = HashLineDirective !DirectiveUpdate
  | HashLineShebang
  | HashLineMalformed
  deriving (Eq, Show)

mkLexerEnv :: [Extension] -> LexerEnv
mkLexerEnv exts = LexerEnv {lexerExtensions = Set.fromList exts}

mkInitialLexerState :: FilePath -> [Extension] -> Text -> (LexerEnv, LexerState)
mkInitialLexerState sourceName exts input =
  ( mkLexerEnv exts,
    LexerState
      { lexerInput = input,
        lexerLogicalSourceName = sourceName,
        lexerLine = 1,
        lexerCol = 1,
        lexerByteOffset = 0,
        lexerAtLineStart = True,
        lexerPrevTokenKind = Nothing,
        lexerHadTrivia = True
      }
  )

mkInitialLayoutState :: Bool -> LayoutState
mkInitialLayoutState enableModuleLayout =
  LayoutState
    { layoutContexts = [],
      layoutPendingLayout = Nothing,
      layoutPrevLine = Nothing,
      layoutPrevTokenKind = Nothing,
      layoutModuleMode =
        if enableModuleLayout
          then ModuleLayoutSeekStart
          else ModuleLayoutOff,
      layoutPrevTokenEndSpan = Nothing,
      layoutBuffer = []
    }

mkToken :: LexerState -> LexerState -> Text -> LexTokenKind -> LexToken
mkToken start end tokTxt kind =
  LexToken
    { lexTokenKind = kind,
      lexTokenText = tokTxt,
      lexTokenSpan = mkSpan start end,
      lexTokenOrigin = FromSource
    }

mkErrorToken :: LexerState -> LexerState -> Text -> Text -> LexToken
mkErrorToken start end rawTxt msg = mkToken start end rawTxt (TkError msg)

mkSpan :: LexerState -> LexerState -> SourceSpan
mkSpan start end =
  SourceSpan
    { sourceSpanSourceName = lexerLogicalSourceName start,
      sourceSpanStartLine = lexerLine start,
      sourceSpanStartCol = lexerCol start,
      sourceSpanEndLine = lexerLine end,
      sourceSpanEndCol = lexerCol end,
      sourceSpanStartOffset = lexerByteOffset start,
      sourceSpanEndOffset = lexerByteOffset end
    }

advanceChars :: Text -> LexerState -> LexerState
advanceChars consumed st =
  let !n = T.length consumed
      go (!line, !col, !byteOff, !atLineStart) ch =
        case ch of
          '\n' -> (line + 1, 1, byteOff + 1, True)
          '\t' ->
            let nextTabStop = 8 - ((col - 1) `mod` 8)
             in (line, col + nextTabStop, byteOff + 1, atLineStart)
          ' ' -> (line, col + 1, byteOff + 1, atLineStart)
          _ -> (line, col + 1, byteOff + utf8CharWidth ch, False)
      (!finalLine, !finalCol, !finalByteOff, !finalAtLineStart) =
        T.foldl' go (lexerLine st, lexerCol st, lexerByteOffset st, lexerAtLineStart st) consumed
   in st
        { lexerInput = T.drop n (lexerInput st),
          lexerLine = finalLine,
          lexerCol = finalCol,
          lexerByteOffset = finalByteOff,
          lexerAtLineStart = finalAtLineStart
        }

advanceN :: Int -> LexerState -> LexerState
advanceN n st = advanceChars (T.take n (lexerInput st)) st

consumeWhile :: (Char -> Bool) -> LexerState -> LexerState
consumeWhile f st =
  let consumed = T.takeWhile f (lexerInput st)
   in advanceChars consumed st

tokenStartLine :: LexToken -> Int
tokenStartLine tok =
  case lexTokenSpan tok of
    SourceSpan {sourceSpanStartLine = line} -> line
    NoSourceSpan -> 1

tokenStartCol :: LexToken -> Int
tokenStartCol tok =
  case lexTokenSpan tok of
    SourceSpan {sourceSpanStartCol = col} -> col
    NoSourceSpan -> 1

virtualSymbolToken :: Text -> SourceSpan -> LexToken
virtualSymbolToken sym span' =
  LexToken
    { lexTokenKind = case sym of
        "{" -> TkSpecialLBrace
        "}" -> TkSpecialRBrace
        ";" -> TkSpecialSemicolon
        _ -> error ("virtualSymbolToken: unexpected symbol " ++ T.unpack sym),
      lexTokenText = sym,
      lexTokenSpan = span',
      lexTokenOrigin = InsertedLayout
    }

utf8CharWidth :: Char -> Int
utf8CharWidth ch =
  case ord ch of
    code
      | code <= 0x7F -> 1
      | code <= 0x7FF -> 2
      | code <= 0xFFFF -> 3
      | otherwise -> 4
