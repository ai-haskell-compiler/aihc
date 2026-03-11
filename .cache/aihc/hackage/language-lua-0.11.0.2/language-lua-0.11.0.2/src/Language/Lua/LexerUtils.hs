{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Lua.LexerUtils where

import           Data.Text (Text)
import qualified Data.Text as Text

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative (Applicative(..))
#endif

import           Language.Lua.Token
import           AlexTools

-- | Lua token with position information.
data LexToken = LexToken
  { ltokToken  :: Token
  , ltokRange  :: SourceRange
  , ltokLexeme :: Text
  } deriving (Show)

-- | Lexer mode
data Mode
  = NormalMode

  | StringMode StringMode [SourceRange] Input
    -- ^ string type, errors, input at start

  | CommentMode Input
    -- ^ Single line comment. Input at beginning of comment

  | QuoteMode Input -- input at beginning of long-quote
              Int       -- delim length
              Bool      -- is comment
                -- ^ start delimlen iscomment

data StringMode = SingleQuote | DoubleQuote


-- | This is called when we encounter the end of a line before seeing
-- the closing character for a string.
unterminatedString :: Action Mode [Lexeme Token]
unterminatedString =
  do ~(StringMode _strTy _errs inp0) <- getLexerState
     setLexerState NormalMode
     longToken inp0 TokUntermString


-- | An unknown character in "normal mode"
invalidChar :: Action Mode [Lexeme Token]
invalidChar =
  do setLexerState NormalMode
     lexeme TokUnexpected

-- | A a bad escape withing a string
invalidEsc :: Action Mode [Lexeme Token]
invalidEsc =
  do inp1 <- startInput
     inp2 <- endInput
     ~(StringMode m errs inp0) <- getLexerState
     let err = SourceRange
                 { sourceFrom = inputPos inp1
                 , sourceTo   = inputPrev inp2
                 }
     setLexerState (StringMode m (err : errs) inp0)
     return []

checkEOF :: Mode -> Input -> [Lexeme Token]
checkEOF mode Input { inputPrev = end } =
  case mode of
    NormalMode {}         -> []
    CommentMode {}        -> []

    QuoteMode inp _ True  -> ret TokUntermComment  inp
    QuoteMode inp _ _     -> ret TokUntermString   inp
    StringMode _ _ inp     -> ret TokUntermString inp

  where
  ret t Input { inputPos = start, inputText = rest } =
    [ Lexeme { lexemeToken = t
             , lexemeRange = SourceRange { sourceFrom = start, sourceTo = end }
             , lexemeText  = rest
             } ]

-- | Start lexing a long-quoted string literal
enterLongString :: Action Mode [Lexeme Token]
enterLongString =
  do inp <- startInput
     len <- matchLength
     setLexerState (QuoteMode inp len False)
     return []

-- | Start lexing of a string literal
enterString :: StringMode -> Action Mode [Lexeme Token]
enterString sm =
  do inp <- startInput
     setLexerState (StringMode sm [] inp)
     return []

-- | Start lexing a long-quoted comment
enterLongComment :: Action Mode [Lexeme Token]
enterLongComment =
  do inp <- startInput
     len <- matchLength
     setLexerState (QuoteMode inp (len - 2) True)
     return []

-- | Start lexing a single-line comment
enterComment :: Action Mode [Lexeme Token]
enterComment =
  do inp <- startInput
     setLexerState (CommentMode inp)
     return []

-- | Construct a lexeme spanning multiple matches
longToken ::
  Input {- ^ input from the mode       -} ->
  Token {- ^ token for lexeme          -} ->
  Action Mode [Lexeme Token]
longToken Input { inputPos  = start, inputText = text } t =
  do Input { inputPrev = end } <- endInput
     let lexLen = 1 + sourceIndex end - sourceIndex start
     return [Lexeme
              { lexemeToken  = t
              , lexemeRange  = SourceRange { sourceFrom = start, sourceTo = end }
              , lexemeText   = Text.take lexLen text
              } ]

-- | The closing delimiter for long-quoted lexemes must be the same length as
-- the opening delimiter. This predicate checks if the currently match
-- delimiter is the right length.
endStringPredicate ::
  Mode      {- ^ lexer mode                    -} ->
  Input {- ^ input stream before the token -} ->
  Int       {- ^ length of the token           -} ->
  Input {- ^ input stream after the token  -} ->
  Bool      {- ^ is expected ending long-quote -}
endStringPredicate mode _ len _ =
  case mode of
    QuoteMode _ startlen _ -> len == startlen
    _                      -> False

-- | Action called at the end of a lexer-sub mode.
endMode :: Action Mode [Lexeme Token]
endMode =
  do mode <- getLexerState
     setLexerState NormalMode
     case mode of
       StringMode _ err inp        -> longToken inp
                                    $ if null err then TokSLit
                                                  else TokUnexpected
       CommentMode inp             -> longToken inp TokComment
       QuoteMode   inp _ isComment -> longToken inp
                                    $ if isComment then TokComment
                                                   else TokSLit
       NormalMode -> error "endMode: internal lexer error"

-- | Drop the first line of a Lua file when it starts with a '#'
dropSpecialComment :: Text -> Text
dropSpecialComment text
  | "#" `Text.isPrefixOf` text = Text.dropWhile (/='\n') text
  | otherwise = text
-- Newline is preserved in order to ensure that line numbers stay correct

-- | This function drops whitespace and comments from a list of lexemes
-- in order to make it suitable for parsing.
dropWhiteSpace :: [Lexeme Token] -> [Lexeme Token]
dropWhiteSpace = filter (not . isWhite . lexemeToken)
  where
  isWhite TokWhiteSpace = True
  isWhite TokComment    = True
  isWhite _             = False
