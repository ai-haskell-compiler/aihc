{

{-# OPTIONS_GHC -w #-}
{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Language.Lua.Annotated.Lexer
  ( llex
  , llexNamed
  , llexNamedWithWhiteSpace
  , llexFile
  , SourcePos(..)
  , SourceRange(..)
  , dropWhiteSpace
  , Lexeme(..)
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Language.Lua.Token
import           Language.Lua.LexerUtils
import           AlexTools

}

$letter      = [a-zA-Z_]                -- first letter of variables
$identletter = [a-zA-Z_0-9]             -- letters for rest of variables

$digit    = 0-9                         -- decimal digits
$hexdigit = [0-9a-fA-F]                 -- hexadecimal digits

$dqstr    = . # [ \" \\ ]               -- valid character in a string literal with dquotes
$sqstr    = . # [ \' \\ ]               -- valid character in a string literal with quotes
$longstr  = [ . \n ]                    -- valid character in a long string

-- escape characters
@charesc   = \\ ([ntvbrfa\\"'] | $digit{1,3} | x$hexdigit{2} | u\{$hexdigit+\} | \n | z $white*)

@digits    = $digit+
@hexdigits = $hexdigit+

@mantpart = @digits | @digits \. @digits | @digits \. | \. @digits
@exppart  = [eE][\+\-]? @digits

@hexprefix   = 0x | 0X
@mantparthex = @hexdigits | @hexdigits \. @hexdigits | @hexdigits \. | \. @hexdigits
@expparthex  = [pP][\+\-]? @hexdigits

tokens :-

    <0> $white+    { lexeme TokWhiteSpace }

    -- keywords
    <0> "and"      { lexeme TokAnd }
    <0> "break"    { lexeme TokBreak }
    <0> "do"       { lexeme TokDo }
    <0> "else"     { lexeme TokElse }
    <0> "elseif"   { lexeme TokElseIf }
    <0> "end"      { lexeme TokEnd }
    <0> "false"    { lexeme TokFalse }
    <0> "for"      { lexeme TokFor }
    <0> "function" { lexeme TokFunction }
    <0> "goto"     { lexeme TokGoto }
    <0> "if"       { lexeme TokIf }
    <0> "in"       { lexeme TokIn }
    <0> "local"    { lexeme TokLocal }
    <0> "nil"      { lexeme TokNil }
    <0> "not"      { lexeme TokNot }
    <0> "or"       { lexeme TokOr }
    <0> "repeat"   { lexeme TokRepeat }
    <0> "return"   { lexeme TokReturn }
    <0> "then"     { lexeme TokThen }
    <0> "true"     { lexeme TokTrue }
    <0> "until"    { lexeme TokUntil }
    <0> "while"    { lexeme TokWhile }

    -- identifiers
    <0> $letter $identletter* { lexeme TokIdent }

    -- number literals
    <0> @digits                              { lexeme TokInt   }
    <0> @hexprefix @digits                   { lexeme TokInt   }
    <0> @mantpart @exppart?                  { lexeme TokFloat }
    <0> @hexprefix @mantparthex @expparthex? { lexeme TokFloat }

    <0> \'                   { enterString SingleQuote }
    <state_sstring> @charesc ;
    <state_sstring> $sqstr   ;
    <state_sstring> \'       { endMode }

    -- string literals
    <0> \"                   { enterString DoubleQuote }
    <state_dstring> @charesc ;
    <state_dstring> $dqstr   ;
    <state_dstring> \"       { endMode }

    -- long strings
    <0> \[ =* \[              { enterLongString }
    <state_lstring> \] =* \] / { endStringPredicate } { endMode }
    <state_lstring> $longstr  ;

    -- comments
    <0> "--" \[ =* \[       { enterLongComment }
    <0> "--"                { enterComment }
    <state_comment> .*      { endMode }

    -- operators
    <0> "+"   { lexeme TokPlus }
    <0> "-"   { lexeme TokMinus }
    <0> "*"   { lexeme TokStar }
    <0> "/"   { lexeme TokSlash }
    <0> "//"  { lexeme TokDSlash }
    <0> "%"   { lexeme TokPercent }
    <0> "^"   { lexeme TokExp }
    <0> "#"   { lexeme TokSh }
    <0> "=="  { lexeme TokEqual }
    <0> "~="  { lexeme TokNotequal }
    <0> "<="  { lexeme TokLEq }
    <0> ">="  { lexeme TokGEq }
    <0> "<"   { lexeme TokLT }
    <0> ">"   { lexeme TokGT }
    <0> "="   { lexeme TokAssign }
    <0> "("   { lexeme TokLParen }
    <0> ")"   { lexeme TokRParen }
    <0> "{"   { lexeme TokLBrace }
    <0> "}"   { lexeme TokRBrace }
    <0> "["   { lexeme TokLBracket }
    <0> "]"   { lexeme TokRBracket }
    <0> "::"  { lexeme TokDColon }
    <0> ";"   { lexeme TokSemic }
    <0> ":"   { lexeme TokColon }
    <0> ","   { lexeme TokComma }
    <0> "."   { lexeme TokDot }
    <0> ".."  { lexeme TokDDot }
    <0> "..." { lexeme TokEllipsis }
    <0> "&"   { lexeme TokAmpersand }
    <0> "|"   { lexeme TokPipe }
    <0> "~"   { lexeme TokTilde }
    <0> "<<"  { lexeme TokDLT }
    <0> ">>"  { lexeme TokDGT }

    <state_sstring,state_dstring> \\ .  { invalidEsc }
    <state_sstring,state_dstring> \n { unterminatedString }

    <0> .                              { invalidChar }

{

lexerTools :: Input -> [Lexeme Token]
lexerTools = $makeLexer LexerConfig
  { lexerInitialState = NormalMode
  , lexerEOF = \_ _ -> []
  , lexerStateMode = \mode ->
       case mode of
         NormalMode        -> 0
         CommentMode    {} -> state_comment
         QuoteMode      {} -> state_lstring
         StringMode SingleQuote _ _ -> state_sstring
         StringMode DoubleQuote _ _ -> state_dstring
  }

alexGetByte = makeAlexGetByte (fromIntegral . min 127 . fromEnum)

-- | Lua lexer with default @=<string>@ name.
llex :: Text {- ^ chunk -} -> [Lexeme Token]
llex = llexNamed "=<string>"


-- | Lua lexer with explicit name.
llexNamed ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [Lexeme Token]
llexNamed name chunk = dropWhiteSpace (llexNamedWithWhiteSpace name chunk)


-- | Lua lexer with explicit name, preseves white space and comments.
llexNamedWithWhiteSpace ::
  String {- ^ name -} ->
  Text   {- ^ chunk -} ->
  [Lexeme Token]
llexNamedWithWhiteSpace name chunk =
   lexerTools (initialInput (Text.pack name) (dropSpecialComment chunk))


-- | Run Lua lexer on a file.
llexFile :: FilePath -> IO [Lexeme Token]
llexFile fp = fmap (llexNamed fp) (Text.readFile fp)

}
