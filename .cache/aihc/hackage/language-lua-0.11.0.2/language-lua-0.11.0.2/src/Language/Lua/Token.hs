module Language.Lua.Token where

-- | Lua tokens
data Token
  = TokPlus               -- ^+
  | TokMinus              -- ^\-
  | TokStar               -- ^\*
  | TokSlash              -- ^/
  | TokPercent            -- ^%
  | TokExp                -- ^^
  | TokSh                 -- ^#
  | TokEqual              -- ^==
  | TokNotequal           -- ^~=
  | TokLEq                -- ^<=
  | TokGEq                -- ^\>=
  | TokLT                 -- ^<
  | TokGT                 -- ^\>
  | TokAssign             -- ^=
  | TokLParen             -- ^(
  | TokRParen             -- ^)
  | TokLBrace             -- ^{
  | TokRBrace             -- ^}
  | TokLBracket           -- ^\[
  | TokRBracket           -- ^]
  | TokDColon             -- ^::
  | TokSemic              -- ^;
  | TokColon              -- ^:
  | TokComma              -- ^,
  | TokDot                -- ^.
  | TokDDot               -- ^..
  | TokEllipsis           -- ^...
  | TokDLT                -- ^<<
  | TokDGT                -- ^\>\>
  | TokAmpersand          -- ^&
  | TokPipe               -- ^|
  | TokDSlash             -- ^//
  | TokTilde              -- ^~

  | TokAnd                -- ^and
  | TokBreak              -- ^break
  | TokDo                 -- ^do
  | TokElse               -- ^else
  | TokElseIf             -- ^elseif
  | TokEnd                -- ^end
  | TokFalse              -- ^false
  | TokFor                -- ^for
  | TokFunction           -- ^function
  | TokGoto               -- ^goto
  | TokIf                 -- ^if
  | TokIn                 -- ^in
  | TokLocal              -- ^local
  | TokNil                -- ^nil
  | TokNot                -- ^not
  | TokOr                 -- ^or
  | TokRepeat             -- ^repeat
  | TokReturn             -- ^return
  | TokThen               -- ^then
  | TokTrue               -- ^true
  | TokUntil              -- ^until
  | TokWhile              -- ^while

  | TokFloat              -- ^floating point number constant
  | TokInt                -- ^integer number constant
  | TokSLit               -- ^string constant
  | TokIdent              -- ^identifier

  | TokWhiteSpace         -- ^white space
  | TokComment            -- ^comment

  | TokUntermString       -- ^ unterminated string
  | TokUntermComment      -- ^ unterminated comment
  | TokUnexpected         -- ^ unexpected character
    deriving Eq

instance Show Token where
    show TokPlus          = "`+`"
    show TokMinus         = "`-`"
    show TokStar          = "`*`"
    show TokSlash         = "`/`"
    show TokPercent       = "`%`"
    show TokExp           = "`^`"
    show TokSh            = "`#`"
    show TokEqual         = "`==`"
    show TokNotequal      = "`~=`"
    show TokLEq           = "`<=`"
    show TokGEq           = "`>=`"
    show TokLT            = "`<`"
    show TokGT            = "`>`"
    show TokAssign        = "`=`"
    show TokLParen        = "`(`"
    show TokRParen        = "`)`"
    show TokLBrace        = "`{`"
    show TokRBrace        = "`}`"
    show TokLBracket      = "`[`"
    show TokRBracket      = "`]`"
    show TokDColon        = "`::`"
    show TokSemic         = "`;`"
    show TokColon         = "`:`"
    show TokComma         = "`,`"
    show TokDot           = "`.`"
    show TokDDot          = "`..`"
    show TokEllipsis      = "`...`"
    show TokDLT           = "`<<`"
    show TokDGT           = "`>>`"
    show TokAmpersand     = "`&`"
    show TokPipe          = "`|`"
    show TokDSlash        = "`//`"
    show TokTilde         = "`~`"

    show TokAnd           = "`and`"
    show TokBreak         = "`break`"
    show TokDo            = "`do`"
    show TokElse          = "`else`"
    show TokElseIf        = "`elseif`"
    show TokEnd           = "`end`"
    show TokFalse         = "`false`"
    show TokFor           = "`for`"
    show TokFunction      = "`function`"
    show TokGoto          = "`goto`"
    show TokIf            = "`if`"
    show TokIn            = "`in`"
    show TokLocal         = "`local`"
    show TokNil           = "`nil`"
    show TokNot           = "`not`"
    show TokOr            = "`or`"
    show TokRepeat        = "`repeat`"
    show TokReturn        = "`return`"
    show TokThen          = "`then`"
    show TokTrue          = "`true`"
    show TokUntil         = "`until`"
    show TokWhile         = "`while`"

    show TokWhiteSpace    = "white_space"
    show TokComment       = "comment"

    show TokFloat         = "float"
    show TokInt           = "int"
    show TokSLit          = "string"
    show TokIdent         = "identifier"

    show TokUntermString  = "unterminated_string"
    show TokUntermComment = "unterminated_comment"
    show TokUnexpected    = "unexpected_character"
