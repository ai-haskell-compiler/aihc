module Text.Format.Format
  ( FmtItem(..)
  , Format(..)
  , Format1(..)
  ) where


import           Control.Arrow
import           Data.Char          (isDigit)
import qualified Data.List          as L
import           Data.String

import           Text.Format.ArgFmt
import           Text.Format.ArgKey
import           Text.Format.Error


data FmtItem = Lit String
             | Arg ArgKey ArgFmt
             deriving (Show, Eq)

{-| A data type indicates a format string

Format string contains "replacement fields" surrounded by curly braces {}.
Anything that is not contained in braces is considered literal text, which is
copied unchanged to the output. If you need to include a brace character in the
literal text, it can be escaped by doubling {{ and }}.


==== Format string syntax

  @
  format -> {chars | ("{" [key][":"fmt] "}")}
  key    -> \<see 'ArgKey'\>
  fmt    -> \<see 'ArgFmt'\>
  @

  Note: This library use a description language to describe syntax,
        see next section.

  Note: A key can be omitted only if there is no explict index key before it,
        it will be automatically caculated and inserted to the format string
        according to its position in the omitted key sequence.

  Examples

    >>> "I like {coffee}, I drink it everyday." :: Format
    >>> "{no:<20}    {name:<20}    {age}" :: Format
    >>> "{{\"no\": {no}, \"name\": \"{name}\"}}" :: Format


==== Syntax description language

  A syntax expr may contain a list of fields as followings

  @
  identifier                       identifier of an expr
  \<description\>                    use natural language as an expr
  ->                               use right hand expr to describe identifier
  ()                               a required field, may be omitted
  []                               an optional field
  {}                               repeat any times of the field
  |                                logical or, choice between left and right
  ""                               literal text
  @

  Built-in exprs

  @
  char  -> \<any character\>
  chars -> {char}
  int   -> \<integer without sign\>
  @
-}
newtype Format = Format { unFormat :: [FmtItem] } deriving (Show, Eq)

instance IsString Format where
  fromString cs = Format $ fixIndex 0 $ parse cs
    where
      stack :: String -> String
      stack = reverse . (`drop` (reverse cs)) . length

      parse :: String -> [FmtItem]
      parse "" = []
      parse cs =
        case parseLiteral cs of
          ("", _)   ->
            case parseArg cs of
              (cs1, Just cs2, cs3) ->
                (Arg (read cs1) (read cs2)) : (parse cs3)
              _ -> errorNoParse $ stack ""
          (ls, cs1) -> (Lit ls) : (parse cs1)

      parseLiteral :: String -> (String, String)
      parseLiteral ""               = ("", "")
      parseLiteral ('{' : '{' : cs) = first ('{' :) (parseLiteral cs)
      parseLiteral ('}' : '}' : cs) = first ('}' :) (parseLiteral cs)
      parseLiteral ('{' : cs)       = ([], '{' : cs)
      parseLiteral ('}' : cs)       = ([], '}' : cs)
      parseLiteral (c : cs)         = first (c :) (parseLiteral cs)

      parseArg :: String -> (String, Maybe String, String)
      parseArg cs@('{' : '{' : _) = ("", Nothing, cs)
      parseArg ('{' : cs) =
        case parseArgKey cs of
          (cs1, '}' : cs2) -> (cs1, Just "", cs2)
          (cs1, ':' : cs2) ->
            case parseArgFmt 0 cs2 of
              (cs11, '}' : cs12) -> (cs1, Just cs11, cs12)
              _                  -> errorCloseTag $ stack cs2
          _ -> errorCloseTag $ stack cs

      parseArgKey :: String -> (String, String)
      parseArgKey ""           = ("", "")
      parseArgKey cs@('}' : _) = ("", cs)
      parseArgKey cs@(':' : _) = ("", cs)
      parseArgKey (c : cs)     = first (c :) (parseArgKey cs)

      parseArgFmt :: Int -> String -> (String, String)
      parseArgFmt _ ""           = ("", "")
      parseArgFmt 0 cs@('}' : _) = ("", cs)
      parseArgFmt n ('{' : cs)   = first ('{' :) (parseArgFmt (n + 1) cs)
      parseArgFmt n ('}' : cs)   = first ('}' :) (parseArgFmt (n - 1) cs)
      parseArgFmt n (c : cs)     = first (c :) (parseArgFmt n cs)

      fixIndex :: Int -> [FmtItem] -> [FmtItem]
      fixIndex _ [] = []
      -- fixing empty key
      fixIndex next ((Arg key fmt) : items)
        | key == mempty = (Arg (Index next) fmt) : fixIndex (next + 1) items
      -- once there is an explict indexed key, stop fixing
      fixIndex next items@((Arg (Index _) _) : _) = items
      fixIndex next (item : items) = item : fixIndex next items


-- | A variant of 'Format',
-- it transforms all argument's key to __Nest (Index 0) key__
newtype Format1 = Format1 { unFormat1 :: [FmtItem] } deriving (Show, Eq)

instance IsString Format1 where
  fromString = Format1 . map zeroKey . unFormat . fromString
    where
      zeroKey :: FmtItem -> FmtItem
      zeroKey (Arg key fmt) = Arg (Nest (Index 0) key) fmt
      zeroKey item          = item
