{- |
Copyright   : (c) Takayuki Muranushi, 2016
License     : BSD3
Maintainer  : whosekiteneverfly@gmail.com
Stability   : experimental


Provides a interactive printer for printing Unicode characters in ghci REPL. Our design goal is that 'uprint' produces String representations that are valid Haskell 'String' literals and uses as many Unicode printable characters as possible. Hence

@
read . ushow == id
@

see the tests of this package for detailed specifications.

__Example__

With 'print' :

@
$ __ghci__
...
> __["哈斯克尔7.6.1"]__
["\\21704\\26031\\20811\\23572\\&7.6.1"]
>
@

With 'uprint' :

@
$ __ghci -interactive-print=Text.Show.Unicode.uprint Text.Show.Unicode__
...
Ok, modules loaded: Text.Show.Unicode.
> __("Хорошо!",["哈斯克尔7.6.1的力量","感じる"])__
("Хорошо!",["哈斯克尔7.6.1的力量","感じる"])
> "改\\n行"
"改\\n行"
@

You can make 'uprint' the default interactive printer in several ways. One is to
@cabal install unicode-show@, and add the following lines to your @~/.ghci@ config file.

@
import qualified Text.Show.Unicode
:set -interactive-print=Text.Show.Unicode.uprint
@

-}

module Text.Show.Unicode (ushow, uprint, urecover, ushowWith, uprintWith, urecoverWith) where

import           Control.Monad.Trans.State.Strict (StateT (StateT, runStateT),
                                                   get, put)
import           Data.Char                        (isAscii, isPrint)
import qualified Data.List                        as L
import qualified Data.Ord                         as O
import           Safe                             (minimumByMay)
import           Text.ParserCombinators.ReadP     (gather, readP_to_S)
import           Text.Read.Lex                    (lexChar)

-- | Create a parser for less dependencies. ReadP is too slow
type Parser a = StateT String Maybe a


-- | Show the input, and then replace Haskell character literals
-- with the character it represents, for any Unicode printable characters except backslash, single and double quotation marks.
-- If something fails, fallback to standard 'show'.
ushow :: Show a => a -> String
ushow = ushowWith shouldRecover

-- | Replace Haskell character literals with the character it represents, for
-- any Unicode printable characters except backslash, single and double
-- quotation marks.
urecover :: String -> String
urecover = urecoverWith shouldRecover

shouldRecover :: Char -> Bool
shouldRecover c = isPrint c && not (isAscii c)

-- | A version of 'print' that uses 'ushow'.
uprint :: Show a => a -> IO ()
uprint = putStrLn . ushow

-- | Show the input, and then replace character literals
-- with the character itself, for characters that satisfy the given predicate.
ushowWith :: Show a => (Char -> Bool) -> a -> String
ushowWith p = urecoverWith p . show

-- | Replace character literals with the character itself, for characters that
-- satisfy the given predicate.
urecoverWith :: (Char -> Bool) -> String -> String
urecoverWith p s =
  case runStateT (recoverChars p) s of
      Just (r, left) -> r ++ left
      Nothing        -> s


recoverChars :: (Char -> Bool) -> Parser String
recoverChars p = outsideLiteral
  where
    outsideLiteral = do
      notLit <- untilDoubleQuote
      rest <- get
      case rest of
          '\"' : inLiteral -> do
            put inLiteral
            (notLit ++) . ('\"' :) <$> insideLiteral
          _ ->
            return $ notLit ++ rest

    insideLiteral = do
      recovered <- recoverCharInLiteral
      case recovered of
          ("\"",'"') -> ('"' :) <$> outsideLiteral
          (s, c)
            | p c -> (c :) <$> insideLiteral
            | otherwise -> (s ++) <$> insideLiteral

    untilDoubleQuote = StateT $ Just . L.break (== '\"')


-- | Parse one Haskell character literal expression from a 'String' produced by 'show', and
--   returns the pair of the string before parsed with the parsed character.
--  * Note that special delimiter sequence "\&" may appear in a string. c.f.  <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6 Section 2.6 of the Haskell 2010 specification>.
recoverCharInLiteral :: Parser (String, Char)
recoverCharInLiteral = StateT $ \s ->
      let result = readP_to_S (gather lexChar) s
          -- The longest match result should leave the shortest string.
          -- So choose the result with the minimum length left.
       in minimumByMay (O.comparing (length . snd)) result


-- | A version of 'print' that uses 'ushowWith'.
uprintWith :: Show a => (Char -> Bool) -> a -> IO ()
uprintWith p = putStrLn . ushowWith p
