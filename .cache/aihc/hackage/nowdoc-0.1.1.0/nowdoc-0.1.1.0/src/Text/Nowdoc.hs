{- |

Module:		Text.Nowdoc
Description:	3 QuasiQuoter: nowdoc, txtfile, binfile
Copyright:	(c) Yoshikuni Jujo, 2018
License:	BSD-3-Clause
Maintainer:	PAF01143@nifty.ne.jp

With QuasiQuotes language extension.

-}

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Text.Nowdoc (nowdoc, txtfile, binfile) where

import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import Language.Haskell.TH (ExpQ, stringE, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Data.ByteString.Char8 as BSC

{- |
Simplest here document. Only two transformation.

* remove head newline if exist
* remove one space from '|', space, space, ..., ']'

@
main :: IO ()
main = putStrLn [nowdoc|hello|]
@

@
main :: IO ()
main = putStr [nowdoc|
Hello, world!
|]
@

@
main :: IO ()
main = putStr [nowdoc|
main :: IO ()
main = putStr [nowdoc|
Hello, world!
| ]
|]
@
-}

nowdoc :: QuasiQuoter
nowdoc = qq { quoteExp = stringE . unescape . \case '\n' : cs -> cs; cs -> cs }

unescape :: String -> String
unescape ('|' : cs) = case span (== ' ') cs of
	(_ : ss, ']' : cs') -> '|' : ss ++ "]" ++ unescape cs'
	(ss, cs') -> '|' : ss ++ unescape cs'
unescape (c : cs) = c : unescape cs
unescape "" = ""

{- |
QuasiQuoter txtfile incerts file contents as string without transformation.
It read file as text file (with default encoding on your system).

@
main :: IO ()
main = putStr [txtfile|foo.txt|]
@
-}

txtfile :: QuasiQuoter
txtfile = qq { quoteExp = toQ readFile }

toQ :: (FilePath -> IO String) -> FilePath -> ExpQ
toQ rf fp_ = do
	cnt <- runIO $ rf fp
	addDependentFile fp
	stringE cnt
	where fp = dropBoth isSpace fp_

{- |
QuasiQuoter binfile incerts file contents as string without transformation.
It read file as binary file.

@
main :: IO ()
main = print [binfile|foo.dat|]
@
-}

binfile :: QuasiQuoter
binfile = qq { quoteExp = toQ $ (BSC.unpack <$>) . BSC.readFile }

-- ERROR MESSAGE

qq :: QuasiQuoter
qq = QuasiQuoter {
	quoteExp = undefined,
	quotePat = const $ err "pattern",
	quoteType = const $ err "type",
	quoteDec = const $ err "declaration" }

err :: String -> a
err ctx = error $
	"You have used the `nowdoc' QuasiQoter in a " ++ ctx ++
	" context; you must only use it in an expression context"

-- TOOLS

dropBoth :: (a -> Bool) -> [a] -> [a]
dropBoth = (.) <$> dropWhile <*> dropWhileEnd
