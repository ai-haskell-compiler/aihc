{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Quasiquotation for byte builders.
module Data.Bytes.Builder.Template
  ( bldr
  ) where

import Control.Monad (when)
import Data.Bytes.Builder.Class (toBuilder)
import GHC.Ptr (Ptr (Ptr))
import Language.Haskell.Meta.Parse (parseExp)
import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH.Lib (integerL, litE, stringPrimL)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import qualified Data.ByteString.Short as SBS
import qualified Data.Bytes.Builder as Builder
import qualified Data.Text.Short as TS
import qualified Language.Haskell.TH as TH

{- | A quasiquoter for builders. Haskell expressions are interpolated
with backticks, and the @ToBuilder@ class is used to convert them
to builders. Several common escape sequences for whitespace and
control characters are recongized. Consider the following expression,
where the binding @partition@ has type @Word32@:

> [templ|[WARN] Partition `partition` has invalid data.\n|]

This expression has type @Builder@ and expands to:

> Builder.cstringLen (Ptr "[WARN] Partition "#, 17) <>
> Builder.toBuilder partition <>
> Builder.cstringLen (Ptr " has invalid data.\n"#, 19)

The @ToBuilder@ instance for @Word32@ uses decimal encoding, so this
would result in the following if @partition@ was 42 (with a newline
character at the end):

> [WARN] Partition 42 has invalid data.

In the future, a more sophisticated @bbldr@ variant will be added
that will support expressions where the maximum length of the entire
builder can be computed at compile time.
-}
bldr :: QuasiQuoter
bldr =
  QuasiQuoter
    { quoteExp = templExp
    , quotePat = notHandled "patterns"
    , quoteType = notHandled "types"
    , quoteDec = notHandled "declarations"
    }
 where
  notHandled things _ =
    fail $
      things ++ "are not handled by the byte template quasiquoter"

templExp :: String -> Q Exp
templExp inp = do
  checkOverloadedStrings
  rawParts <- case parse inp of
    Left err -> fail err
    Right [] -> fail "empty template"
    Right v -> pure v
  let expParts = compile <$> rawParts
  foldl1 (\e1 e2 -> [|$e1 <> $e2|]) expParts

checkOverloadedStrings :: Q ()
checkOverloadedStrings = do
  olEnabled <- TH.isExtEnabled TH.OverloadedStrings
  when (not olEnabled) $
    fail "Byte templates require the OverloadedStrings extension enabled."

type Template = [TemplPart]
data TemplPart
  = Literal String
  | Splice String

compile :: TemplPart -> Q Exp
compile (Literal lit) =
  let bytes = SBS.unpack . TS.toShortByteString . TS.pack $ lit
      strExp = litE . stringPrimL $ bytes
      strLen = litE . integerL . fromIntegral $ length bytes
   in [|Builder.cstringLen (Ptr $(strExp), $(strLen))|]
compile (Splice str) = case parseExp str of
  Left err -> fail err
  Right hs -> [|toBuilder $(pure hs)|]

parse :: String -> Either String Template
parse = partsLoop
 where
  partsLoop "" = do
    pure []
  partsLoop ('`' : inp) = do
    (!spl, !rest) <- spliceLoop inp
    (Splice spl :) <$> partsLoop rest
  partsLoop inp = do
    (!lit, !rest) <- litLoop "" inp
    (Literal lit :) <$> partsLoop rest
  litLoop :: String -> String -> Either String (String, String)
  litLoop !acc rest@"" = pure (reverse acc, rest)
  litLoop !acc rest@('`' : _) = pure (reverse acc, rest)
  litLoop !acc ('\\' : next) = do
    (c, rest) <- parseEscape next
    litLoop (c : acc) rest
  litLoop !acc (c : rest) = litLoop (c : acc) rest
  spliceLoop :: String -> Either String (String, String)
  spliceLoop inp = case break (== '`') inp of
    ([], _) -> Left "internal error"
    (hs, '`' : rest) -> pure (hs, rest)
    (_, _ : _) -> Left "internal error"
    (_, []) -> Left "unterminated interpolation"
  parseEscape :: String -> Either String (Char, String)
  parseEscape "" = Left "incomplete escape"
  parseEscape ('\\' : rest) = pure ('\\', rest)
  parseEscape ('`' : rest) = pure ('`', rest)
  parseEscape ('\'' : rest) = pure ('\'', rest)
  parseEscape ('\"' : rest) = pure ('\"', rest)
  parseEscape ('0' : rest) = pure ('\0', rest)
  parseEscape ('a' : rest) = pure ('\a', rest)
  parseEscape ('b' : rest) = pure ('\b', rest)
  parseEscape ('f' : rest) = pure ('\f', rest)
  parseEscape ('n' : rest) = pure ('\n', rest)
  parseEscape ('r' : rest) = pure ('\r', rest)
  parseEscape ('t' : rest) = pure ('\t', rest)
  parseEscape ('v' : rest) = pure ('\v', rest)
  parseEscape (c : _) = Left $ "unrecognized escape: \\" ++ [c]
