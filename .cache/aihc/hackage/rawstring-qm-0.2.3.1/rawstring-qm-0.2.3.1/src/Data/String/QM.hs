{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Data.String.QM
 ( qq
 , qm
 , qn
 , qt
 , qtl
 , qtb
 , module TT
 )
where

import           Prelude

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote

import           Data.ByteString.Char8                as Strict (ByteString,
                                                                 unpack)
import           Data.ByteString.Lazy.Char8           as Lazy (ByteString,
                                                               unpack)
import           Data.Char                            (isAlpha, isAlphaNum)
import           Data.Maybe
import           Data.Monoid                          (Monoid (..), (<>))
import           Data.Text                            as T (Text, unpack)
import qualified Data.Text.Internal.Builder           as B
import qualified Data.Text.Internal.Builder.Functions as B
import           Data.Text.Lazy                       as LazyT (Text, unpack)
import           GHC.Exts                             (IsString (..))
import qualified Language.Haskell.TH                  as TH

import           Data.Text.ToText                     as TT
import           Data.Text.ToTextBuilder              as TT

data StringPart = Literal String | AntiQuote String deriving Show


-- | qq is a block quote extension, it can be used anywhere you would put normal quotes
--   but you would require to have new line in them
--  if you put it as a pattern it will expan to 'a':'b':'c'...
qq :: QuasiQuoter
qq = QuasiQuoter
    { quoteExp  = ls
    , quotePat  = return . expandIntoCons
    , quoteType = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a type)"
    , quoteDec  = \_ -> fail "illegal raw string QuasiQuote (allowed as expression only, used as a declaration)"
}

-- | QuasiQuoter for interpolating '$var' and '${expr}' into a string literal.
--  var and expr are just Names
qm :: QuasiQuoter
qm = QuasiQuoter (makeExpr . parseQM [])
                 (error "Cannot use qm as a pattern")
                 (error "Cannot use qm as a type")
                 (error "Cannot use qm as a dec")

-- | QuasiQuoter for interpolating '${expr}' into a string literal.
--  var and expr are just Names
qn :: QuasiQuoter
qn = QuasiQuoter (makeExpr . parseQN [])
                 (error "Cannot use qm as a pattern")
                 (error "Cannot use qm as a type")
                 (error "Cannot use qm as a dec")

-- | QuasiQuoter for interpolating '${expr}' into strict text.
--  var and expr are just Names output is of type text vars are auto converted to text
qt :: QuasiQuoter
qt = QuasiQuoter (makeExprT . parseQN [])
                 (error "Cannot use qm as a pattern")
                 (error "Cannot use qm as a type")
                 (error "Cannot use qm as a dec")

-- | QuasiQuoter for interpolating '${expr}' into a lazy text.
--  var and expr are just Names type lazy text, vars are magically (via `ToText` typeclass) converted to text
qtl :: QuasiQuoter
qtl = QuasiQuoter (makeExprTL . parseQN [])
                 (error "Cannot use qm as a pattern")
                 (error "Cannot use qm as a type")
                 (error "Cannot use qm as a dec")

-- | QuasiQuoter for interpolating '${expr}' into a text builder.
--  var and expr are just Names type lazy text, vars are magically (via `ToTextBuilder` typeclass) converted to text
qtb :: QuasiQuoter
qtb = QuasiQuoter (makeExprTB . parseQN [])
                 (error "Cannot use qm as a pattern")
                 (error "Cannot use qm as a type")
                 (error "Cannot use qm as a dec")

parseQM a []           = [Literal (reverse a)]
parseQM a ('\\':x:xs)  = parseQM (x:a) xs
parseQM a "\\"         = parseQM ('\\':a) []

parseQM a ('$':'{':xs)     = Literal (reverse a) : unQM [] xs
parseQM a ('$':x:xs) | x == '_' || isAlpha x =
    Literal (reverse a) : AntiQuote (x:pre) : parseQM [] post
    where
    (pre, post) = span isIdent xs
parseQM a (x:xs)       = parseQM (x:a) xs


unQM a ('\\':x:xs) = unQM (x:a) xs
unQM a "\\"        = unQM ('\\':a) []
unQM a ('}':xs)    = AntiQuote (reverse a) : parseQM [] xs
unQM a (x:xs)      = unQM (x:a) xs



parseQN a []           = [Literal (reverse a)]
parseQN a ('\\':x:xs)  = parseQN (x:a) xs
parseQN a "\\"         = parseQN ('\\':a) []

parseQN a ('$':'{':xs) = Literal (reverse a) : unQN [] xs
-- parseQN a ('$':x:xs) | x == '_' || isAlpha x =
--    Literal (reverse a) : AntiQuote (x:pre) : parseQN [] post
--    where
--    (pre, post) = span isIdent xs
parseQN a (x:xs)       = parseQN (x:a) xs


unQN a ('\\':x:xs) = unQN (x:a) xs
unQN a "\\"        = unQN ('\\':a) []
unQN a ('}':xs)    = AntiQuote (reverse a) : parseQN [] xs
unQN a (x:xs)      = unQN (x:a) xs

makeExpr [] = ls ""
makeExpr (Literal a:xs)   = TH.appE [| (++) a |]
                            $ makeExpr xs
makeExpr (AntiQuote a:xs) = TH.appE [| (++) $(varE (mkName a)) |]
                            $ makeExpr xs

makeExprT [] = ls ""
makeExprT (Literal a:xs)   = TH.appE [| (<>) a |]
                            $ makeExprT xs
makeExprT (AntiQuote a:xs) = TH.appE [| (<>) (toText $(varE (mkName a))) |]
                            $ makeExprT xs

makeExprTL [] = ls ""
makeExprTL (Literal a:xs)   = TH.appE [| (<>) a |]
                            $ makeExprTL xs
makeExprTL (AntiQuote a:xs) = TH.appE [| (<>) (toLazyText $(varE (mkName a))) |]
                            $ makeExprTL xs

makeExprTB [] = ls ""
makeExprTB (Literal a:xs)   = TH.appE [| (B.<>) (B.fromLazyText a) |]
                                      $ makeExprTB xs
makeExprTB (AntiQuote a:xs) = TH.appE [| (B.<>) (toTextBuilder $(varE (mkName a))) |]
                                      $ makeExprTB xs

-- reify' = varE . mkName
-- reify

ls = return . TH.LitE . TH.StringL

isIdent '_'  = True
isIdent '\'' = True
isIdent x    = isAlphaNum x

-- Convert cons into pattern cons
expandIntoCons [c]    = LitP (CharL c)
expandIntoCons (c:cs) = InfixP (LitP (CharL c)) '(:) (expandIntoCons cs)
