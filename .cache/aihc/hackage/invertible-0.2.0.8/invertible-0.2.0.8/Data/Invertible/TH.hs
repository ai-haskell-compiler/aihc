-- |
-- Convenient construction of bidirectional functions using case-like syntax.
{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, Trustworthy #-}
module Data.Invertible.TH
  ( biCase
  ) where

import Control.Arrow (second)
import Control.Monad (liftM2)
import Data.Char (isSpace)
import Data.Data (Data, gmapT)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Data.Typeable (cast)
import Language.Haskell.Meta.Parse (parsePat)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
#if MIN_VERSION_base(4,9,0)
import Text.Read.Lex (isSymbolChar)
#endif

import Data.Invertible.Bijection

#if !MIN_VERSION_base(4,9,0)
isSymbolChar :: Char -> Bool
isSymbolChar = (`elem` "!#$%&*+./<=>?@\\^|-~:")
#endif

split :: String -> String -> [String]
split _ [] = []
split d (p:s)
  | not (isSymbolChar p)
  , Just (p':s') <- stripPrefix d s
  , not (isSymbolChar p') = [p] : conshead p' (split d s')
  | otherwise = conshead p $ split d s
  where
  conshead c [] = [[c]]
  conshead c (h:t) = (c:h):t

patToPat :: TH.Pat -> TH.Pat
patToPat = ptp . gmapT pta where
  pta :: Data a => a -> a
  pta = fromMaybe id $ cast patToPat
  ptp (TH.ViewP e p) = TH.ViewP (TH.VarE 'biTo `TH.AppE` e) p
  ptp p = p

patToExp :: TH.Pat -> TH.Exp
patToExp (TH.LitP l) = TH.LitE l
patToExp (TH.VarP v) = TH.VarE v
patToExp (TH.TupP l) = TH.TupE $ map (
#if MIN_VERSION_template_haskell(2,16,0)
  Just .
#endif
  patToExp) l
patToExp (TH.UnboxedTupP l) = TH.UnboxedTupE $ map (
#if MIN_VERSION_template_haskell(2,16,0)
  Just .
#endif
  patToExp) l
#if MIN_VERSION_template_haskell(2,12,0)
patToExp (TH.UnboxedSumP p a n) = TH.UnboxedSumE (patToExp p) a n
#endif
patToExp (TH.ConP c
#if MIN_VERSION_template_haskell(2,18,0)
  _ -- [Type], probably should have to be empty
#endif
  a) = foldl (\f -> TH.AppE f . patToExp) (TH.ConE c) a
patToExp (TH.InfixP l o r) = TH.InfixE (Just $ patToExp l) (TH.ConE o) (Just $ patToExp r)
patToExp (TH.UInfixP l o r) = TH.UInfixE (patToExp l) (TH.ConE o) (patToExp r)
patToExp (TH.ParensP p) = TH.ParensE $ patToExp p
patToExp (TH.TildeP p) = patToExp p
patToExp (TH.BangP p) = patToExp p
patToExp (TH.AsP _ p) = patToExp p
patToExp TH.WildP = TH.VarE 'undefined
patToExp (TH.RecP c f) = TH.RecConE c $ map (second patToExp) f
patToExp (TH.ListP l) = TH.ListE $ map patToExp l
patToExp (TH.SigP p t) = TH.SigE (patToExp p) t
patToExp (TH.ViewP e p) = TH.VarE 'biFrom `TH.AppE` e `TH.AppE` patToExp p

parseP :: String -> TH.PatQ
parseP s = either (fail . (++) ("Failed to parse pattern '" ++ s ++ "': ")) return $ parsePat s

biExp :: String -> TH.ExpQ
biExp = fmap ie . mapM ic . filter (not . all isSpace) . concatMap (split ";") . lines where
  ie l = TH.InfixE (Just $ ce l) (TH.ConE '(:<->:)) (Just $ ce $ map swap l)
  ce [(p, e)] = TH.LamE [patToPat p] $ patToExp e
  ce l = TH.LamCaseE [ TH.Match (patToPat p) (TH.NormalB $ patToExp e) [] | (p, e) <- l ]
  ic s
    | [fs, gs] <- split "<->" s =
      liftM2 (,) (parseP fs) (parseP gs)
    | otherwise = fail "each bijection case must contain exactly one '<->'"

-- |Construct an expression representing a function bijection based on a set of newline- or semicolon-separated cases.
-- Each case should be two pattern-expressions separated by @<->@.
-- Each pattern-expression is a haskell pattern that can also be interpreted as an expression.
-- You can think of these as symmetric or bidirectional case expressions.
-- The result will be a bijection that is the combination of two lambdas, one with the cases intepreted forward, and one reverse.
-- For example:
--
-- > newtype T a = C a
-- > biC :: T a <-> a
-- > biC = [biCase| C a <-> a |]
--
-- > isJust :: Maybe () <-> Bool
-- > isJust = [biCase|
-- >     Just () <-> True
-- >     Nothing <-> False
-- >   |]
-- 
biCase :: QuasiQuoter
biCase = QuasiQuoter
  { quoteExp = biExp
  , quoteType = const $ fail "biCase not supported in types"
  , quotePat = const $ fail "biCase not supported in patterns"
  , quoteDec = const $ fail "biCase not supported in declarations"
  }
