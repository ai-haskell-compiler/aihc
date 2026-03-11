{-# LANGUAGE OverloadedStrings #-}
module Language.REST.ProofGen where

import qualified Data.HashMap.Strict as M
import qualified Data.List as L
import qualified Data.Text as T
import Text.Printf

import Language.REST.Path
import Language.REST.Internal.Rewrite
import Language.REST.RuntimeTerm
import Language.REST.Op

-- Hardcoded
opToLH :: Op -> String
opToLH (Op "union") = "mp"
opToLH (Op "toMS")  = "multiset_of"
opToLH (Op op) = T.unpack op

withParens :: Bool -> String -> String
withParens True t = "(" ++ t ++ ")"
withParens False t = t

toLH :: Bool -> RuntimeTerm -> String
-- Hardcoded rules
toLH parens (App "m" [arg]) = withParens parens $ printf "Multiset [%s]" (toLH False arg)
toLH parens (App "cons" [x, xs]) = withParens parens $ printf "%s:%s" (toLH True x) (toLH True xs)

toLH _ (App op [])   = opToLH op
toLH parens (App op args) =
  withParens parens $ printf "%s %s" (opToLH op) (unwords $ map (toLH True) args)

toProof :: Path Rewrite RuntimeTerm a -> String
toProof (steps, PathTerm result _) = "    " ++ L.intercalate "\n=== " (proofSteps ++ [toLH False result]) ++ "\n*** QED"
  where
    proofSteps :: [String]
    proofSteps = zipWith (curry proofStep) steps [0..]

    proofStep (Step (PathTerm t _) _ _ True, _)                       = toLH False t
    proofStep (Step (PathTerm t _) (Rewrite lhs rhs name) _ False, i) = toLH False t ++ " ? " ++ toLemma lemma
      where
        lemma = go (subTerms t)

        lemmaName = maybe "lemma" T.pack name

        toLemma s = toLH False (App (Op lemmaName) (map snd $ L.sort $ M.toList s))

        go []            = undefined
        go ((st, f): _) | Just su <- unify lhs st M.empty
                        , f (subst su rhs) == nextTerm
                        = su
        go (_:xs)       = go xs

        nextTerm = if i < (length steps - 1) then (pathTerm . term) (steps !! (i + 1)) else result
