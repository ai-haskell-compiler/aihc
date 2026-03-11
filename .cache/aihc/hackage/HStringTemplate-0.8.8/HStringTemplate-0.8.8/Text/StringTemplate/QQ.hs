{-# LANGUAGE DeriveDataTypeable, QuasiQuotes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.StringTemplate.QQ
-- Copyright   :  (c) Sterling Clover 2009
-- License     :  BSD 3 Clause
-- Maintainer  :  s.clover@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides stmp, a quasi-quoter for StringTemplate expressions.
-- Quoted templates are guaranteed syntactically well-formed at compile time,
-- and antiquotation (of identifiers only) is provided by backticks.
-- Usage: @ let var = [0,1,2] in toString [$stmp|($\`var\`; separator = ', '$)|] === \"(0, 1, 2)\"@
-----------------------------------------------------------------------------

module Text.StringTemplate.QQ (stmp) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Text.StringTemplate.Base
import qualified Data.Set as S

quoteTmplExp :: String -> TH.ExpQ
quoteTmplPat :: String -> TH.PatQ

stmp :: QuasiQuoter
stmp = QuasiQuoter {quoteExp = quoteTmplExp, quotePat = quoteTmplPat}

quoteTmplPat = error "Cannot apply stmp quasiquoter in patterns"
quoteTmplExp s = return tmpl
  where
    vars = case parseSTMPNames ('$','$') s of
             Right (xs,_,_) -> xs
             Left  err -> fail $ show err
    base  = TH.AppE (TH.VarE (TH.mkName "Text.StringTemplate.newSTMP")) (TH.LitE (TH.StringL s))
    tmpl  = S.foldr addAttrib base $ S.fromList vars
    addAttrib var = TH.AppE
        (TH.AppE (TH.AppE (TH.VarE (TH.mkName "Text.StringTemplate.setAttribute"))
                          (TH.LitE (TH.StringL ('`' : var ++ "`"))))
                 (TH.VarE (TH.mkName  var)))
