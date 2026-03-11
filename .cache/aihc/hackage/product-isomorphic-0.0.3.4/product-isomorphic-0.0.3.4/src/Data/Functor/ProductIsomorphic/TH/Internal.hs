{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Data.Functor.ProductIsomorphic.TH.Internal
-- Copyright   : 2017-2018 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module defines templates to make product constructors.
module Data.Functor.ProductIsomorphic.TH.Internal (
  defineProductConstructor, defineTupleProductConstructor,

  reifyRecordType,
  ) where

import Control.Applicative ((<|>))
import Language.Haskell.TH
  (Q, Name, tupleTypeName, Info (..), reify,
   TypeQ, arrowT, appT, conT, varT,
   Dec, ExpQ, conE, Con (..), nameBase,)
import Language.Haskell.TH.Compat.Data (unDataD, unNewtypeD, unTyVarBndr)
import Data.List (foldl')

import Data.Functor.ProductIsomorphic.Unsafe (ProductConstructor (..))


recordInfo' :: Info -> Maybe (((TypeQ, [Name]), ExpQ), (Maybe [Name], [TypeQ]))
recordInfo' =  d  where
  d (TyConI tcon) = do
    (tcn, bs, r)  <-
      do (_cxt, tcn, bs, _mk, [r], _ds)  <-  unDataD tcon
         Just (tcn, bs, r)
      <|>
      do (_cxt, tcn, bs, _mk,  r , _ds)  <-  unNewtypeD tcon
         Just (tcn, bs, r)
    let vns = map tyVarName bs
    case r of
      NormalC dcn ts   -> Just (((buildT tcn vns, vns), conE dcn), (Nothing, [return t | (_, t) <- ts]))
      RecC    dcn vts  -> Just (((buildT tcn vns, vns), conE dcn), (Just ns, ts))
        where (ns, ts) = unzip [(n, return t) | (n, _, t) <- vts]
      _                -> Nothing
  d _                  =  Nothing
  tyVarName = fst . unTyVarBndr
  buildT tcn vns = foldl' appT (conT tcn) [ varT vn | vn <- vns ]

-- | Low-level reify interface for record type name.
reifyRecordType :: Name -> Q (((TypeQ, [Name]), ExpQ), (Maybe [Name], [TypeQ]))
reifyRecordType recTypeName =
  maybe
  (fail msgOnErr)
  return
  . recordInfo' =<< reify recTypeName
  where
    recTypeNameS = show recTypeName
    recTypeNameB = nameBase recTypeName
    msgOnErr =
      "Valid record type constructor not found: " ++ recTypeNameS ++ ".\n"
      ++ "    Possible causes:\n"
      ++ "      - " ++ recTypeNameB ++ " is not a type name.\n"
      ++ "        (Type name must be prefixed with double-single-quotes: e.g. ''" ++ recTypeNameB ++ ")\n"
      ++ "      - " ++ recTypeNameB ++ " has multiple data constructors.\n"
      ++ "        (Currently, only types with exactly *one* data constructors are supported)\n"

-- | Make template of ProductConstructor instance from type constructor name.
defineProductConstructor :: Name     -- ^ name of product or record type constructor
                         -> Q [Dec]  -- ^ result template
defineProductConstructor tyN = do
  (((tyQ, _), dtQ), (_, colts))  <- reifyRecordType tyN
  [d| instance ProductConstructor $(foldr (appT . (arrowT `appT`)) tyQ colts) where
        productConstructor = $(dtQ)
    |]

-- | Make template of ProductConstructor instance of tuple type.
defineTupleProductConstructor :: Int     -- ^ n-tuple
                              -> Q [Dec] -- ^ result template
defineTupleProductConstructor =
  defineProductConstructor . tupleTypeName
