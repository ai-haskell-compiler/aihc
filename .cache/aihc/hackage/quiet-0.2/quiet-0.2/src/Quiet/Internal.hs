-- Some Parts: Copyright 2010, Universiteit Utrecht, All Rights Reserved.
-- License: BSD3

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}

module Quiet.Internal (
    ConType(..)
  , QShow(..)
  , QRead(..)
  , expectInfix
  ) where

import           Data.Proxy (Proxy(..))

import           GHC.Generics ((:*:)(..), (:+:)(..))
import           GHC.Generics (Constructor(..))
import           GHC.Generics (Fixity(..))
import           GHC.Generics (U1(..), K1(..), M1(..), D, C, S)
import qualified GHC.Read as Read
import           GHC.Show (appPrec, appPrec1, showChar, showParen)

import           Text.ParserCombinators.ReadPrec (ReadPrec)
import qualified Text.ParserCombinators.ReadPrec as ReadPrec
import qualified Text.Read.Lex as Lex

--------------------------------------------------------------
-- ConType

data ConType =
    ConPrefix
  | ConInfix String

--------------------------------------------------------------
-- QShow

class QShow f where
  qshowsPrec_ :: ConType -> Int -> f a -> ShowS

  qshowsNullary :: f a -> Bool
  qshowsNullary _ =
    False

instance QShow U1 where
  qshowsPrec_ _ _ U1 =
    id

  qshowsNullary _ =
    True

instance Show c => QShow (K1 i c) where
  qshowsPrec_ _ n (K1 a) =
    showsPrec n a

instance (QShow a, Constructor c) => QShow (M1 C c a) where
  qshowsPrec_ _ n c@(M1 x) =
    let
      fixity =
        conFixity c

      t =
        case fixity of
          Prefix ->
            ConPrefix
          Infix _ _ ->
            ConInfix $ conName c
    in
      case fixity of
        Prefix ->
          showParen (n > appPrec && not (qshowsNullary x)) $
            showString (conName c) .
            if qshowsNullary x then id else showChar ' ' .
            qshowsPrec_ t appPrec1 x
        Infix _ m ->
          showParen (n > m) $ qshowsPrec_ t (m+1) x

instance QShow a => QShow (M1 S s a) where
  qshowsPrec_ t n (M1 x) =
    qshowsPrec_ t n x

  qshowsNullary (M1 x) =
    qshowsNullary x

instance QShow a => QShow (M1 D d a) where
  qshowsPrec_ t n (M1 x) =
    qshowsPrec_ t n x

instance (QShow a, QShow b) => QShow (a :+: b) where
  qshowsPrec_ t n = \case
    L1 x ->
      qshowsPrec_ t n x
    R1 x ->
      qshowsPrec_ t n x

instance (QShow a, QShow b) => QShow (a :*: b) where
  qshowsPrec_ t n (a :*: b) =
    case t of
      ConPrefix ->
        qshowsPrec_ t n a .
        showChar ' ' .
        qshowsPrec_ t n b
      ConInfix s ->
        let
          isInfixTypeCon = \case
            ':':_ ->
              True
            _ ->
              False

          showBacktick =
            if isInfixTypeCon s then
              id
            else
              showChar '`'
        in
          qshowsPrec_ t n a .
          showChar ' ' .
          showBacktick .
          showString s .
          showBacktick .
          showChar ' ' .
          qshowsPrec_ t n b

------------------------------------------------------------------------
-- QRead

class QRead f where
  qreadPrec_ :: ConType -> ReadPrec (f a)

  qreadNullary :: Proxy f -> Bool
  qreadNullary _ =
    False

instance QRead U1 where
  qreadPrec_ _ =
    pure U1

  qreadNullary _ =
    True

instance Read c => QRead (K1 i c) where
  qreadPrec_ _ =
    K1 <$> Read.readPrec

instance (QRead a, Constructor c) => QRead (M1 C c a) where
  qreadPrec_ _ =
    let
      proxy =
        Proxy @(M1 C c a)

      con =
        undefined :: M1 C c a p
    in
      Read.parens $
      case conFixity con of
        Prefix ->
          if qreadNullary proxy then do
            Read.expectP (Lex.Ident (conName con))
            M1 <$> ReadPrec.step (qreadPrec_ ConPrefix)

          else
            ReadPrec.prec appPrec $ do
              Read.expectP (Lex.Ident (conName con))
              M1 <$> ReadPrec.step (qreadPrec_ ConPrefix)

        Infix _ m ->
          ReadPrec.prec m $
            M1 <$> ReadPrec.step (qreadPrec_ (ConInfix (conName con)))

instance QRead a => QRead (M1 S s a) where
  qreadPrec_ t =
    M1 <$> qreadPrec_ t

  qreadNullary x =
    qreadNullary x

instance QRead a => QRead (M1 D d a) where
  qreadPrec_ t =
    M1 <$> qreadPrec_ t

instance (QRead a, QRead b) => QRead (a :+: b) where
  qreadPrec_ t =
    (L1 <$> qreadPrec_ t)
    ReadPrec.+++
    (R1 <$> qreadPrec_ t)

instance (QRead a, QRead b) => QRead (a :*: b) where
  qreadPrec_ t =
    Read.parens $
      case t of
        ConPrefix ->
          (:*:)
            <$> qreadPrec_ t
            <*> qreadPrec_ t

        ConInfix s ->
          (:*:)
            <$> qreadPrec_ t <* expectInfix s
            <*> qreadPrec_ t

expectInfix :: String -> ReadPrec ()
expectInfix = \case
  xs@(':':_) ->
    Read.expectP (Lex.Symbol xs)
  xs -> do
    Read.expectP (Lex.Punc "`")
    Read.expectP (Lex.Ident xs)
    Read.expectP (Lex.Punc "`")
