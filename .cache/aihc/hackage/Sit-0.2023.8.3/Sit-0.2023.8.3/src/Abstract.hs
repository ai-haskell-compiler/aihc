{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-typed-holes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | Auxiliary functions for abstract syntax.

module Abstract where

import Control.Monad

import Sit.Abs as A

import Lens

-- data AppView
--   = FixV [Exp]
--   | SetV [Exp]
--   | VarV [Exp]
--   | ZeroV [Exp]
--   | SucV  [Exp]

-- | Gather applications to expose head.

appView :: Exp -> (Exp, [Exp])
appView = \case
  App f e -> over _2 (++ [e]) $ appView f
  e -> (e, [])

-- | Can this expression only denote a type?

mustBeType :: Exp -> Bool
mustBeType = \case
  Nat      -> True
  Set      -> True
  Set1     -> True
  Set2     -> True
  Forall{} -> True
  Pi{}     -> True
  Arrow{}  -> True
  App f _  -> mustBeType f
  _ -> False

-- | Is this expression an introduction?

introduction :: Exp -> Bool
introduction = \case
  Var{}    -> False
  App{}    -> False
  Case{}   -> False
  Int{}    -> True
  Infty    -> True
  Size     -> True
  Nat      -> True
  Set      -> True
  Set1     -> True
  Set2     -> True
  Zero     -> True
  Suc      -> True
  Fix      -> True
  LZero    -> True
  LSuc     -> True
  Lam{}    -> True
  Forall{} -> True
  Pi{}     -> True
  Arrow{}  -> True
  Plus{}   -> True
  ELam{}   -> True

-- | Convert "identifier-or-underscore" to string.

fromIdU :: A.IdU -> String
fromIdU = \case
  A.Id (A.Ident x) -> x
  A.Under -> "_"

-- | Try to convert an expression to a list of A.IdU

parseIdUs :: A.Exp -> Maybe [A.IdU]
parseIdUs e = do
  let (f, es) = appView e
  forM (f : es) $ \case
    A.Var x -> Just x
    _ -> Nothing
