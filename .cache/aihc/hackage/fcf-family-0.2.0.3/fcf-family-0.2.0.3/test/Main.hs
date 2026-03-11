{-# LANGUAGE
  AllowAmbiguousTypes,
  CPP,
  DataKinds,
  PolyKinds,
  ScopedTypeVariables,
  TemplateHaskell,
  TypeApplications,
  TypeFamilies,
  TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans -fdefer-type-errors #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module Main where

import Control.Exception (TypeError(..), try)
import Data.Type.Bool
import GHC.Stack (HasCallStack)
import GHC.TypeNats

import Fcf.Core
import Fcf.Family
import Fcf.Family.TH

fcfify ''(||)
fcfify ''(+)
fcfify ''If

type family Fst (xs :: (k, l)) :: k where Fst '(x, _) = x
type family Snd (xs :: (k, l)) :: l where Snd '(_, y) = y

fcfify ''Fst
fcfify ''Snd

#if __GLASGOW_HASKELL__ >= 910
type (:+:) = MkName "ghc-internal" "GHC.Internal.TypeNats" "+"
type If_ = MkName "ghc-internal" "GHC.Internal.Data.Type.Bool" "If"
#else
type (:+:) = MkName "base" "GHC.TypeNats" "+"
type If_ = MkName "base" "Data.Type.Bool" "If"
#endif

main :: IO ()
main = do
  equate @(Eval (Family (:+:) P0 '(1, '(2, '())))) @3
  equate @(Eval (Family If_ P1 '( 'True , '(1, '(2, '()))))) @1
  equate @(Eval (Family If_ P1 '( 'False, '(1, '(2, '()))))) @2

  -- Test applyFamily
  equate @(Eval $(applyFamily ''(+) [ [t|3|] , [t|4|] ])) @7
  equate @(Eval $(applyFamily ''If [ [t|'True|] , [t|Int|] , [t|()|] ])) @Int

  -- Test kind inference: the two Any under If should have the same type.
  equate @(Eval (Family If_ P1 '( 'True, '(Any, '(Any, '()))))) @Any

  -- The following should fail because Family_ is untyped (unlike Family)
  -- -fdefer-type-errors only stops at lets.
  shouldFail $ let err = equate @(Eval (Family_ If_ P1 '( 'True, '(Any, '(Any, '()))))) @Any in err

-- Utils

type family Any :: k where {}

equate :: forall a b. (a ~ b) => IO ()
equate = pure ()

shouldFail :: HasCallStack => IO () -> IO ()
shouldFail run = try run >>= \r ->
  case r of
    Left (TypeError _) -> pure ()
    Right () -> error "This test should have failed, but didn't"
