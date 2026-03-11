{-# LANGUAGE TemplateHaskell #-} -- due to GHC bug
{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

{- | Definitions for parsing data type constructor names on the type level.

Classically, when doing 'GHC.Generic.Generic' programming in Haskell that
inspects data type metadata such as constructor and record names, we reify these
early and do any parsing etc. on the term level. Constant folding should compute
much of this at compile time, so performance isn't really a worry. But if you're
doing failable operations such as parsing, you can't catch failures at compile
time.

This module provides definitions for parsing constructor names on the type
level, and is used internally in sum type generics. But wait, how do you write a
type-level string parser? That's now feasible-- see the Symparsec library :)
-}

module Generic.Data.MetaParse.Cstr
  ( CstrParser(..)
  , CstrParser'(..)
  , ForceGCParse
  , Raw
  , ParseCstrTo
  ) where

import Generic.Data.MetaParse.Internal
import Data.Kind ( type Constraint )
import GHC.TypeLits ( type Symbol, KnownSymbol )
import GHC.TypeLits ( ErrorMessage(..), type TypeError )
import GHC.Exts ( Proxy# )

-- | Constructor name parse result demotion function using 'Proxy#'.
type ParseCstrTo tag r =
       forall (x :: CstrParseResult tag)
    .  ReifyCstrParseResult tag x
    => Proxy# x -> r

-- | Types defining constructor name parsers.
--
-- When defining instances of these two classes, ensure that you place an empty
-- TH splice e.g. @$(pure [])@ between the instances. This is due to a GHC bug.
class CstrParser' tag => CstrParser tag where
    -- | Constructor name parser.
    --
    -- The Symparsec library generates type families that look like this. See
    -- "Generic.Data.Cstr.Parser.Symparsec" for handy definitions.
    type ParseCstr tag (str :: Symbol)
        :: Either ErrorMessage (CstrParseResult tag)

    -- | Constraint enabling reification of the parsed type-level constructor
    --   name.
    --
    -- For example, you might reify @'(a, b) :: (Symbol, Symbol)@ with
    -- @(KnownSymbol a, KnownSymbol b)@.
    type ReifyCstrParseResult tag (x :: CstrParseResult tag) :: Constraint

-- | Types defining constructor name parsers (inner class).
--
-- We're forced to separate this associated type family from the other class due
-- to GHC complaining "type constructor cannot be used here (it is defined and
-- used in the same recursive group)".
--
-- When defining instances of these two classes, ensure that you place an empty
-- TH splice e.g. @$(pure [])@ between the instances. This is due to a GHC bug.
class CstrParser' tag where
    -- | Result kind of the constructor name parser.
    type CstrParseResult tag :: k

-- | Unwrap a generic constructor parse result. Emits a 'TypeError' on parse
--   failure.
type ForceGCParse :: Symbol -> Symbol -> Either ErrorMessage k -> k
type family ForceGCParse dtName cstr a where
    ForceGCParse _      _    (Right a) = a
    ForceGCParse dtName cstr (Left  e) = TypeError
      ( Text "error while parsing "
        :<>: Text dtName :<>: Text "." :<>: Text cstr :<>: Text ":"
        :$$: e
      )

instance CstrParser' Raw where type CstrParseResult Raw = Symbol
$(pure [])
instance CstrParser  Raw where
    type ParseCstr Raw str = Right str
    type ReifyCstrParseResult Raw str = KnownSymbol str
