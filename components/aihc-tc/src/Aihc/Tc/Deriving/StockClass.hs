{-# LANGUAGE OverloadedStrings #-}

-- | What stock deriving knows about each class it can derive.
--
-- Stock deriving asks the same four questions of every class: which
-- extension a source clause needs before it may name the class, what a
-- @deriving@ clause without a strategy means for a newtype, which library
-- names the generated method bodies mention, and which bodies to write.
-- The answers used to be four separate lists spread over the strategy
-- check, the context inference, and the generator, so a class added to one
-- and forgotten in another looked supported until the generator refused it.
--
-- One row per class answers all four here. The row names the methods to
-- write as a tag rather than as a function, so that this module stays below
-- the generator: a row whose tag is new does not compile until the
-- generator handles it.
--
-- Where each class lives is deliberately not stated here. The type checker
-- knows no library, so the package and the module of a class reach it
-- through the 'Aihc.Tc.Deriving.References.DerivingReferences' of the
-- configuration, and a name below is only a class once that table agrees.
module Aihc.Tc.Deriving.StockClass
  ( StockClass (..),
    StockMethods (..),
    NewtypeDefaulting (..),
    stockClasses,
    lookupStockClass,
    stockClassRequirement,
    stockClassMethodsOf,
    generatesStockMethods,
    newtypeDefaultingOf,
  )
where

import Aihc.Parser.Syntax (Extension (..))
import Aihc.Tc.Deriving.References (DerivingReference, DerivingReferences (..))
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)

-- | Everything stock deriving knows about one class.
data StockClass = StockClass
  { -- | The name the class is written under, which is also its name in the
    -- class table of the configuration.
    stockClassName :: !Text,
    -- | The extension a source clause needs before it may derive the class.
    -- The six classes of the Haskell report need none.
    stockClassExtension :: !(Maybe Extension),
    -- | What a clause without an explicit strategy means at a newtype.
    stockClassNewtypeDefaulting :: !NewtypeDefaulting,
    -- | The method bodies the generator writes, or 'Nothing' for a class
    -- that is recognized as stock but not generated yet. A plan for such a
    -- class infers no context and reports that the class is not supported.
    stockClassMethods :: !(Maybe StockMethods),
    -- | The library names the generated bodies mention, as selectors into
    -- the reference table. A method of the class being derived needs no
    -- entry: the class says where its own methods live.
    stockClassReferences :: ![DerivingReferences -> DerivingReference]
  }

-- | Which bodies the generator writes for a class. The generator maps each
-- tag to the equations it builds, so the two cannot drift apart.
data StockMethods
  = StockEqMethods
  | StockOrdMethods
  | StockShowMethods
  | StockReadMethods
  | StockBoundedMethods
  deriving (Eq, Show)

-- | What a @deriving@ clause without a strategy selects at a newtype.
data NewtypeDefaulting
  = -- | Always coerce the instance of the representation type. GHC derives
    -- these through the newtype without asking for an extension, because
    -- the stock instance and the coerced one agree.
    NewtypeAlways
  | -- | Coerce the instance of the representation type when
    -- @GeneralizedNewtypeDeriving@ is on. The stock instance would differ,
    -- so a newtype without the extension derives it structurally, and a
    -- newtype whose representation has no instance falls back to stock.
    NewtypeWithGnd
  | -- | Never coerce: the clause means stock.
    NewtypeNever
  deriving (Eq, Show)

-- | Every class stock deriving knows, whether or not it generates code for
-- it. A class missing from this table is not stock, so @deriving stock@ of
-- it is an error and a bare clause falls to another mechanism.
stockClasses :: [StockClass]
stockClasses =
  [ (report "Eq" NewtypeAlways) {stockClassMethods = Just StockEqMethods, stockClassReferences = [derivingTrue, derivingFalse]},
    (report "Ord" NewtypeAlways) {stockClassMethods = Just StockOrdMethods, stockClassReferences = [derivingLT, derivingEQ, derivingGT]},
    (report "Show" NewtypeNever)
      { stockClassMethods = Just StockShowMethods,
        stockClassReferences = [derivingIntCon, derivingGreaterOrEqual, derivingCons]
      },
    (report "Read" NewtypeNever)
      { stockClassMethods = Just StockReadMethods,
        stockClassReferences =
          [ derivingIntCon,
            derivingBind,
            derivingThen,
            derivingReturn,
            derivingReadParens,
            derivingReadPrecContext,
            derivingReadStep,
            derivingReadReset,
            derivingReadAlternative,
            derivingReadFail,
            derivingReadExpect,
            derivingReadField,
            derivingReadSymField,
            derivingLexemeIdent,
            derivingLexemeSymbol,
            derivingLexemePunc
          ]
      },
    (report "Bounded" NewtypeAlways) {stockClassMethods = Just StockBoundedMethods},
    report "Enum" NewtypeWithGnd,
    report "Ix" NewtypeAlways,
    extension "Functor" DeriveFunctor NewtypeWithGnd,
    extension "Foldable" DeriveFoldable NewtypeWithGnd,
    extension "Traversable" DeriveTraversable NewtypeNever,
    extension "Data" DeriveDataTypeable NewtypeNever,
    extension "Typeable" DeriveDataTypeable NewtypeNever,
    extension "Generic" DeriveGeneric NewtypeNever,
    extension "Generic1" DeriveGeneric NewtypeNever,
    extension "Lift" DeriveLift NewtypeNever
  ]
  where
    report name defaulting = StockClass name Nothing defaulting Nothing []
    extension name required defaulting = (report name defaulting) {stockClassExtension = Just required}

-- | The row of a class, or 'Nothing' when stock deriving does not know it.
lookupStockClass :: Text -> Maybe StockClass
lookupStockClass className = find ((== className) . stockClassName) stockClasses

-- | The extension a stock clause for a class needs, in the shape the
-- strategy check reads it: the outer 'Maybe' says whether the class is
-- stock at all, the inner one whether it needs an extension.
stockClassRequirement :: Text -> Maybe (Maybe Extension)
stockClassRequirement className = stockClassExtension <$> lookupStockClass className

-- | The bodies to write for a class, or 'Nothing' when the generator has
-- none for it.
stockClassMethodsOf :: Text -> Maybe StockMethods
stockClassMethodsOf className = lookupStockClass className >>= stockClassMethods

-- | Whether the generator writes an instance body for a class. A plan for
-- a class without one infers no context and generates nothing.
generatesStockMethods :: Text -> Bool
generatesStockMethods = isJust . stockClassMethodsOf

-- | What a clause without a strategy means at a newtype. A class outside
-- the table never coerces.
newtypeDefaultingOf :: Text -> NewtypeDefaulting
newtypeDefaultingOf className =
  maybe NewtypeNever stockClassNewtypeDefaulting (lookupStockClass className)
