{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Representations of datatypes as sums of products, and the classes that
-- convert a value to and from its representation.
--
-- The vocabulary mirrors GHC's @GHC.Generics@. The representation types
-- take arguments of kind 'Type' only, where GHC lets some of them be
-- poly-kinded.
module GHC.Generics
  ( -- * Generic representation types
    V1,
    U1 (..),
    Par1 (..),
    Rec1 (..),
    K1 (..),
    M1 (..),
    (:+:) (..),
    (:*:) (..),
    (:.:) (..),

    -- ** Unboxed representation types
    URec (..),
    UAddr,
    UChar,
    UDouble,
    UFloat,
    UInt,
    UWord,

    -- ** Synonyms for convenience
    Rec0,
    R,
    D1,
    C1,
    S1,
    D,
    C,
    S,

    -- * Meta-information
    Datatype (..),
    Constructor (..),
    Selector (..),
    Fixity (..),
    FixityI (..),
    Associativity (..),
    prec,
    SourceUnpackedness (..),
    SourceStrictness (..),
    DecidedStrictness (..),
    Meta (..),

    -- * Generic type classes
    Generic (..),
    Generic1 (..),

    -- * Generic wrapper
    Generically (..),
    Generically1 (..),
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import GHC.Prim (Addr#, Char#, Double#, Float#, Int#, Word#)
import GHC.Ptr (Ptr)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import Prelude
  ( Applicative (..),
    Bool (..),
    Char,
    Double,
    Either (..),
    Eq (..),
    Float,
    Foldable (..),
    Functor (..),
    Int,
    Maybe (..),
    Monad (..),
    Ord (..),
    Ordering (..),
    Read,
    Show (..),
    String,
    Traversable (..),
    Word,
    flip,
    fromInteger,
    (<$>),
  )

-- * Representation types

-- | Void: used for datatypes without constructors.
data V1 (p :: Type)

-- | Unit: used for constructors without arguments.
data U1 (p :: Type) = U1

-- | Used for marking occurrences of the parameter.
newtype Par1 (p :: Type) = Par1 {unPar1 :: p}

-- | Recursive calls of kind @Type -> Type@ (or kind @k -> Type@).
newtype Rec1 (f :: Type -> Type) (p :: Type) = Rec1 {unRec1 :: f p}

-- | Constants, additional parameters and recursion of kind @Type@.
newtype K1 (i :: Type) (c :: Type) (p :: Type) = K1 {unK1 :: c}

-- | Meta-information (constructor names, etc.).
newtype M1 (i :: Type) (c :: Meta) (f :: Type -> Type) (p :: Type) = M1 {unM1 :: f p}

infixr 5 :+:

-- | Sums: encode choice between constructors.
data (:+:) (f :: Type -> Type) (g :: Type -> Type) (p :: Type) = L1 (f p) | R1 (g p)

infixr 6 :*:

-- | Products: encode multiple arguments to constructors.
data (:*:) (f :: Type -> Type) (g :: Type -> Type) (p :: Type) = f p :*: g p

infixr 7 :.:

-- | Composition of functors.
newtype (:.:) (f :: Type -> Type) (g :: Type -> Type) (p :: Type) = Comp1 {unComp1 :: f (g p)}

-- | Constants of unlifted kinds.
data family URec (a :: Type) (p :: Type)

data instance URec (Ptr ()) p = UAddr {uAddr# :: Addr#}

data instance URec Char p = UChar {uChar# :: Char#}

data instance URec Double p = UDouble {uDouble# :: Double#}

data instance URec Float p = UFloat {uFloat# :: Float#}

data instance URec Int p = UInt {uInt# :: Int#}

data instance URec Word p = UWord {uWord# :: Word#}

-- | Type synonym for @'URec' 'Addr#'@.
type UAddr = URec (Ptr ())

-- | Type synonym for @'URec' 'Char#'@.
type UChar = URec Char

-- | Type synonym for @'URec' 'Double#'@.
type UDouble = URec Double

-- | Type synonym for @'URec' 'Float#'@.
type UFloat = URec Float

-- | Type synonym for @'URec' 'Int#'@.
type UInt = URec Int

-- | Type synonym for @'URec' 'Word#'@.
type UWord = URec Word

-- | Tag for @K1@: recursion (of kind @Type@).
data R

-- | Tag for @M1@: datatype.
data D

-- | Tag for @M1@: constructor.
data C

-- | Tag for @M1@: record selector.
data S

-- | Type synonym for encoding recursion (of kind @Type@).
type Rec0 = K1 R

-- | Type synonym for encoding meta-information for datatypes.
type D1 = M1 D

-- | Type synonym for encoding meta-information for constructors.
type C1 = M1 C

-- | Type synonym for encoding meta-information for record selectors.
type S1 = M1 S

-- * Meta-information

-- | Datatype to represent the fixity of a constructor. An infix declaration
-- directly corresponds to an application of 'Infix'.
data Fixity
  = Prefix
  | Infix Associativity Int
  deriving (Eq, Show, Ord, Read)

-- | This variant of 'Fixity' appears at the type level.
data FixityI
  = PrefixI
  | InfixI Associativity Nat

-- | Datatype to represent the associativity of a constructor.
data Associativity
  = LeftAssociative
  | RightAssociative
  | NotAssociative
  deriving (Eq, Show, Ord, Read)

-- | Get the precedence of a fixity value.
prec :: Fixity -> Int
prec Prefix = 10
prec (Infix _ n) = n

-- | The unpackedness of a field as the user wrote it.
data SourceUnpackedness
  = NoSourceUnpackedness
  | SourceNoUnpack
  | SourceUnpack
  deriving (Eq, Show, Ord, Read)

-- | The strictness of a field as the user wrote it.
data SourceStrictness
  = NoSourceStrictness
  | SourceLazy
  | SourceStrict
  deriving (Eq, Show, Ord, Read)

-- | The strictness that the compiler inferred for a field.
data DecidedStrictness
  = DecidedLazy
  | DecidedStrict
  | DecidedUnpack
  deriving (Eq, Show, Ord, Read)

-- | Datatype to represent metadata associated with a datatype
-- (@MetaData@), constructor (@MetaCons@), or field selector (@MetaSel@).
--
-- * In @MetaData n m p nt@, @n@ is the datatype's name, @m@ is the module
--   in which the datatype is defined, @p@ is the package in which the
--   datatype is defined, and @nt@ is @'True@ if the datatype is a
--   @newtype@.
--
-- * In @MetaCons n f s@, @n@ is the constructor's name, @f@ is its fixity,
--   and @s@ is @'True@ if the constructor contains record selectors.
--
-- * In @MetaSel mn su ss ds@, if the field uses record syntax, then @mn@ is
--   'Just' the record name. Otherwise, @mn@ is 'Nothing'. @su@ and @ss@ are
--   the field's unpackedness and strictness annotations, and @ds@ is the
--   strictness that the compiler infers for the field.
data Meta
  = MetaData Symbol Symbol Symbol Bool
  | MetaCons Symbol FixityI Bool
  | MetaSel (Maybe Symbol) SourceUnpackedness SourceStrictness DecidedStrictness

-- | Class for datatypes that represent datatypes.
class Datatype (d :: Meta) where
  -- | The name of the datatype (unqualified).
  datatypeName :: t d (f :: Type -> Type) (a :: Type) -> String

  -- | The fully-qualified name of the module where the type is declared.
  moduleName :: t d (f :: Type -> Type) (a :: Type) -> String

  -- | The package name of the module where the type is declared.
  packageName :: t d (f :: Type -> Type) (a :: Type) -> String

  -- | Marks if the datatype is actually a newtype.
  isNewtype :: t d (f :: Type -> Type) (a :: Type) -> Bool
  isNewtype _ = False

instance (KnownSymbol n, KnownSymbol m, KnownSymbol p, KnownBool nt) => Datatype ('MetaData n m p nt) where
  datatypeName _ = symbolVal (Proxy :: Proxy n)
  moduleName _ = symbolVal (Proxy :: Proxy m)
  packageName _ = symbolVal (Proxy :: Proxy p)
  isNewtype _ = boolVal (Proxy :: Proxy nt)

-- | Class for datatypes that represent data constructors.
class Constructor (c :: Meta) where
  -- | The name of the constructor.
  conName :: t c (f :: Type -> Type) (a :: Type) -> String

  -- | The fixity of the constructor.
  conFixity :: t c (f :: Type -> Type) (a :: Type) -> Fixity
  conFixity _ = Prefix

  -- | Marks if this constructor is a record.
  conIsRecord :: t c (f :: Type -> Type) (a :: Type) -> Bool
  conIsRecord _ = False

instance (KnownSymbol n, KnownFixityI f, KnownBool r) => Constructor ('MetaCons n f r) where
  conName _ = symbolVal (Proxy :: Proxy n)
  conFixity _ = fixityVal (Proxy :: Proxy f)
  conIsRecord _ = boolVal (Proxy :: Proxy r)

-- | Class for datatypes that represent records.
class Selector (s :: Meta) where
  -- | The name of the selector. It is empty for a field without a name.
  selName :: t s (f :: Type -> Type) (a :: Type) -> String

  -- | The unpackedness annotation of a field, as the user wrote it.
  selSourceUnpackedness :: t s (f :: Type -> Type) (a :: Type) -> SourceUnpackedness

  -- | The strictness annotation of a field, as the user wrote it.
  selSourceStrictness :: t s (f :: Type -> Type) (a :: Type) -> SourceStrictness

  -- | The strictness that the compiler inferred for a field.
  selDecidedStrictness :: t s (f :: Type -> Type) (a :: Type) -> DecidedStrictness

instance
  (KnownMaybeSymbol mn, KnownSourceUnpackedness su, KnownSourceStrictness ss, KnownDecidedStrictness ds) =>
  Selector ('MetaSel mn su ss ds)
  where
  selName _ = maybeSymbolVal (Proxy :: Proxy mn)
  selSourceUnpackedness _ = sourceUnpackednessVal (Proxy :: Proxy su)
  selSourceStrictness _ = sourceStrictnessVal (Proxy :: Proxy ss)
  selDecidedStrictness _ = decidedStrictnessVal (Proxy :: Proxy ds)

-- * Values of the promoted metadata

--
-- GHC reads a promoted metadata argument back as a value through its
-- internal singleton classes. The classes below do the same for each kind
-- that the metadata uses, one class for each kind.

-- | A promoted 'Bool' whose value is known.
class KnownBool (b :: Bool) where
  boolVal :: Proxy b -> Bool

instance KnownBool 'False where
  boolVal _ = False

instance KnownBool 'True where
  boolVal _ = True

-- | A promoted @'Maybe' 'Symbol'@ whose value is known. 'Nothing' reads
-- back as the empty string, as 'selName' needs.
class KnownMaybeSymbol (m :: Maybe Symbol) where
  maybeSymbolVal :: Proxy m -> String

instance KnownMaybeSymbol 'Nothing where
  maybeSymbolVal _ = ""

instance (KnownSymbol s) => KnownMaybeSymbol ('Just s) where
  maybeSymbolVal _ = symbolVal (Proxy :: Proxy s)

-- | A promoted 'FixityI' whose value is known.
class KnownFixityI (f :: FixityI) where
  fixityVal :: Proxy f -> Fixity

instance KnownFixityI 'PrefixI where
  fixityVal _ = Prefix

instance (KnownAssociativity a, KnownNat n) => KnownFixityI ('InfixI a n) where
  fixityVal _ = Infix (associativityVal (Proxy :: Proxy a)) (fromInteger (natVal (Proxy :: Proxy n)))

-- | A promoted 'Associativity' whose value is known.
class KnownAssociativity (a :: Associativity) where
  associativityVal :: Proxy a -> Associativity

instance KnownAssociativity 'LeftAssociative where
  associativityVal _ = LeftAssociative

instance KnownAssociativity 'RightAssociative where
  associativityVal _ = RightAssociative

instance KnownAssociativity 'NotAssociative where
  associativityVal _ = NotAssociative

-- | A promoted 'SourceUnpackedness' whose value is known.
class KnownSourceUnpackedness (u :: SourceUnpackedness) where
  sourceUnpackednessVal :: Proxy u -> SourceUnpackedness

instance KnownSourceUnpackedness 'NoSourceUnpackedness where
  sourceUnpackednessVal _ = NoSourceUnpackedness

instance KnownSourceUnpackedness 'SourceNoUnpack where
  sourceUnpackednessVal _ = SourceNoUnpack

instance KnownSourceUnpackedness 'SourceUnpack where
  sourceUnpackednessVal _ = SourceUnpack

-- | A promoted 'SourceStrictness' whose value is known.
class KnownSourceStrictness (s :: SourceStrictness) where
  sourceStrictnessVal :: Proxy s -> SourceStrictness

instance KnownSourceStrictness 'NoSourceStrictness where
  sourceStrictnessVal _ = NoSourceStrictness

instance KnownSourceStrictness 'SourceLazy where
  sourceStrictnessVal _ = SourceLazy

instance KnownSourceStrictness 'SourceStrict where
  sourceStrictnessVal _ = SourceStrict

-- | A promoted 'DecidedStrictness' whose value is known.
class KnownDecidedStrictness (d :: DecidedStrictness) where
  decidedStrictnessVal :: Proxy d -> DecidedStrictness

instance KnownDecidedStrictness 'DecidedLazy where
  decidedStrictnessVal _ = DecidedLazy

instance KnownDecidedStrictness 'DecidedStrict where
  decidedStrictnessVal _ = DecidedStrict

instance KnownDecidedStrictness 'DecidedUnpack where
  decidedStrictnessVal _ = DecidedUnpack

-- * Generic type classes

-- | Representable types of kind @Type@.
class Generic a where
  -- | Generic representation type.
  type Rep a :: Type -> Type

  -- | Convert from the datatype to its representation.
  from :: a -> Rep a x

  -- | Convert from the representation to the datatype.
  to :: Rep a x -> a

-- | Representable types of kind @Type -> Type@.
class Generic1 (f :: Type -> Type) where
  -- | Generic representation type.
  type Rep1 f :: Type -> Type

  -- | Convert from the datatype to its representation.
  from1 :: f a -> Rep1 f a

  -- | Convert from the representation to the datatype.
  to1 :: Rep1 f a -> f a

-- * Generic wrappers

-- | A datatype whose instances are defined generically, through the
-- 'Generic' representation. Use it with @DerivingVia@.
newtype Generically a = Generically a

-- | A type whose instances are defined generically, through the
-- 'Generic1' representation. Use it with @DerivingVia@.
newtype Generically1 (f :: Type -> Type) (a :: Type) = Generically1 (f a)

instance (Generic1 f, Functor (Rep1 f)) => Functor (Generically1 f) where
  fmap f (Generically1 values) = Generically1 (to1 (fmap f (from1 values)))

-- * Generic instances for the types of the Haskell report

--
-- The representations are written out the way stock deriving generates
-- them, so that a hand-written instance here and a derived one elsewhere
-- agree. Three abbreviations stand for the metadata that these datatypes
-- share: a datatype that is not a newtype, a prefix constructor without
-- record syntax, and a field without a name or a strictness annotation.
-- The names are the ones that GHC gives, so that a program that shows them
-- prints the same text.

type MetaD (n :: Symbol) (m :: Symbol) (p :: Symbol) = ('MetaData n m p 'False :: Meta)

type MetaC (n :: Symbol) = ('MetaCons n 'PrefixI 'False :: Meta)

type MetaS = ('MetaSel 'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy :: Meta)

instance Generic Void where
  type Rep Void = D1 (MetaD "Void" "GHC.Internal.Base" "ghc-internal") V1
  from x = case x of {}
  to r = case unM1 r of {}

instance Generic () where
  type Rep () = D1 (MetaD "Unit" "GHC.Tuple" "ghc-prim") (C1 (MetaC "()") U1)
  from () = M1 (M1 U1)
  to _ = ()

instance Generic Bool where
  type Rep Bool = D1 (MetaD "Bool" "GHC.Types" "ghc-prim") (C1 (MetaC "False") U1 :+: C1 (MetaC "True") U1)
  from False = M1 (L1 (M1 U1))
  from True = M1 (R1 (M1 U1))
  to r = case unM1 r of
    L1 _ -> False
    R1 _ -> True

instance Generic Ordering where
  type Rep Ordering = D1 (MetaD "Ordering" "GHC.Types" "ghc-prim") (C1 (MetaC "LT") U1 :+: (C1 (MetaC "EQ") U1 :+: C1 (MetaC "GT") U1))
  from LT = M1 (L1 (M1 U1))
  from EQ = M1 (R1 (L1 (M1 U1)))
  from GT = M1 (R1 (R1 (M1 U1)))
  to r = case unM1 r of
    L1 _ -> LT
    R1 rest -> case rest of
      L1 _ -> EQ
      R1 _ -> GT

instance Generic (Maybe a) where
  type Rep (Maybe a) = D1 (MetaD "Maybe" "GHC.Internal.Maybe" "ghc-internal") (C1 (MetaC "Nothing") U1 :+: C1 (MetaC "Just") (S1 MetaS (Rec0 a)))
  from Nothing = M1 (L1 (M1 U1))
  from (Just a) = M1 (R1 (M1 (M1 (K1 a))))
  to r = case unM1 r of
    L1 _ -> Nothing
    R1 just -> Just (unK1 (unM1 (unM1 just)))

instance Generic (Either a b) where
  type Rep (Either a b) = D1 (MetaD "Either" "GHC.Internal.Data.Either" "ghc-internal") (C1 (MetaC "Left") (S1 MetaS (Rec0 a)) :+: C1 (MetaC "Right") (S1 MetaS (Rec0 b)))
  from (Left a) = M1 (L1 (M1 (M1 (K1 a))))
  from (Right b) = M1 (R1 (M1 (M1 (K1 b))))
  to r = case unM1 r of
    L1 left -> Left (unK1 (unM1 (unM1 left)))
    R1 right -> Right (unK1 (unM1 (unM1 right)))

instance Generic [a] where
  type
    Rep [a] =
      D1
        (MetaD "List" "GHC.Types" "ghc-prim")
        ( C1 (MetaC "[]") U1
            :+: C1 ('MetaCons ":" ('InfixI 'RightAssociative 5) 'False :: Meta) (S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 [a]))
        )
  from [] = M1 (L1 (M1 U1))
  from (a : as) = M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 as))))
  to r = case unM1 r of
    L1 _ -> []
    R1 cons -> case unM1 cons of
      a :*: as -> unK1 (unM1 a) : unK1 (unM1 as)

instance Generic (a, b) where
  type Rep (a, b) = D1 (MetaD "Tuple2" "GHC.Tuple" "ghc-prim") (C1 (MetaC "(,)") (S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 b)))
  from (a, b) = M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))
  to r = case unM1 (unM1 r) of
    a :*: b -> (unK1 (unM1 a), unK1 (unM1 b))

instance Generic (a, b, c) where
  type Rep (a, b, c) = D1 (MetaD "Tuple3" "GHC.Tuple" "ghc-prim") (C1 (MetaC "(,,)") (S1 MetaS (Rec0 a) :*: (S1 MetaS (Rec0 b) :*: S1 MetaS (Rec0 c))))
  from (a, b, c) = M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))))
  to r = case unM1 (unM1 r) of
    a :*: (b :*: c) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c))

instance Generic (a, b, c, d) where
  type Rep (a, b, c, d) = D1 (MetaD "Tuple4" "GHC.Tuple" "ghc-prim") (C1 (MetaC "(,,,)") ((S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 b)) :*: (S1 MetaS (Rec0 c) :*: S1 MetaS (Rec0 d))))
  from (a, b, c, d) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b)) :*: (M1 (K1 c) :*: M1 (K1 d))))
  to r = case unM1 (unM1 r) of
    (a :*: b) :*: (c :*: d) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c), unK1 (unM1 d))

instance Generic (a, b, c, d, e) where
  type Rep (a, b, c, d, e) = D1 (MetaD "Tuple5" "GHC.Tuple" "ghc-prim") (C1 (MetaC "(,,,,)") ((S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 b)) :*: (S1 MetaS (Rec0 c) :*: (S1 MetaS (Rec0 d) :*: S1 MetaS (Rec0 e)))))
  from (a, b, c, d, e) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b)) :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (K1 e)))))
  to r = case unM1 (unM1 r) of
    (a :*: b) :*: (c :*: (d :*: e)) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c), unK1 (unM1 d), unK1 (unM1 e))

instance Generic (a, b, c, d, e, f) where
  type Rep (a, b, c, d, e, f) = D1 (MetaD "Tuple6" "GHC.Tuple" "ghc-prim") (C1 (MetaC "(,,,,,)") ((S1 MetaS (Rec0 a) :*: (S1 MetaS (Rec0 b) :*: S1 MetaS (Rec0 c))) :*: (S1 MetaS (Rec0 d) :*: (S1 MetaS (Rec0 e) :*: S1 MetaS (Rec0 f)))))
  from (a, b, c, d, e, f) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))) :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (K1 f)))))
  to r = case unM1 (unM1 r) of
    (a :*: (b :*: c)) :*: (d :*: (e :*: f)) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c), unK1 (unM1 d), unK1 (unM1 e), unK1 (unM1 f))

instance Generic (a, b, c, d, e, f, g) where
  type Rep (a, b, c, d, e, f, g) = D1 (MetaD "Tuple7" "GHC.Tuple" "ghc-prim") (C1 (MetaC "(,,,,,,)") ((S1 MetaS (Rec0 a) :*: (S1 MetaS (Rec0 b) :*: S1 MetaS (Rec0 c))) :*: ((S1 MetaS (Rec0 d) :*: S1 MetaS (Rec0 e)) :*: (S1 MetaS (Rec0 f) :*: S1 MetaS (Rec0 g)))))
  from (a, b, c, d, e, f, g) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))) :*: ((M1 (K1 d) :*: M1 (K1 e)) :*: (M1 (K1 f) :*: M1 (K1 g)))))
  to r = case unM1 (unM1 r) of
    (a :*: (b :*: c)) :*: ((d :*: e) :*: (f :*: g)) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c), unK1 (unM1 d), unK1 (unM1 e), unK1 (unM1 f), unK1 (unM1 g))

-- * Functor instances

instance Functor V1 where
  fmap _ v = case v of {}

instance Functor U1 where
  fmap _ _ = U1

instance Functor Par1 where
  fmap f (Par1 value) = Par1 (f value)

instance (Functor f) => Functor (Rec1 f) where
  fmap f (Rec1 values) = Rec1 (fmap f values)

instance Functor (K1 i c) where
  fmap _ (K1 value) = K1 value

instance (Functor f) => Functor (M1 i c f) where
  fmap f (M1 values) = M1 (fmap f values)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (L1 values) = L1 (fmap f values)
  fmap f (R1 values) = R1 (fmap f values)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (left :*: right) = fmap f left :*: fmap f right

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Comp1 values) = Comp1 (fmap (fmap f) values)

-- * Foldable instances

instance Foldable V1 where
  foldr _ _ v = case v of {}

instance Foldable U1 where
  foldr _ initial _ = initial

instance Foldable Par1 where
  foldr f initial (Par1 value) = f value initial

instance (Foldable f) => Foldable (Rec1 f) where
  foldr f initial (Rec1 values) = foldr f initial values

instance Foldable (K1 i c) where
  foldr _ initial _ = initial

instance (Foldable f) => Foldable (M1 i c f) where
  foldr f initial (M1 values) = foldr f initial values

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldr f initial (L1 values) = foldr f initial values
  foldr f initial (R1 values) = foldr f initial values

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldr f initial (left :*: right) = foldr f (foldr f initial right) left

instance (Foldable f, Foldable g) => Foldable (f :.: g) where
  foldr f initial (Comp1 values) = foldr (flip (foldr f)) initial values

-- * Traversable instances

instance Traversable V1 where
  traverse _ v = case v of {}

instance Traversable U1 where
  traverse _ _ = pure U1

instance Traversable Par1 where
  traverse f (Par1 value) = fmap Par1 (f value)

instance (Traversable f) => Traversable (Rec1 f) where
  traverse f (Rec1 values) = fmap Rec1 (traverse f values)

instance Traversable (K1 i c) where
  traverse _ (K1 value) = pure (K1 value)

instance (Traversable f) => Traversable (M1 i c f) where
  traverse f (M1 values) = fmap M1 (traverse f values)

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse f (L1 values) = fmap L1 (traverse f values)
  traverse f (R1 values) = fmap R1 (traverse f values)

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  traverse f (left :*: right) = (:*:) <$> traverse f left <*> traverse f right

instance (Traversable f, Traversable g) => Traversable (f :.: g) where
  traverse f (Comp1 values) = fmap Comp1 (traverse (traverse f) values)

-- * Applicative and Monad instances

instance Applicative U1 where
  pure _ = U1
  _ <*> _ = U1

instance Applicative Par1 where
  pure = Par1
  Par1 f <*> Par1 value = Par1 (f value)

instance (Applicative f) => Applicative (Rec1 f) where
  pure value = Rec1 (pure value)
  Rec1 functions <*> Rec1 values = Rec1 (functions <*> values)

instance (Applicative f) => Applicative (M1 i c f) where
  pure value = M1 (pure value)
  M1 functions <*> M1 values = M1 (functions <*> values)

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure value = pure value :*: pure value
  (leftFunctions :*: rightFunctions) <*> (leftValues :*: rightValues) =
    (leftFunctions <*> leftValues) :*: (rightFunctions <*> rightValues)

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure value = Comp1 (pure (pure value))
  Comp1 functions <*> Comp1 values = Comp1 (fmap (<*>) functions <*> values)

instance Monad U1 where
  _ >>= _ = U1

instance Monad Par1 where
  Par1 value >>= f = f value

instance (Monad f) => Monad (Rec1 f) where
  Rec1 values >>= f = Rec1 (values >>= \value -> unRec1 (f value))

instance (Monad f) => Monad (M1 i c f) where
  M1 values >>= f = M1 (values >>= \value -> unM1 (f value))

instance (Monad f, Monad g) => Monad (f :*: g) where
  (left :*: right) >>= f =
    (left >>= \value -> leftFactor (f value)) :*: (right >>= \value -> rightFactor (f value))
    where
      leftFactor (value :*: _) = value
      rightFactor (_ :*: value) = value
