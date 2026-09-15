{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Representations of datatypes as sums of products, and the classes that
-- convert a value to and from its representation.
--
-- The vocabulary mirrors GHC's @GHC.Generics@ with one deliberate gap: the
-- metadata constructors of 'Meta' carry no datatype, constructor or
-- selector names. GHC spells those as type-level strings, which aihc's kind
-- checker does not have yet, so 'MetaData', 'MetaCons' and 'MetaSel' keep
-- only the arguments that are ordinary promoted types. The 'Datatype',
-- 'Constructor' and 'Selector' classes are declared so that a signature may
-- mention them, but have no instances until type-level strings arrive; the
-- names then become arguments of the metadata constructors again.
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
  )
where

import Data.Kind (Type)
import Data.Void (Void)
import GHC.Prim (Addr#, Char#, Double#, Float#, Int#, Word#)
import GHC.Ptr (Ptr)
import Prelude
  ( Bool (..),
    Char,
    Double,
    Either (..),
    Eq (..),
    Float,
    Functor (..),
    Int,
    Maybe (..),
    Ord (..),
    Ordering (..),
    Read,
    Show (..),
    String,
    Word,
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
--
-- GHC spells the precedence of 'InfixI' as a type-level natural. aihc has no
-- type-level literals yet, so the precedence is absent here and 'InfixI'
-- carries only the associativity.
data FixityI
  = PrefixI
  | InfixI Associativity

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
-- GHC's constructors begin with the type-level strings that name the
-- datatype, its module, its package, the constructor, and the record
-- selector. aihc has no type-level strings yet, so those arguments are
-- absent and only the remaining ones are kept: whether the datatype is a
-- newtype, the fixity of a constructor and whether it has record syntax,
-- and the three strictness marks of a field.
data Meta
  = MetaData Bool
  | MetaCons FixityI Bool
  | MetaSel SourceUnpackedness SourceStrictness DecidedStrictness

-- | Class for datatypes that represent datatypes.
--
-- There are no instances yet: an instance names the datatype, which needs
-- type-level strings.
class Datatype (d :: Meta) where
  -- | The name of the datatype (unqualified).
  datatypeName :: t d (f :: Type -> Type) (a :: Type) -> String

  -- | The fully-qualified name of the module where the type is declared.
  moduleName :: t d (f :: Type -> Type) (a :: Type) -> String

  -- | The package name of the original type constructor.
  packageName :: t d (f :: Type -> Type) (a :: Type) -> String

  -- | Marks if the datatype is actually a newtype.
  isNewtype :: t d (f :: Type -> Type) (a :: Type) -> Bool

-- | Class for datatypes that represent data constructors.
--
-- There are no instances yet: an instance names the constructor, which
-- needs type-level strings.
class Constructor (c :: Meta) where
  -- | The name of the constructor.
  conName :: t c (f :: Type -> Type) (a :: Type) -> String

  -- | The fixity of the constructor.
  conFixity :: t c (f :: Type -> Type) (a :: Type) -> Fixity

  -- | Marks if this constructor is a record.
  conIsRecord :: t c (f :: Type -> Type) (a :: Type) -> Bool

-- | Class for datatypes that represent records.
--
-- There are no instances yet: an instance names the selector, which needs
-- type-level strings.
class Selector (s :: Meta) where
  -- | The name of the selector.
  selName :: t s (f :: Type -> Type) (a :: Type) -> String

  -- | The strictness information for a field as the user wrote it.
  selSourceUnpackedness :: t s (f :: Type -> Type) (a :: Type) -> SourceUnpackedness

  -- | The strictness information for a field as the user wrote it.
  selSourceStrictness :: t s (f :: Type -> Type) (a :: Type) -> SourceStrictness

  -- | The strictness that the compiler inferred for a field.
  selDecidedStrictness :: t s (f :: Type -> Type) (a :: Type) -> DecidedStrictness

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

-- * Generic instances for the types of the Haskell report

--
-- The representations are written out the way stock deriving generates
-- them, so that a hand-written instance here and a derived one elsewhere
-- agree. Three abbreviations stand for the metadata that every one of these
-- datatypes shares: a datatype that is not a newtype, a prefix constructor
-- without record syntax, and a field with no strictness annotation.

type MetaD = ('MetaData 'False :: Meta)

type MetaC = ('MetaCons 'PrefixI 'False :: Meta)

type MetaS = ('MetaSel 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy :: Meta)

instance Generic Void where
  type Rep Void = D1 MetaD V1
  from x = case x of {}
  to r = case unM1 r of {}

instance Generic () where
  type Rep () = D1 MetaD (C1 MetaC U1)
  from () = M1 (M1 U1)
  to _ = ()

instance Generic Bool where
  type Rep Bool = D1 MetaD (C1 MetaC U1 :+: C1 MetaC U1)
  from False = M1 (L1 (M1 U1))
  from True = M1 (R1 (M1 U1))
  to r = case unM1 r of
    L1 _ -> False
    R1 _ -> True

instance Generic Ordering where
  type Rep Ordering = D1 MetaD (C1 MetaC U1 :+: (C1 MetaC U1 :+: C1 MetaC U1))
  from LT = M1 (L1 (M1 U1))
  from EQ = M1 (R1 (L1 (M1 U1)))
  from GT = M1 (R1 (R1 (M1 U1)))
  to r = case unM1 r of
    L1 _ -> LT
    R1 rest -> case rest of
      L1 _ -> EQ
      R1 _ -> GT

instance Generic (Maybe a) where
  type Rep (Maybe a) = D1 MetaD (C1 MetaC U1 :+: C1 MetaC (S1 MetaS (Rec0 a)))
  from Nothing = M1 (L1 (M1 U1))
  from (Just a) = M1 (R1 (M1 (M1 (K1 a))))
  to r = case unM1 r of
    L1 _ -> Nothing
    R1 just -> Just (unK1 (unM1 (unM1 just)))

instance Generic (Either a b) where
  type Rep (Either a b) = D1 MetaD (C1 MetaC (S1 MetaS (Rec0 a)) :+: C1 MetaC (S1 MetaS (Rec0 b)))
  from (Left a) = M1 (L1 (M1 (M1 (K1 a))))
  from (Right b) = M1 (R1 (M1 (M1 (K1 b))))
  to r = case unM1 r of
    L1 left -> Left (unK1 (unM1 (unM1 left)))
    R1 right -> Right (unK1 (unM1 (unM1 right)))

instance Generic [a] where
  type
    Rep [a] =
      D1
        MetaD
        ( C1 MetaC U1
            :+: C1 ('MetaCons ('InfixI 'RightAssociative) 'False :: Meta) (S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 [a]))
        )
  from [] = M1 (L1 (M1 U1))
  from (a : as) = M1 (R1 (M1 (M1 (K1 a) :*: M1 (K1 as))))
  to r = case unM1 r of
    L1 _ -> []
    R1 cons -> case unM1 cons of
      a :*: as -> unK1 (unM1 a) : unK1 (unM1 as)

instance Generic (a, b) where
  type Rep (a, b) = D1 MetaD (C1 MetaC (S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 b)))
  from (a, b) = M1 (M1 (M1 (K1 a) :*: M1 (K1 b)))
  to r = case unM1 (unM1 r) of
    a :*: b -> (unK1 (unM1 a), unK1 (unM1 b))

instance Generic (a, b, c) where
  type Rep (a, b, c) = D1 MetaD (C1 MetaC (S1 MetaS (Rec0 a) :*: (S1 MetaS (Rec0 b) :*: S1 MetaS (Rec0 c))))
  from (a, b, c) = M1 (M1 (M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))))
  to r = case unM1 (unM1 r) of
    a :*: (b :*: c) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c))

instance Generic (a, b, c, d) where
  type Rep (a, b, c, d) = D1 MetaD (C1 MetaC ((S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 b)) :*: (S1 MetaS (Rec0 c) :*: S1 MetaS (Rec0 d))))
  from (a, b, c, d) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b)) :*: (M1 (K1 c) :*: M1 (K1 d))))
  to r = case unM1 (unM1 r) of
    (a :*: b) :*: (c :*: d) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c), unK1 (unM1 d))

instance Generic (a, b, c, d, e) where
  type Rep (a, b, c, d, e) = D1 MetaD (C1 MetaC ((S1 MetaS (Rec0 a) :*: S1 MetaS (Rec0 b)) :*: (S1 MetaS (Rec0 c) :*: (S1 MetaS (Rec0 d) :*: S1 MetaS (Rec0 e)))))
  from (a, b, c, d, e) = M1 (M1 ((M1 (K1 a) :*: M1 (K1 b)) :*: (M1 (K1 c) :*: (M1 (K1 d) :*: M1 (K1 e)))))
  to r = case unM1 (unM1 r) of
    (a :*: b) :*: (c :*: (d :*: e)) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c), unK1 (unM1 d), unK1 (unM1 e))

instance Generic (a, b, c, d, e, f) where
  type Rep (a, b, c, d, e, f) = D1 MetaD (C1 MetaC ((S1 MetaS (Rec0 a) :*: (S1 MetaS (Rec0 b) :*: S1 MetaS (Rec0 c))) :*: (S1 MetaS (Rec0 d) :*: (S1 MetaS (Rec0 e) :*: S1 MetaS (Rec0 f)))))
  from (a, b, c, d, e, f) = M1 (M1 ((M1 (K1 a) :*: (M1 (K1 b) :*: M1 (K1 c))) :*: (M1 (K1 d) :*: (M1 (K1 e) :*: M1 (K1 f)))))
  to r = case unM1 (unM1 r) of
    (a :*: (b :*: c)) :*: (d :*: (e :*: f)) -> (unK1 (unM1 a), unK1 (unM1 b), unK1 (unM1 c), unK1 (unM1 d), unK1 (unM1 e), unK1 (unM1 f))

instance Generic (a, b, c, d, e, f, g) where
  type Rep (a, b, c, d, e, f, g) = D1 MetaD (C1 MetaC ((S1 MetaS (Rec0 a) :*: (S1 MetaS (Rec0 b) :*: S1 MetaS (Rec0 c))) :*: ((S1 MetaS (Rec0 d) :*: S1 MetaS (Rec0 e)) :*: (S1 MetaS (Rec0 f) :*: S1 MetaS (Rec0 g)))))
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
