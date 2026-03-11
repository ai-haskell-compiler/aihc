{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

{-|When I use "Data.Aeson" library to decode a json string into a Haskell value,
I want to provide default values for @Maybe@ fields, which will be Nothing
when these fields are omitted in the json string. It's a hard work when there
are lots of @Maybe@ fields or deeply nested fields, it will also make your code
hard to read and to maintain. This module provides a solution by using
Higher-kinded data (HKD).
See [this blog](https://reasonablypolymorphic.com/blog/higher-kinded-data/)
for more information about @HKD@.

For example, if you have a @Config@ type as follows,

@
data Config = Config { dbHost :: String
                     , dbPort :: Int
                     , dbName :: String
                       ...
                     }

@

and you want to read these configuration data from a json file when you start
you application, you instantiate "Data.Aeson.FromJSON" for the @Config@,

@
data Config = Config { dbHost :: String
                     , dbPort :: Int
                     , dbName :: String
                       ...
                     } deriving Generic

instance FromJSON Config
@

and you want 'dbPort' can be omitted in the json string, a default value will be
used when it is omitted, you change @String@ type to @Maybe String@,

@
data Config = Config { dbHost :: String
                     , dbPort :: Maybe Int
                     , dbName :: String
                       ...
                     } deriving Generic

instance FromJSON Config
@

and decode and use the configuration data in main function as below,

@
main = do
  config \<- fromJust \<$\> decodeFileStrict "./config.json"
  let host = dbHost config
      port = fromMaybe defaultDBPort $ dbPort config
      ...
  dbConn <- connectDB host port ...
  ...
@

it is neither elegant nor easy to maintain when you have lots of configuration
items.

By using @HKD@ and @type family@, it becomes easier to maintain your code.

@
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data ConfigH f = Config { dbHost = String
                        , dbPort = HKD f String
                          ...
                        } deriving Generic

instance Default ConfigH

instance FromJSON (ConfigH Maybe)

type Config = Config Identity

instance FromJSON Config where
  parseJSON v = applyDef def \<$\> parseJSON v
    where
      def = Config undefined 3306 ...


main = do
  -- Enable RecordWildCards extension
  Config{..} \<- fromJust \<$\> decodeFileStrict "./config.json"
  dbConn <- connectDB dbHost dbPort ...
@

=== More Examples

>>> :set -XDeriveGeneric
>>> :set -XFlexibleInstances
>>> :set -XStandaloneDeriving
>>> import           Data.Functor.Identity
>>> import           GHC.Generics
>>> import           HKD.Default
>>> :{
data Triple f = Triple String (f Int) (f Double) deriving Generic
instance Default Triple
deriving instance Show (Triple Identity)
:}
>>> let def = Triple "hello" (Identity 123) pi :: Triple Identity
>>> applyDef def $ Triple "world" (Just 456) Nothing
Triple "world" (Identity 456) (Identity 3.141592653589793)

>>> :set -XDeriveGeneric
>>> :set -XStandaloneDeriving
>>> :set -XFlexibleInstances
>>> :set -XOverloadedStrings
>>> import           Data.Aeson
>>> import           Data.Functor.Identity
>>> import           GHC.Generics
>>> import           HKD.Default
>>> :{
data Name f = Name { first :: f String
                   , last_ :: f String
                   } deriving Generic
instance Default Name
deriving instance Show (Name Identity)
instance FromJSON (Name Maybe)
data Person f = Person { name :: Name f -- name is required
                       , age  :: f Int  -- age is optional (can be omitted)
                       } deriving Generic
instance Default Person
deriving instance Show (Person Identity)
instance FromJSON (Person Maybe)
instance FromJSON (Person Identity) where
  parseJSON v = applyDef def <$> parseJSON v
    where
      def = Person (Name (Identity "Jorah") (Identity "Gao")) (Identity 28)
:}
>>> decode "{\"name\": {}}" :: Maybe (Person Identity)
Just (Person {name = Name {first = Identity "Jorah", last_ = Identity "Gao"}, age = Identity 28})
>>> decode "{}" :: Maybe (Person Identity)
Nothing

>>> :set -XDeriveGeneric
>>> :set -XFlexibleInstances
>>> :set -XFlexibleContexts
>>> :set -XStandaloneDeriving
>>> :set -XTypeFamilies
>>> import           Data.Functor.Identity
>>> import           GHC.Generics
>>> import           HKD.Default
>>> :{
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a
data Shape f = Square (HKD f Double) | Circle (HKD f Double) deriving Generic
deriving instance Show (Shape Identity)
instance Default Shape where
  defs = [("Square", Square 1.0), ("Circle", Circle 1.0)]
data Container f = Container { base   :: HKD f (Shape f)
                             , height :: HKD f Double
                             } deriving Generic
deriving instance Show (Container Identity)
instance Default Container
:}
>>> let def = Container (Square 10.0) 10.0
>>> applyDef def $ Container Nothing Nothing
Container {base = Square 10.0, height = 10.0}
>>> applyDef def $ Container (Just $ Square Nothing) Nothing
Container {base = Square 10.0, height = 10.0}
>>> applyDef def $ Container (Just $ Circle Nothing) Nothing
Container {base = Circle 1.0, height = 10.0}
>>> applyDefs $ Square Nothing
Square 1.0
>>> applyDefs $ Circle Nothing
Circle 1.0
-}
module HKD.Default
  ( Default(..)
  ) where


import           Data.Functor.Identity
import           Data.Maybe
import           GHC.Generics

data Mismatch = Mismatch

-- | In most cases, use the default implementation for 'Generic' instance.
class Default (t :: (* -> *) -> *) where
  {-| Only used for datatypes with multiple data constructors,
  default implementation is @[]@.

  @since 1.1.0
  -}
  defs :: [(String, t Identity)]
  defs = []

  {-| You should either provide 'lookupDef' or 'defs', default implementation
  is to look up by the given constructor name from 'defs'.

  @since 1.1.0
  -}
  lookupDef ::  String -> Maybe (t Identity)
  lookupDef = flip lookup defs

  applyDef :: t Identity -> t Maybe -> t Identity

  {- | Apply the given default value, and fallback to 'applyDefs' if the
  default value's constructor does not match.
  -}
  default applyDef :: ( Generic (t Identity)
                      , Generic (t Maybe)
                      , GConsName (Rep (t Maybe))
                      , GDefault (Rep (t Identity)) (Rep (t Maybe))
                      )
                   => t Identity -> t Maybe -> t Identity
  applyDef i m | Right r <- gapplyDef (from i) (from m) = to r
  applyDef _ m = applyDefs m -- fallback to apply appropriate default value


  applyDefs :: t Maybe -> t Identity

  {- | Look up the appropriate default value from defs and try to apply.

  The default implementation will raise "Can't find default value" error when
  the result of looking up from 'defs' is 'Nothing'.

  The default implementation will raise "Mismatch Constructor" error when the
  default value's constructor does not match.

  @since 1.1.0
  -}
  default applyDefs :: ( Generic (t Identity)
                       , Generic (t Maybe)
                       , GConsName (Rep (t Maybe))
                       , GDefault (Rep (t Identity)) (Rep (t Maybe))
                       )
                    => t Maybe -> t Identity
  applyDefs m = applyOrThrow (lookupDef (gconsName (from m))) m
    where
      -- suppress compiler error
      -- NB: ‘Rep’ is a type function, and may not be injective
      applyOrThrow :: ( Generic (t Identity)
                      , Generic (t Maybe)
                      , GDefault (Rep (t Identity)) (Rep (t Maybe))
                      )
                   => (Maybe (t Identity)) -> t Maybe -> t Identity
      applyOrThrow (Just i) m | Right r <- gapplyDef (from i) (from m) = to r
      applyOrThrow Nothing _  = error "HKD.Default: Can't find default value"
      applyOrThrow _ _        = error "HKD.Default: Mismatch Constructor"


class GDefault f g where
  gapplyDef :: f (t Identity) -> g (t Maybe) -> Either Mismatch (f (t Identity))

-- Data type
instance GDefault f g => GDefault (D1 c f) (D1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 <$> gapplyDef p k

-- Choice between data constructors
instance ( GDefault f g
         , GDefault f' g'
         ) => GDefault (f :+: f') (g :+: g') where
  gapplyDef (L1 p) (L1 k) = L1 <$> gapplyDef p k
  gapplyDef (R1 p) (R1 k) = R1 <$> gapplyDef p k
  gapplyDef _ _           = Left Mismatch

-- Data constructor
instance ( Constructor c
         , GDefault f g
         ) => GDefault (C1 c f) (C1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 <$> gapplyDef p k

-- Enum type (nullary data constructor)
instance Constructor c => GDefault (C1 c U1) (C1 c U1) where
  gapplyDef (M1 p) (M1 k) = Right $ M1 p

-- Apply record selectors
instance ( GDefault f g
         , GDefault f' g'
         ) => GDefault (f :*: f') (g :*: g') where
  gapplyDef (p :*: p') (k :*: k') = do
    x <- gapplyDef p k
    y <- gapplyDef p' k'
    return $ x :*: y

-- Selector
instance (Selector c , GDefault f g) => GDefault (S1 c f) (S1 c g) where
  gapplyDef (M1 p) (M1 k) = M1 <$> gapplyDef p k

-- Not nested required field
instance GDefault (K1 i f) (K1 i f) where
  gapplyDef (K1 p) (K1 k) = Right $ K1 k

-- Not nested optional field (use type family)
instance GDefault (K1 i f) (K1 i (Maybe f)) where
  gapplyDef (K1 p) (K1 k) = Right $ K1 $ fromMaybe p k

-- Not nested optional field (not use type family)
instance GDefault (K1 i (Identity f)) (K1 i (Maybe f)) where
  gapplyDef (K1 p) (K1 Nothing)  = Right $ K1 p
  gapplyDef (K1 p) (K1 (Just k)) = Right $ K1 $ Identity k

-- Nested required field
instance Default t => GDefault (K1 i (t Identity)) (K1 i (t Maybe)) where
  gapplyDef (K1 p) (K1 k) = Right $ K1 $ applyDef p k

-- Nested optional field
instance Default t => GDefault (K1 i (t Identity)) (K1 i (Maybe (t Maybe))) where
  gapplyDef (K1 p) (K1 Nothing)  = Right $ K1 p
  gapplyDef (K1 p) (K1 (Just k)) = Right $ K1 $ applyDef p k


class GConsName f where
  gconsName :: f p -> String

instance GConsName f => GConsName (D1 c f) where
  gconsName (M1 x) = gconsName x

instance (GConsName f, GConsName g)=> GConsName (f :+: g) where
  gconsName (L1 x) = gconsName x
  gconsName (R1 x) = gconsName x

instance Constructor c => GConsName (C1 c f) where
  gconsName = conName
