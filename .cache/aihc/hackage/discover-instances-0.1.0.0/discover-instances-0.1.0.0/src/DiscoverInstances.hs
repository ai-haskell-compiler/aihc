{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Generally speaking, this module is only useful to discover instances
-- of unary type classes where the instance is unconstrained.
--
-- That is to say - not all that useful in libraries.
--
-- However, this has ended up being super useful to me in a bunch of
-- application contexts. Consider @persistent-discover@, which grabs all
-- the @PersistEntity@ instances in scope and makes the @[EntityDef]@ for
-- them. Or consider a front-end types generating module, which needs to
-- import a ton of modules, and then call @toSomeFrontEndClass :: SomeClass
-- a => Proxy a -> SomeThing@ on each thing.
--
-- This library can simplify that process.
--
-- @since 0.1.0.0
module DiscoverInstances
    (
    -- * The main interface
      discoverInstances
    -- * Using the results of 'discoverInstances'
    -- $using
    , withInstances
    , forInstances
    , module SomeDictOf
    -- * Re-exports
    , module Data.Proxy
    ) where

import Data.Proxy
import Data.Typeable
import Language.Haskell.TH hiding (cxt)
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax.Compat
import SomeDictOf

-- | This TemplateHaskell function accepts a type and splices in a list of
-- 'SomeDict's that provide evidence that the type is an instance of
-- the class that you asked for.
--
-- There are some limitations.
--
-- * The class can only accept a single parameter.
-- * The instances returned do not have a context.
--
-- Example Use:
--
-- @
-- eq :: ['SomeDict' Eq]
-- eq = $$(discoverInstances)
-- @
--
-- This function uses typed @TemplateHaskell@, which means you don't need to
-- provide a type annotation directly. However, you can pass a type directly.
--
-- @
-- ord :: ['SomeDict' 'Ord']
-- ord = $$(discoverInstances @Ord)
-- @
--
-- GHC supports using the @$$@ syntax without parentheses if the expression is
-- a single term. So you can also write this:
--
-- @
-- functor :: ['SomeDict' 'Functor']
-- functor = $$discoverInstances
-- @
--
-- But you'll get an error if you type-apply like that.
--
-- @since 0.1.0.0
discoverInstances :: forall (c :: _ -> Constraint) . (Typeable c) => SpliceQ [SomeDict c]
discoverInstances = liftSplice $ do
    let
        className =
            show (typeRep (Proxy @c))
    instanceDecs <- reifyInstances (mkName className) [VarT (mkName "a")]

    dicts <- fmap listTE $ traverse decToDict instanceDecs

    examineSplice [|| concat $$(liftSplice $ pure dicts) ||]

-- $using
--
-- Once you've acquired a @'SomeDict' c@ for some type class @c@ that you care
-- about, it's not entirely clear how you might use it.
--
-- The "SomeDictOf" module contains functions for working with these, but the
-- simplest thing to do is probably pattern match on them directly. The use case
-- that this library was designed to support is iterating over all the visible
-- instances of a class and performing some operation on the class.
--
-- Consider the @persistent@ database library. The @PersistEntity@ type class
-- defines a method @entityDef :: PersistEntity a => proxy a -> EntityDef@,
-- where the @EntityDef@ type contains information that relates the type to the
-- database encoding for the type. Let's get all the @EntityDef@s for the types
-- in scope:
--
-- @
-- entityDefs :: [EntityDef]
-- entityDefs =
--      'map'
--          (\\('SomeDictOf' proxy) -> entityDef proxy)
--          $$('discoverInstances' \@PersistEntity)
-- @
--
-- The @EntityDef@ for an entity include the documentation comments written for
-- the entity. So we can print out all the documentation for the database
-- tables.
--
-- Another intended use is to render generated code. We use the
-- @aeson-typescript@ library to generate TypeScript code for our API. Without
-- this library, we maintain a list of types to generate code for. This list is
-- duplicated in the imports, as well as the data declarations.
--
-- @
-- import Model.Foo
-- import Model.Bar
-- import Model.Baz
-- import Model.Quux
--
-- renderTypeScript :: IO ()
-- renderTypeScript = do
--     writeToFile $ concat
--         [ writeType \@Foo
--         , writeType \@Bar
--         , writeType \@Baz
--         , writeType \@Quux
--         ]
-- @
--
-- With 'discoverInstances', we can skip a lot of this work.
--
-- @
-- import Model.Foo
-- import Model.Bar
-- import Model.Baz
-- import Model.Quux
--
-- renderTypeScript :: IO ()
-- renderTypeScript = do
--     writeToFile $ concat
--         $ flip map $$('discoverInstances' \@TypeScript)
--         $ \\('SomeDictOf' ('Proxy' :: 'Proxy' ty) ->
--             writeType \@ty
-- @
--
-- The above two patterns are encapsulated in 'withInstances'. We can rewrite
-- them like this:
--
-- @
-- entityDefs =
--     'withInstances'
--         $$(discoverInstances \@PersistEntity)
--         $ \\proxy -> entityDef proxy
--
-- renderTypeScript :: IO ()
-- renderTypeScript = do
--     writeToFile $ concat
--         $ 'withInstances' $$('discoverInstances' \@TypeScript)
--         $ \\('Proxy' :: 'Proxy' ty) ->
--             writeType \@ty
-- @
--
-- Another use case is to load all models out of the database, to ensure that
-- serialization and deserialization logic works. 'forInstances' is useful for
-- operating over instances effectfully.
--
-- @
--loadAllModels :: SqlPersistM ()
--loadAllModels = do
--    'forInstances' $$('discoverInstances' \@PersistEntity) \\('Proxy' :: 'Proxy' a) -> do
--        selectList [] []  :: SqlPersistM [Entity a]
-- @

-- | An alias for the pattern:
--
-- @
-- flip map $$discoverInstances $ \\('SomeDictOf' p) -> f p
-- @
--
-- @since 0.1.0.0
withInstances
    :: Functor f
    => f (SomeDict c) -> (forall a. c a => Proxy a -> r) -> f r
withInstances dicts f =
    fmap (\(SomeDictOf p) -> f p) dicts

-- | An alias for the pattern:
--
-- @
-- for $$discoverInstances $ \\('SomeDictOf' p) -> do
--     f p
-- @
--
-- @since 0.1.0.0
forInstances
    :: (Traversable t, Applicative f)
    => t (SomeDict c)
    -> (forall a. c a => Proxy a -> f r)
    -> f (t r)
forInstances dicts f =
    traverse (\(SomeDictOf p) -> f p) dicts

listTE :: [TExp a] -> TExp [a]
listTE = TExp . ListE . map unType

decToDict :: forall k (c :: k -> Constraint). InstanceDec -> Q (TExp [SomeDict c])
decToDict = \case
    InstanceD _moverlap cxt typ _decs ->
        case cxt of
            [] -> do
                let
                    t =
                        case typ of
                            AppT _ t' ->
                               stripSig t'
                            _ ->
                                t
                    stripSig (SigT a _) =
                        a
                    stripSig x =
                        x
                    proxy =
                        [| Proxy :: Proxy $(pure t) |]
                unsafeTExpCoerce [| [ SomeDictOf $proxy ] |]
            _ -> do
                -- reportWarning $
                --     "I haven't figured out how to put constrained instances on here, so I'm skipping the type: "
                --     <> show typ
                --     <> ", context: "
                --     <> show cxt
                examineSplice [|| [] ||]

    _ -> do
        reportWarning $
            "discoverInstances called on 'reifyInstances' somehow returned something that wasn't a type class instance."
        examineSplice [|| [] ||]
