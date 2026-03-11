{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- XXX: Lift instances. Don't want to pollute main module with TH shenanigans.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.EDN.QQ
  ( edn
  , ednList
  , ednMap
  , ednVec
  , ednSet
  , fromEDN
  ) where

import Data.Data (Data)
#if MIN_VERSION_base(4,13,0)
#else
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import Data.Typeable (cast)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Exp(..), Lift(..), Q)

import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Language.Haskell.TH.Syntax as TH

import Data.EDN (FromEDN, decodeText, parseText)
import Data.EDN.AST.Types (Tagged(..), Value(..))

-- | Quasiquoter for 'Data.EDN.TaggedValue'.
--
-- @
-- Tagged "foo" "bar" Nil === [edn| #foo/bar nil |]
-- @
edn :: QuasiQuoter
edn = ednQQ $ \str -> do
  src <- fmap TH.loc_filename TH.qLocation

  case parseText src (Text.pack str) of
    Right val ->
      lift val
    Left err ->
      error err

-- | Quasiquoter for untagged 'Value' wrapped in a List.
--
-- @
-- [ednList| #foo/bar nil |]
-- ===
-- List [ Tagged "foo" "bar" Nil ]
-- @
ednList :: QuasiQuoter
ednList = ednQQ $ \str -> do
  src <- fmap TH.loc_filename TH.qLocation
  let doc = "(" <> Text.pack str <> ")"

  case parseText src doc of
    Right (NoTag tv) ->
      lift tv
    Right Tagged{} ->
      error "unexpected tagged value"
    Left err ->
      error err

-- | Quasiquoter for untagged 'Value' wrapped in a Vec.
--
-- @
-- [ednVec| #foo/bar nil |]
-- ===
-- Vec [ Tagged "foo" "bar" Nil ]
-- @
ednVec :: QuasiQuoter
ednVec = ednQQ $ \str -> do
  src <- fmap TH.loc_filename TH.qLocation
  let doc = "[" <> Text.pack str <> "]"

  case parseText src doc of
    Right (NoTag tv) ->
      lift tv
    Right Tagged{} ->
      error "unexpected tagged value"
    Left err ->
      error err

-- | Quasiquoter for untagged 'Value' wrapped in a Set.
--
-- @
-- [ednList| #foo/bar nil |]
-- ===
-- List [ Tagged "foo" "bar" Nil ]
-- @
ednSet :: QuasiQuoter
ednSet = ednQQ $ \str -> do
  src <- fmap TH.loc_filename TH.qLocation
  let doc = "#{" <> Text.pack str <> "}"

  case parseText src doc of
    Right (NoTag tv) ->
      lift tv
    Right Tagged{} ->
      error "unexpected tagged value"
    Left err ->
      error err

-- | Quasiquoter for untagged 'Value' wrapped in a Map.
--
-- @
-- [ednMap| :key value |]
-- ===
-- Map [ (NoTag (Keyword "key"), NoTag (Symbol "" "value")) ]
-- @
ednMap :: QuasiQuoter
ednMap = ednQQ $ \str -> do
  src <- fmap TH.loc_filename TH.qLocation
  let doc = "{" <> Text.pack str <> "}"

  case parseText src doc of
    Right (NoTag tv) ->
      lift tv
    Right Tagged{} ->
      error "unexpected tagged value"
    Left err ->
      error err

-- | Specializable QuasiQuoter for compile-time decoding.
--
-- > ednPerson = fromEDN @Person
--
-- And in another module (a TH restriction):
--
-- > theFred = [ednPerson| #myapp/Person { :first "Fred" } |]
fromEDN :: forall a. (Lift a, FromEDN a) => QuasiQuoter
fromEDN = ednQQ $ \str -> do
  src <- fmap TH.loc_filename TH.qLocation
  case decodeText src (Text.pack str) of
    Left err ->
      error err
    Right (val :: a) ->
      lift val

ednQQ :: (String -> Q Exp) -> QuasiQuoter
ednQQ qexp = QuasiQuoter
  { quoteExp  = qexp
  , quotePat  = error "EDN unavailable in patterns"
  , quoteType = error "EDN unavailable in types"
  , quoteDec  = error "EDN unavailable in declarations"
  }

-- XXX: Workaround for Text.pack not present in the same module with Text constructors.
-- See https://stackoverflow.com/a/38182444
#if MIN_VERSION_base(4,15,0)
liftData' :: (Data a, TH.Quote m) => a -> m Exp
#else
liftData' :: Data a => a -> Q Exp
#endif
liftData' = TH.dataToExpQ $ fmap liftText . cast

#if MIN_VERSION_base(4,15,0)
liftText :: TH.Quote m => Text.Text -> m Exp
#else
liftText :: Text.Text -> Q Exp
#endif
liftText txt = AppE (VarE 'Text.pack) <$> lift (Text.unpack txt)

#if MIN_VERSION_base(4,15,0)
liftVector :: (Lift a, TH.Quote m) => Vector.Vector a -> m Exp
#else
liftVector :: Lift a => Vector.Vector a -> Q Exp
#endif
liftVector vec =
  AppE (VarE 'Vector.fromList) <$> lift (Vector.toList vec)

-- XXX: Workaround for undefined toConstr in Data instance for Vector.
instance Data a => Lift (Tagged Text a) where
  lift = \case
    NoTag val -> do
      val' <- liftData' val
      pure $ ConE 'NoTag `AppE` val'
    Tagged tagNS tag val -> do
      tagNS' <- liftText tagNS
      tag' <- liftText tag
      val' <- liftData' val
      pure $ ConE 'Tagged `AppE` tagNS' `AppE` tag' `AppE` val'

#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . lift
#endif

instance Lift Value where
  lift = \case
    Vec items ->
      AppE (ConE 'Vec) <$> liftVector items
    val ->
      liftData' val

#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . lift
#endif
