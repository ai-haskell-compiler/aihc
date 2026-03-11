{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.JsonSpec
  ( HasJsonEncodingSpec(EncodingSpec, toJSONStructure), Ref(Ref)
  , Specification(JsonLet, JsonRef, JsonString)
  )
import Data.Text (Text)
import Prelude (Applicative(pure), ($), (.), Eq, IO, Ord)


main :: IO ()
main =
  {-
    The "test" is that this file compiles at all. It _should_ compile but
    the "stable environment" bug this is testing was causing this to fail
    to compile because the reference to "Baz" via "bar" did not share
    the same environment as the direct reference to Baz in the instance
    `HasJsonEncodingSpec Baz`.  That is to say, the "structure type"
    created by one references was `Ref env1 spec` and `Ref env2 spec`
    for the other reference, where env1 and env2 are not identical
    even though they are functionally equivalent. This means that
    `instance HasJsonEncodingSpec Foo` could not delegate to `instance
    HasJsonEncodingSpec Baz`, which is a major usability problem.

    The solution was to have the environment in the `Ref env spec`
    structure be fixed at the point where the reference is defined,
    not where it is used.
  -}
  pure ()


{-| Shared specification definitions. -}
type TestShared a =
  JsonLet
    '[ '( "Foo"
        , JsonLet '[ '("bar", JsonRef "Baz") ] (JsonRef "bar")
        )
     , '( "Baz" , JsonString)
     ]
    (JsonRef a)


newtype Foo = Foo Baz
  deriving stock (Eq, Ord)
instance HasJsonEncodingSpec Foo where
  type EncodingSpec Foo =
    TestShared "Foo"
  toJSONStructure (Foo val) =
    Ref . Ref . toJSONStructure $ val


newtype Baz = Baz Text
  deriving newtype
    ( Eq
    , Ord
    )
instance HasJsonEncodingSpec Baz where
  type EncodingSpec Baz =
    TestShared "Baz"
  toJSONStructure (Baz val) = Ref val


