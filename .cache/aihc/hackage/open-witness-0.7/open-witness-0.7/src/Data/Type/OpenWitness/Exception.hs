{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS -fno-warn-unused-matches #-}

module Data.Type.OpenWitness.Exception
    ( Exn
    , declexn
    , throw
    , catch
    ) where

import Control.Exception qualified as CE (Exception, catch, throw)
import Data.Kind
import Data.Type.OpenWitness
import Data.Type.Witness
import Language.Haskell.TH hiding (Type)
import Prelude

-- | A key to match exceptions. The type variable is the data the exception carries.
newtype Exn (e :: Type) =
    MkExn (IOWitness e)

instance TestEquality Exn where
    testEquality (MkExn a) (MkExn b) = testEquality a b

newtype ExnException =
    MkExnException (SomeOf Exn)

-- | Template Haskell function to declare 'Exn' exception keys.
declexn :: TypeQ -> Q Exp
declexn te = [|MkExn $(iowitness te)|]

instance Show ExnException where
    show _ = "ExnException"

instance CE.Exception ExnException

throw :: Exn e -> e -> a
throw exn e = CE.throw (MkExnException (MkSomeOf exn e))

catch :: IO a -> Exn e -> (e -> IO a) -> IO a
catch foo exn catcher =
    CE.catch
        foo
        (\ex@(MkExnException cell) ->
             case matchSomeOf exn cell of
                 Just e -> catcher e
                 _ -> CE.throw ex)
