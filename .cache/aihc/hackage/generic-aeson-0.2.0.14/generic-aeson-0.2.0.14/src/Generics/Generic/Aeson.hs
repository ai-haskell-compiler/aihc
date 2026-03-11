{-# LANGUAGE
    CPP
  , FlexibleContexts
  , FlexibleInstances
  , OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  , TypeOperators
  #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
-- | This module offers generic conversions to and from JSON 'Value's
-- for data types with a 'Generic' instance.
--
-- The structure of the generated JSON is meant to be close to
-- idiomatic JSON. This means:
--
-- * Enumerations are converted to JSON strings.
--
-- * Record fields become JSON keys.
--
-- * Data types with one unlabeled field convert to just that field.
--
-- * Data types with multiple unlabeled fields become arrays.
--
-- * Multiple constructors are represented by keys.
--
-- * 'Maybe' values are either an absent key, or the value.
--
-- See 'tests/Main.hs' for more examples.
module Generics.Generic.Aeson
  ( gtoJson
  , gparseJson
  , GtoJson (..)
  , GfromJson (..)
  , formatLabel
  , Settings (..)
  , defaultSettings
  , gtoJsonWithSettings
  , gparseJsonWithSettings
  ) where

import Control.Applicative
import Control.Monad (unless, when)
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Types hiding (GFromJSON, GToJSON)
import Data.Bifunctor (first)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Generics.Deriving.ConNames
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap   as H
#else
import qualified Data.HashMap.Strict as H
#endif
import qualified Data.Text           as T
import qualified Data.Vector         as V

import Generics.Generic.Aeson.Util

#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key (fromText)
#else
-- for compatibility with aeson < 2
type Key = Text

fromText :: Text -> Key
fromText = id
#endif 

-- | Class for converting the functors from "GHC.Generics" to JSON.
-- You generally don't need to give any custom instances. Just add
-- 'deriving Generic' and call 'gToJson'.
class GtoJson f where
  -- | Generically show a functor as a JSON value.  The first argument
  -- tells us if there are multiple constructors in the data type. The
  -- second indicates if this data type is an enumeration (only empty
  -- constructors). A functor is then converted to either a list
  -- of values (for non-labeled fields) or a list of String/value
  -- pairs (for labeled fields).
  gtoJSONf :: Settings -> Bool -> Bool -> f a -> Either [Value] [(Text, Value)]

-- | Class for parsing the functors from "GHC.Generics" from JSON.
-- You generally don't need to give any custom instances. Just add
-- 'deriving Generic' and call 'gFromJson'.
class GfromJson f where
  -- | Generically read a functor from a JSON value.  The first
  -- argument tells us if there are multiple constructors in the data
  -- type. The second indicates if we've already detected that this
  -- data type has multiple constructors. When this is False, the
  -- (:*:) puts the fields in the state. The third indicates if this
  -- data type is an enumeration (only empty constructors). The third
  -- is a function for parsing the recursive positions. A JSON value
  -- is then parsed to either a functor, or a failure.
  gparseJSONf :: Settings -> Bool -> Bool -> Bool -> StateT [Value] Parser (f a)

-- Void: Used for data types without constructors
-- instance GJSON V1

-- Unit: Used for constructors without arguments
instance GtoJson U1 where
  gtoJSONf _ _ _ U1 = Right []
instance GfromJson U1 where
  gparseJSONf _ _ _ _ = return U1

-- | Convert any datatype with a 'Generic' instance to a JSON 'Value'.

gtoJson
  :: forall a. (Generic a, GtoJson (Rep a), ConNames (Rep a), GIsEnum (Rep a))
  => a -> Value
gtoJson = gtoJsonWithSettings defaultSettings

gtoJsonWithSettings
  :: forall a. (Generic a, GtoJson (Rep a), ConNames (Rep a), GIsEnum (Rep a))
   => Settings -> a -> Value
gtoJsonWithSettings settings x =
  case gtoJSONf settings (multipleConstructors $ conNames x) (isEnum (Proxy :: Proxy a)) (from x) of
    Left  [v] -> v
    Left  _   -> error "The impossible happened: multiple returned values in gtoJSON."
    Right _   -> error "The impossible happened: labeled values returned in gtoJSON."

-- | Parse any datatype with a 'Generic' instance from a JSON 'Value'.
gparseJson
  :: forall a. (Generic a, GfromJson (Rep a), ConNames (Rep a), GIsEnum (Rep a))
  => Value -> Parser a
gparseJson = gparseJsonWithSettings defaultSettings

gparseJsonWithSettings
  :: forall a. (Generic a, GfromJson (Rep a), ConNames (Rep a), GIsEnum (Rep a))
  => Settings -> Value -> Parser a
gparseJsonWithSettings set
  = fmap to
  . evalStateT (gparseJSONf set (multipleConstructors $ conNames (undefined :: a)) False (isEnum (Proxy :: Proxy a)))
  . return

-- Structure type for constant values.
instance ToJSON c => GtoJson (K1 a c) where
  gtoJSONf _ _ _ (K1 a) = Left [toJSON a]
instance FromJSON c => GfromJson (K1 a c) where
  gparseJSONf _ _ _ _   = lift . fmap K1 . parseJSON =<< pop

instance (GtoJson f, GtoJson g) => GtoJson (f :+: g) where
  gtoJSONf set mc enm (L1 x) = gtoJSONf set mc enm x
  gtoJSONf set mc enm (R1 x) = gtoJSONf set mc enm x
instance (GfromJson f, GfromJson g) => GfromJson (f :+: g) where
  gparseJSONf set mc smf enm
    =  L1 <$> gparseJSONf set mc smf enm
   <|> R1 <$> gparseJSONf set mc smf enm

instance (GtoJson f, GtoJson g) => GtoJson (f :*: g) where
  gtoJSONf set mc enm (x :*: y) =
    case (gtoJSONf set mc enm x, gtoJSONf set mc enm y) of
      (Left  xvs, Left  yvs) -> Left  (xvs ++ yvs)
      (Right xvs, Right yvs) -> Right (xvs ++ yvs)
      _                      -> error "The impossible happened: product of mixed label and non-label fields in GJSON instance for (:*:)."
instance (GfromJson f, GfromJson g) => GfromJson (f :*: g) where
  gparseJSONf set mc smf enm =
    do unless smf selFields
       (:*:) <$> gparseJSONf set mc True enm <*> gparseJSONf set mc True enm
    where
      selFields =
        do v <- pop
           case v of
             o@Object{} -> put (repeat o)
             Array vs   -> put (V.toList vs)
             _          -> fail "Expected object or array in gparseJSONf for (:*:)."

instance GtoJson f => GtoJson (M1 D c f) where
  gtoJSONf set a b (M1 x) = gtoJSONf set a b x
instance GfromJson f => GfromJson (M1 D c f) where
  gparseJSONf set a b x = M1 <$> gparseJSONf set a b x

instance (Constructor c, GtoJson f) => GtoJson (M1 C c f) where
  gtoJSONf set _  True  (M1 _) = Left [toJSON $ conNameT set (undefined :: M1 C c f p)]
  gtoJSONf set mc False (M1 x) =
    case gtoJSONf set mc False x of
      -- Single field constructors are not wrapped in an array.
      Left  [v] -> Left [wrap v]
      Left  vs  -> Left [wrap . Array $ V.fromList vs]
      Right vs  -> Left [wrap $ toObject vs]
    where
      wrap = if mc
             then toObject
                . return
                . (conNameT set (undefined :: M1 C c f p), )
             else id
instance (Constructor c, GfromJson f) => GfromJson (M1 C c f) where
  gparseJSONf set mc smf True =
    do str    <- pop
       conStr <- lift (parseJSON str)
       let expectedConStr = conNameT set (undefined :: M1 C c f p)
       unless (conStr == expectedConStr) $
         fail $ "Error parsing enumeration: expected " ++ T.unpack expectedConStr ++ ", found " ++ T.unpack conStr ++ "."
       M1 <$> gparseJSONf set mc smf True
  gparseJSONf set mc smf False =
    do
       when mc (selProp "C" propName)
       M1 <$> gparseJSONf set mc smf False
    where
      propName = case conNameT set (undefined :: M1 C c f p) of
        "" -> Nothing
        n  -> Just n

instance (Selector c, GtoJson f) => GtoJson (M1 S c f) where
  gtoJSONf set mc enm (M1 x) =
    case gtoJSONf set mc enm x of
      Left  [v] -> case selNameT set (undefined :: M1 S c f p) of
        Nothing -> Left [v]
        Just n  -> Right [(n, v)]
      Left  _   -> error "The impossible happened: multiple returned values inside label in GJSON instance for S."
      Right _   -> error "The impossible happened: label inside a label in GJSON instance for S."
instance (Selector c, GfromJson f) => GfromJson (M1 S c f) where
  gparseJSONf set mc smf enm =
    do selProp "S" propName
       M1 <$> gparseJSONf set mc smf enm
    where
      propName = selNameT set (undefined :: M1 S c f p)

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} (Selector c, ToJSON a) => GtoJson (M1 S c (K1 i (Maybe a))) where
#else
instance (Selector c, ToJSON a) => GtoJson (M1 S c (K1 i (Maybe a))) where
#endif
  gtoJSONf set   _  _   (M1 (K1 n@Nothing)) = case selNameT set (undefined :: M1 S c f p) of
    Nothing -> Left [toJSON n]
    Just _  -> Right []
  gtoJSONf set mc enm (M1 (K1 (Just x))) = gtoJSONf set mc enm (M1 (K1 x) :: (M1 S c (K1 i a)) p)

#if __GLASGOW_HASKELL__ >= 710
instance {-# OVERLAPPING #-} (Selector c, FromJSON a) => GfromJson (M1 S c (K1 i (Maybe a))) where
#else
instance (Selector c, FromJSON a) => GfromJson (M1 S c (K1 i (Maybe a))) where
#endif
  gparseJSONf set mc smf enm =
    do M1 (K1 x) <- parser
       return (M1 (K1 (Just x)))
    <|>
    do case selNameT set (undefined :: M1 S c (K1 i a) p) of
         Nothing ->
           do o <- pop
              M1 . K1 <$> lift (parseJSON o)
         Just n  ->
           do o <- pop
              case o of
                Object h | H.member (fromText n) h
                         -> error impossible <$> parser
                         | otherwise
                         -> return $ M1 (K1 Nothing)
                _ -> lift $ typeMismatch "Object" (Array V.empty)
    where
      parser = gparseJSONf set mc smf enm :: StateT [Value] Parser (M1 S c (K1 i a) p)
      impossible = "The impossible happened: parser succeeded after failing in GfromJson S Maybe"

selProp :: Text -> Maybe Text -> StateT [Value] Parser ()
selProp cname propName =
  case propName of
    Nothing -> do o <- pop
                  modify (o:)
    Just p  -> do o <- pop
                  v <- lift (withObject ("Expected property " ++ show propName ++ " in object in gparseJSONf for " ++ show cname ++ ".")
                                        (.: fromText p) o)
                  modify (v:)

pop :: StateT [Value] Parser Value
pop =
  do (v:vs) <- get
     put vs
     return v

toObject :: ToJSON v => [(Text, v)] -> Value
toObject = object . map (uncurry (.=) . first fromText)
