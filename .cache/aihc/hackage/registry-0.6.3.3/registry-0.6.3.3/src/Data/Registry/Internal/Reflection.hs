{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}

-- |
--  Utility functions to display or manipulate types
module Data.Registry.Internal.Reflection where

import Data.Semigroup
import Data.Text as T
import Data.Typeable (splitTyConApp)
import Protolude as P hiding (intercalate, TypeRep, isPrefixOf, (<>))

import GHC.Exts
import Type.Reflection as Reflection

-- | Return true if the type of this type rep represents a function
isFunction :: SomeTypeRep -> Bool
isFunction d =
  case d of
    SomeTypeRep (Fun _ _) -> True
    _other -> False

-- | Show the full type of a typeable value
showFullValueType :: Typeable a => a -> Text
showFullValueType = showTheFullValueType . Reflection.typeOf

-- | Show the full type of a typeable function
showFullFunctionType :: Typeable a => a -> ([Text], Text)
showFullFunctionType = showTheFullFunctionType . Reflection.typeOf

-- | Show the full type of a typeable value
--   where nested types like @IO[Int]@ or functions are represented and
--   non GHC types are shown with their module names
showTheFullValueType :: forall (r1 :: RuntimeRep) (arg :: TYPE r1). (TypeRep arg -> Text)
showTheFullValueType a =
  case a of
    Fun (App t1 t2) t3 ->
      showNested (SomeTypeRep t1) (SomeTypeRep t2) <> " -> " <> showTheFullValueType t3
    Fun t1 t2 ->
      showTheFullValueType t1 <> " -> " <> showTheFullValueType t2
    App t1 t2 ->
      showNested (SomeTypeRep t1) (SomeTypeRep t2)
    _other ->
      showSingleType (SomeTypeRep a)

-- | Show the full type of a typeable value
--   where nested types like IO[Int] or functions are represented and
--   non GHC types are shown with their module names
showTheFullFunctionType :: forall (r1 :: RuntimeRep) (arg :: TYPE r1). (TypeRep arg -> ([Text], Text))
showTheFullFunctionType a =
  case a of
    Fun (App t1 t2) t3 ->
      let (ins, out) = showTheFullFunctionType t3
       in (showNested (SomeTypeRep t1) (SomeTypeRep t2) : ins, out)
    Fun t1 t2 ->
      let in1 = showTheFullValueType t1
          (ins, out) = showTheFullFunctionType t2
       in (in1 : ins, out)
    App t1 t2 ->
      ([], showNested (SomeTypeRep t1) (SomeTypeRep t2))
    _other ->
      ([], showSingleType (SomeTypeRep a))

-- | Show a type like @m a@
showNested :: SomeTypeRep -> SomeTypeRep -> Text
showNested a b =
  parenthesizeNested $ tweakNested $ showSingleType a <> " " <> showSingleType b

-- | Show a single type. Don't display the module for GHC types
showSingleType :: SomeTypeRep -> Text
showSingleType a =
  case splitTyConApp a of
    (con, []) -> showType con
    (con, [arg]) -> showType con <> " " <> showSingleType arg
    (con, args) -> showType con <> " " <> P.show (fmap showSingleType args)
  where
    showType x =
      let typeWithModuleName = showWithModuleName x
       in if mustShowModuleName typeWithModuleName then typeWithModuleName else P.show x

-- | Return true if the module name can be shown
mustShowModuleName :: Text -> Bool
mustShowModuleName name =
  not $
    P.any identity $
      fmap
        (`isPrefixOf` name)
        [ "GHC.Types.", -- for Int, Double,..
          "GHC.Base.", -- for other Base types
          "GHC.Maybe.", -- for Maybe
          "GHC.Internal.Maybe.", -- for Maybe and more recent GHC versions
          "Data.Either.", -- for Either
          "GHC.Internal.Data.Either.", -- for Either and more recent GHC versions
          "Data.Text.", -- for Text
          "GHC.Internal.Data.Text." -- for Text and more recent GHC versions
        ]

-- | Tweak some standard module names for better display
tweakNested :: Text -> Text
tweakNested "List Char" = "String"
tweakNested n =
  if "List " `isPrefixOf` n
    then "[" <> T.drop 5 n <> "]" -- special processing for lists
    else n

-- | This is an attempt to better render "nested" types like IO (Maybe Text)
--   The input value is @"IO Maybe Text"@ and the output text will be @"IO (Maybe Text)"@
--   This will unfortunately not work with types having several type parameters
--   like @IO (Either Text Int)@
parenthesizeNested :: Text -> Text
parenthesizeNested t =
  case T.splitOn " " t of
    [] -> t
    [_head] -> t
    [outer, inner] -> outer <> " " <> inner
    outer : rest -> outer <> " (" <> parenthesizeNested (T.intercalate " " rest) <> ")"

-- | Show a type constructor with its module name
showWithModuleName :: TyCon -> Text
showWithModuleName t = toS $ tyConModule t <> "." <> tyConName t
