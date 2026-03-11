module Language.C99.Simple.Util where

import GHC.Exts (fromList)

import           Language.C99.Simple.AST
import qualified Language.C99.AST         as C

import Language.C99.Util


-- Append two declaration specs
appendspecs :: C.DeclnSpecs -> C.DeclnSpecs -> C.DeclnSpecs
appendspecs x y = let rec x' = Just $ appendspecs x' y in case x of
  C.DeclnSpecsType ts Nothing  -> C.DeclnSpecsType ts (Just y)
  C.DeclnSpecsQual qs Nothing  -> C.DeclnSpecsQual qs (Just y)

  C.DeclnSpecsType ts (Just x) -> C.DeclnSpecsType ts (rec x)
  C.DeclnSpecsQual qs (Just x) -> C.DeclnSpecsQual qs (rec x)

-- Insert a pointer into a declaration
insertptr :: C.Ptr -> C.Declr -> C.Declr
insertptr ptr (C.Declr Nothing     declr) = C.Declr (Just ptr) declr
insertptr ptr (C.Declr (Just ptr') declr) = C.Declr (Just $ appendptr ptr ptr') declr

-- Append pointers, giving a pointer level of the sum of both
appendptr :: C.Ptr -> C.Ptr -> C.Ptr
appendptr (C.PtrBase quals)      ptr = C.PtrCons quals ptr
appendptr (C.PtrCons quals ptr') ptr = C.PtrCons quals (appendptr ptr ptr')

-- Keep taking qualifiers as long as possible and return the remainder
gettypequals :: Type -> (Maybe C.TypeQualList, Type)
gettypequals ty = (f quals, ty') where
  f [] = Nothing
  f xs = Just $ fromList xs
  (quals, ty') = gettypequals' ty
  gettypequals' ty = case ty of
    Const    ty' -> rec C.QConst    ty'
    Restrict ty' -> rec C.QRestrict ty'
    Volatile ty' -> rec C.QVolatile ty'
    _            -> ([], ty)
  rec qual ty = let (quals, ty') = gettypequals' ty in (qual:quals, ty')

-- Turn a declr in an array by appending an ArrayDeclr
declrarray :: C.Declr -> Maybe C.AssignExpr -> C.Declr
declrarray (C.Declr ptr ddeclr) mexpr =
  C.Declr ptr (C.DirectDeclrArray1 ddeclr Nothing mexpr)

-- Takes a list of C.TypeSpec and turns it into a C.DeclnSpecs
foldtypespecs :: [C.TypeSpec] -> C.DeclnSpecs
foldtypespecs ts = foldtypespecs' (reverse ts) where
  foldtypespecs' []     = error "DeclnSpecs can't be empty"
  foldtypespecs' (t:ts) = foldl step base ts where
    base     = C.DeclnSpecsType t Nothing
    step x y = C.DeclnSpecsType y (Just x)

-- Takes a list of C.TypeSpec and turns it into a C.SpecQualsList
foldtypequals :: [C.TypeSpec] -> C.SpecQualList
foldtypequals ts = foldtypequals' (reverse ts) where
  foldtypequals' []     = error "SpecQualList can't be empty"
  foldtypequals' (t:ts) = foldl step base ts where
    base     = C.SpecQualType t Nothing
    step x y = C.SpecQualType y (Just x)

-- Decay a type: turn an toplevel array into a pointer, usefull for functions
-- returning an array.
decay :: Type -> Type
decay (Array ty len) = Ptr ty
decay ty             = ty
