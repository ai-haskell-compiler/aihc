module Type where

import qualified Data.Foldable as Fold
import qualified Data.List.Match as Match
import qualified Data.List as List
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty, (<>))
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (fst3)

import Control.Monad (mfilter)
import Control.Applicative (liftA2, liftA3, (<$>), (<*>))

import Text.Printf (printf)


data
   Mono =
      Logical | Character | Integer |
      RealSingle | RealDouble | ComplexSingle | ComplexDouble
   deriving (Eq, Show)


data Poly = Variable Var | Mono Mono
   deriving (Eq)

data Var = Real | Complex
   deriving (Eq)

isVar :: Poly -> Bool
isVar (Variable _) = True
isVar (Mono _) = False


unifyPrecision :: Mono -> Mono -> Maybe Poly
unifyPrecision RealSingle RealDouble = Just $ Variable Real
unifyPrecision ComplexSingle ComplexDouble = Just $ Variable Complex
unifyPrecision floatType doubleType =
   toMaybe (floatType == doubleType) $ Mono floatType 

unifyRealComplex :: Poly -> Poly -> Maybe Poly
unifyRealComplex (Variable real) (Variable complex) =
   toMaybe ((real, complex) == (Real, Complex)) $ Variable Real
unifyRealComplex realType complexType =
   toMaybe (realType == complexType) realType



class Foldable f => Unification f where
   unify :: (a -> b -> Maybe c) -> f a -> f b -> Maybe (f c)

instance Unification [] where
   unify uni as bs =
      mfilter (const $ Match.equalLength as bs) $ sequence $ zipWith uni as bs

instance Unification Maybe where
   unify uni ma mb =
      case (ma,mb) of
         (Nothing, Nothing) -> Just Nothing
         (Just a, Just b) -> Just <$> uni a b
         _ -> Nothing

unifyEq :: (a -> a -> Bool) -> a -> a -> Maybe a
unifyEq eq a b = toMaybe (eq a b) a


data Pointer typ = Ptr typ | FunPtr Int typ typ

instance Foldable Pointer where
   foldMap f (Ptr typ) = f typ
   foldMap f (FunPtr _n param ret) = f param <> f ret

instance Unification Pointer where
   unify uni (Ptr aTyp) (Ptr bTyp) = Ptr <$> uni aTyp bTyp
   unify uni (FunPtr an aParam aReturn) (FunPtr bn bParam bReturn) =
      FunPtr
         <$> unifyEq (==) an bn <*> uni aParam bParam <*> uni aReturn bReturn
   unify _ _ _ = Nothing


data Foreign typ = Foreign [Pointer typ] (Maybe typ)

instance Foldable Foreign where
   foldMap f (Foreign params returnType) =
      foldMap (foldMap f) params <> foldMap f returnType

instance Unification Foreign where
   unify uni (Foreign aParams aReturn) (Foreign bParams bReturn) =
      liftA2 Foreign
         (unify (unify uni) aParams bParams) (unify uni aReturn bReturn)

unifySignature ::
   (Unification f) => (a -> b -> Maybe Poly) -> f a -> f b -> Maybe (f Poly)
unifySignature uni floatSig doubleSig =
   mfilter (Fold.any isVar) $ unify uni floatSig doubleSig


data ComfortArray = ComfortArray
data CArray = CArray

class Array array where
   arrayFromWrapper :: Wrapper array typ -> array
   formatArrayName :: array -> Bool -> String
   formatDimName :: array -> String
   caseComfortCArray :: array -> a -> a -> a

instance Array ComfortArray where
   arrayFromWrapper _ = ComfortArray
   formatArrayName ComfortArray mutable =
      (if mutable then "IO" else "") ++ "Array"
   formatDimName ComfortArray = "ZeroInt"
   caseComfortCArray ComfortArray x _ = x

instance Array CArray where
   arrayFromWrapper _ = CArray
   formatArrayName CArray mutable = (if mutable then "IO" else "") ++ "CArray"
   formatDimName CArray = "Int"
   caseComfortCArray CArray _ x = x

data Wrapper array typ =
   Wrapper
      [(typ, Mapping String typ, Bool)] (Maybe typ) [(typ, Mapping String typ)]

instance Foldable (Wrapper array) where
   foldMap f (Wrapper inputs returnType outputs) =
      foldMap (f.fst3) inputs <> foldMap f returnType <> foldMap (f.fst) outputs

instance Unification (Wrapper array) where
   unify uni
         (Wrapper aInputs aReturn aOutputs) (Wrapper bInputs bReturn bOutputs) =

      liftA3 Wrapper
         (unify
            (\(aType,aDims,aMut) (bType,bDims,bMut) ->
               liftA3 (,,)
                  (uni aType bType)
                  (unify uni aDims bDims)
                  (unifyEq (==) aMut bMut))
            aInputs bInputs)
         (unify uni aReturn bReturn)
         (unify
            (\(aType,aDims) (bType,bDims) ->
               liftA2 (,)
                  (uni aType bType)
                  (unify uni aDims bDims))
            aOutputs bOutputs)



class Format typ where
   format :: typ -> String
   formatParameter :: typ -> String


formatCallback :: Int -> Mono -> Mono -> String
formatCallback n param ret =
   concat (replicate n (formatPtr param ++ " -> "))
   ++
   "IO " ++ format ret

formatTuple :: [String] -> String
formatTuple elems =
   case elems of
      [] -> "()"
      [x] -> x
      _ -> "(" ++ List.intercalate "," elems ++ ")"

formatShape :: (Array array) => array -> [String] -> String
formatShape array = formatTuple . flip Match.replicate (formatDimName array)

formatPtr :: (Format typ) => typ -> String
formatPtr typ = "Ptr " ++ format typ

formatFunPtr :: (Format typ) => Int -> typ -> typ -> String
formatFunPtr n param ret =
   printf "FunPtr (%sIO %s)"
      (concat $ replicate n $ formatPtr param ++ " -> ") (format ret)

formatArray ::
   (Array array, Format typ) =>
   array -> Bool -> typ -> Mapping String typ -> String
formatArray array mutable elm mapping =
   case mapping of
      Scalar -> formatParameter elm
      Array dims ->
         formatArrayName array mutable ++ " " ++
         formatShape array dims ++ " " ++ format elm
      Function n param -> formatFunPtr n param elm

instance Format Mono where
   format typ =
      case typ of
         Logical -> "Bool"
         Character -> "CChar"
         Integer -> "CInt"
         RealSingle -> "Float"
         RealDouble -> "Double"
         ComplexSingle -> "(Complex Float)"
         ComplexDouble -> "(Complex Double)"

   formatParameter typ =
      case typ of
         Logical -> "Bool"
         Character -> "Char"
         Integer -> "Int"
         RealSingle -> "Float"
         RealDouble -> "Double"
         ComplexSingle -> "Complex Float"
         ComplexDouble -> "Complex Double"


formatVariable :: Var -> String
formatVariable Real = "a"
formatVariable Complex = "(Complex a)"

formatVarFunPtr :: (Format typ) => Int -> typ -> Var -> String
formatVarFunPtr n param ret =
   printf "FunPtr (%sIO %s)"
      (concat $ replicate n $ formatPtr param ++ " -> ") (formatVariable ret)

instance Format Poly where
   format (Variable var) = formatVariable var
   format (Mono typ) = format typ

   formatParameter (Variable var) = formatVariable var
   formatParameter (Mono typ) = formatParameter typ


formatForeignParam :: Format a => Pointer a -> String
formatForeignParam pointer =
   case pointer of
      Ptr typ -> formatPtr typ
      FunPtr n param ret -> formatFunPtr n param ret

formatForeign :: Format a => Foreign a -> String
formatForeign (Foreign params returnType) =
   concatMap ((++ " -> ") . formatForeignParam) params ++
   "IO " ++ maybe "()" format returnType



data Mapping dim typ = Scalar | Array [dim] | Function Int typ
   deriving (Eq)

instance Foldable (Mapping dim) where
   foldMap f m =
      case m of
         Scalar -> mempty
         Array _ -> mempty
         Function _ param -> f param

instance Unification (Mapping dim) where
   unify _uni Scalar Scalar = Just Scalar
   unify _uni (Array aDims) (Array bDims) =
      Array <$> unifyEq Match.equalLength aDims bDims
   unify uni (Function an aParam) (Function bn bParam) =
      Function <$> unifyEq (==) an bn <*> uni aParam bParam
   unify _uni _ _ = Nothing

mapMappings :: ([dim0] -> [dim1]) -> Mapping dim0 typ -> Mapping dim1 typ
mapMappings f m =
   case m of
      Scalar -> Scalar
      Array dims -> Array $ f dims
      Function n param -> Function n param

mapMapping :: (dim0 -> dim1) -> Mapping dim0 typ -> Mapping dim1 typ
mapMapping f = mapMappings (fmap f)

isScalar :: Mapping dim typ -> Bool
isScalar Scalar = True
isScalar _ = False

maybeArray :: Mapping dim typ -> Maybe [dim]
maybeArray (Array dims) = Just dims
maybeArray _ = Nothing
