module THLego.Helpers where

import qualified Data.Text as Text
import Language.Haskell.TH.Syntax
import THLego.Prelude
import qualified TemplateHaskell.Compat.V0208 as Compat

-- * Decs

typeSynonymDec :: Name -> Type -> Dec
typeSynonymDec a b =
  TySynD a [] b

recordNewtypeDec :: Name -> Name -> Type -> Dec
recordNewtypeDec _name _accessorName _type =
  NewtypeD [] _name [] Nothing _con []
  where
    _con =
      RecC _name [(_accessorName, noBang, _type)]

normalNewtypeDec :: Name -> Type -> Dec
normalNewtypeDec _name _type =
  NewtypeD [] _name [] Nothing _con []
  where
    _con =
      NormalC _name [(noBang, _type)]

recordAdtDec :: Name -> [(Name, Type)] -> Dec
recordAdtDec typeName fields =
  DataD [] typeName [] Nothing [con] []
  where
    con =
      RecC typeName (fmap (\(fieldName, fieldType) -> (fieldName, fieldBang, fieldType)) fields)

productAdtDec :: Name -> [Type] -> Dec
productAdtDec typeName memberTypes =
  DataD [] typeName [] Nothing [con] []
  where
    con =
      NormalC typeName (fmap ((fieldBang,)) memberTypes)

sumAdtDec :: Name -> [(Name, [Type])] -> Dec
sumAdtDec a b =
  DataD [] a [] Nothing (fmap (uncurry sumCon) b) []

sumCon :: Name -> [Type] -> Con
sumCon a b =
  NormalC a (fmap (fieldBang,) b)

enumDec :: Name -> [Name] -> Dec
enumDec a b =
  DataD [] a [] Nothing (fmap (\c -> NormalC c []) b) []

textName :: Text -> Name
textName =
  mkName . Text.unpack

textTyLit :: Text -> TyLit
textTyLit =
  StrTyLit . Text.unpack

noBang :: Bang
noBang =
  Bang NoSourceUnpackedness NoSourceStrictness

fieldBang :: Bang
fieldBang =
  Bang NoSourceUnpackedness SourceStrict

multiAppT :: Type -> [Type] -> Type
multiAppT base args =
  foldl' AppT base args

multiAppE :: Exp -> [Exp] -> Exp
multiAppE base args =
  foldl' AppE base args

arrowChainT :: [Type] -> Type -> Type
arrowChainT params result =
  foldr (\a b -> AppT (AppT ArrowT a) b) result params

appliedTupleT :: [Type] -> Type
appliedTupleT a =
  foldl' AppT (TupleT (length a)) a

appliedTupleOrSingletonT :: [Type] -> Type
appliedTupleOrSingletonT =
  \case
    [a] -> a
    a -> appliedTupleT a

appliedTupleE :: [Exp] -> Exp
appliedTupleE =
  Compat.tupE

appliedTupleOrSingletonE :: [Exp] -> Exp
appliedTupleOrSingletonE =
  \case
    [a] -> a
    a -> appliedTupleE a

nameString :: Name -> String
nameString (Name (OccName x) _) =
  x

decimalIndexName :: Int -> Name
decimalIndexName =
  mkName . showChar '_' . show

alphabeticIndexName :: Int -> Name
alphabeticIndexName a =
  mkName string
  where
    string =
      showIntAtBase 26 (chr . (+) 97) a ""

enumAlphabeticNames :: Int -> [Name]
enumAlphabeticNames =
  fmap alphabeticIndexName . enumFromTo 0 . pred

-- |
-- Map every element of a list with a new name.
{-# INLINE mapWithAlphabeticName #-}
mapWithAlphabeticName :: (Name -> a -> b) -> [a] -> [b]
mapWithAlphabeticName f list =
  foldr step (const []) list 0
  where
    step a next !index =
      f (alphabeticIndexName index) a : next (succ index)

aName :: Name
aName =
  mkName "a"

bName :: Name
bName =
  mkName "b"

cName :: Name
cName =
  mkName "c"

eqConstraintT :: Name -> Type -> Type
eqConstraintT name =
  AppT (AppT EqualityT (VarT name))

applicativeChainE :: Exp -> [Exp] -> Exp
applicativeChainE mappingE apEList =
  case apEList of
    h : t ->
      intersperseInfixE
        (VarE '(<*>))
        (InfixE (Just mappingE) (VarE '(<$>)) (Just h) :| t)
    _ ->
      AppE (VarE 'pure) mappingE

intersperseInfixE :: Exp -> NonEmpty Exp -> Exp
intersperseInfixE op =
  foldl1 (\l r -> InfixE (Just l) op (Just r))

textLitE :: Text -> Exp
textLitE =
  LitE . StringL . Text.unpack
