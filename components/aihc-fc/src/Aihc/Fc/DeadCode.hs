-- | Whole-program dead-code elimination for System FC.
module Aihc.Fc.DeadCode
  ( eliminateDeadCode,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (Pred (..), TcType (..), tyConName)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data References = References
  { referencedValues :: !(Set Text),
    referencedTypes :: !(Set Text)
  }

instance Semigroup References where
  References leftValues leftTypes <> References rightValues rightTypes =
    References (leftValues <> rightValues) (leftTypes <> rightTypes)

instance Monoid References where
  mempty = References Set.empty Set.empty

-- | Retain only the value and type declarations transitively reachable from
-- the named entry point. The input is expected to be the combined program so
-- references can cross module and package boundaries.
eliminateDeadCode :: Text -> FcProgram -> FcProgram
eliminateDeadCode entry (FcProgram topBinds) =
  FcProgram
    [ topBind
    | (index, topBind) <- indexedTopBinds,
      keepTopBind valueDefinitions typeDefinitions reachableValues reachableTypes index topBind
    ]
  where
    indexedTopBinds = zip [0 :: Int ..] topBinds
    valueDefinitions =
      Map.fromList
        [ (varName var, (index, references))
        | (index, topBind) <- indexedTopBinds,
          (var, references) <- valueDefinitionsOf topBind
        ]
    typeDefinitions =
      Map.fromList
        [ (name, (index, references))
        | (index, topBind) <- indexedTopBinds,
          (name, references) <- typeDefinitionsOf topBind
        ]
    constructorOwners =
      Map.fromList
        [ (constructor, name)
        | topBind <- topBinds,
          (name, constructors) <- typeConstructorsOf topBind,
          constructor <- constructors
        ]
    (reachableValues, reachableTypes) = closeReachable valueDefinitions typeDefinitions constructorOwners (Set.singleton entry) Set.empty

closeReachable :: Map Text (Int, References) -> Map Text (Int, References) -> Map Text Text -> Set Text -> Set Text -> (Set Text, Set Text)
closeReachable valueDefinitions typeDefinitions constructorOwners values types =
  let references =
        foldMap (definitionReferences valueDefinitions) values
          <> foldMap (definitionReferences typeDefinitions) types
      values' = values <> referencedValues references
      types' =
        types
          <> referencedTypes references
          <> Set.fromList [owner | value <- Set.toList values', Just owner <- [Map.lookup value constructorOwners]]
   in if values' == values && types' == types
        then (values, types)
        else closeReachable valueDefinitions typeDefinitions constructorOwners values' types'

definitionReferences :: Map Text (Int, References) -> Text -> References
definitionReferences definitions name = maybe mempty snd (Map.lookup name definitions)

keepTopBind :: Map Text (Int, References) -> Map Text (Int, References) -> Set Text -> Set Text -> Int -> FcTopBind -> Bool
keepTopBind valueDefinitions typeDefinitions values types index topBind =
  case topBind of
    FcData name _ _ -> selectedType name
    FcNewtype declaration -> selectedType (fcNewtypeName declaration)
    _ -> any selectedValue [varName var | (var, _) <- valueDefinitionsOf topBind]
  where
    selectedValue name = name `Set.member` values && fmap fst (Map.lookup name valueDefinitions) == Just index
    selectedType name = name `Set.member` types && fmap fst (Map.lookup name typeDefinitions) == Just index

valueDefinitionsOf :: FcTopBind -> [(Var, References)]
valueDefinitionsOf topBind =
  case topBind of
    FcPrimitive var _ -> [(var, referencesVarType var)]
    FcForeignImport foreignCall -> [(fcForeignCallVar foreignCall, referencesVarType (fcForeignCallVar foreignCall))]
    FcTopBind bind -> [(var, referencesTopLevelBind bind) | var <- bindersOf bind]
    FcData {} -> []
    FcNewtype {} -> []

typeDefinitionsOf :: FcTopBind -> [(Text, References)]
typeDefinitionsOf topBind =
  case topBind of
    FcData name _ constructors -> [(name, foldMap (foldMap referencesType . snd) constructors)]
    FcNewtype declaration ->
      [ ( fcNewtypeName declaration,
          referencesType (fcNewtypeRepresentation declaration)
            <> referencesType (fcNewtypeResult declaration)
        )
      ]
    _ -> []

typeConstructorsOf :: FcTopBind -> [(Text, [Text])]
typeConstructorsOf topBind =
  case topBind of
    FcData name _ constructors -> [(name, map fst constructors)]
    FcNewtype declaration -> [(fcNewtypeName declaration, [fcNewtypeConstructor declaration])]
    _ -> []

bindersOf :: FcBind -> [Var]
bindersOf bind =
  case bind of
    FcNonRec var _ -> [var]
    FcRec bindings -> map fst bindings

referencesTopLevelBind :: FcBind -> References
referencesTopLevelBind bind =
  case bind of
    FcNonRec var expression -> referencesVarType var <> referencesExpr Set.empty expression
    FcRec bindings ->
      let binders = Set.fromList (map fst bindings)
       in foldMap (\(var, expression) -> referencesVarType var <> referencesExpr binders expression) bindings

referencesExpr :: Set Var -> FcExpr -> References
referencesExpr bound expression =
  case expression of
    FcVar var
      | var `Set.member` bound -> referencesVarType var
      | otherwise -> mempty {referencedValues = Set.singleton (varName var)} <> referencesVarType var
    FcLit literal -> foldMap referencesType (literalType literal)
    FcApp function argument -> referencesExpr bound function <> referencesExpr bound argument
    FcDictApp function argument -> referencesExpr bound function <> referencesExpr bound argument
    FcTyApp inner ty -> referencesExpr bound inner <> referencesType ty
    FcLam var body -> referencesVarType var <> referencesExpr (Set.insert var bound) body
    FcTyLam _ body -> referencesExpr bound body
    FcDictLam var body -> referencesVarType var <> referencesExpr (Set.insert var bound) body
    FcDict fields -> foldMap (referencesExpr bound) fields
    FcDictSelect dictionary _ -> referencesExpr bound dictionary
    FcLet bind body -> referencesLet bound bind body
    FcCase scrutinee binder alternatives ->
      referencesExpr bound scrutinee
        <> referencesVarType binder
        <> foldMap (referencesAlt (Set.insert binder bound)) alternatives
    FcCast inner coercion -> referencesExpr bound inner <> referencesCoercion coercion

referencesLet :: Set Var -> FcBind -> FcExpr -> References
referencesLet bound bind body =
  case bind of
    FcNonRec var rhs ->
      referencesVarType var
        <> referencesExpr bound rhs
        <> referencesExpr (Set.insert var bound) body
    FcRec bindings ->
      let binders = Set.fromList (map fst bindings)
          innerBound = bound <> binders
       in foldMap (\(var, rhs) -> referencesVarType var <> referencesExpr innerBound rhs) bindings
            <> referencesExpr innerBound body

referencesAlt :: Set Var -> FcAlt -> References
referencesAlt bound alternative =
  referencesAltCon (altCon alternative)
    <> foldMap referencesVarType (altBinders alternative)
    <> referencesExpr (bound <> Set.fromList (altBinders alternative)) (altRhs alternative)

referencesAltCon :: FcAltCon -> References
referencesAltCon altCon =
  case altCon of
    DataAlt constructor -> mempty {referencedValues = Set.singleton constructor}
    LitAlt literal -> foldMap referencesType (literalType literal)
    DefaultAlt -> mempty

referencesVarType :: Var -> References
referencesVarType = referencesType . varType

referencesType :: TcType -> References
referencesType ty =
  case ty of
    TcTyVar {} -> mempty
    TcMetaTv {} -> mempty
    TcTyCon tyCon arguments ->
      mempty {referencedTypes = Set.singleton (tyConName tyCon)}
        <> foldMap referencesType arguments
    TcFunTy argument result -> referencesType argument <> referencesType result
    TcForAllTy _ body -> referencesType body
    TcQualTy predicates body -> foldMap referencesPred predicates <> referencesType body
    TcAppTy function argument -> referencesType function <> referencesType argument

referencesPred :: Pred -> References
referencesPred predicate =
  case predicate of
    ClassPred name arguments ->
      mempty {referencedTypes = Set.singleton name}
        <> foldMap referencesType arguments
    EqPred left right -> referencesType left <> referencesType right

referencesCoercion :: Coercion -> References
referencesCoercion coercion =
  case coercion of
    CoVar {} -> mempty
    Refl ty -> referencesType ty
    Sym inner -> referencesCoercion inner
    Trans left right -> referencesCoercion left <> referencesCoercion right
    TyConAppCo tyCon coercions ->
      mempty {referencedTypes = Set.singleton (tyConName tyCon)}
        <> foldMap referencesCoercion coercions
    AxiomInstCo name types ->
      mempty {referencedTypes = Set.singleton name}
        <> foldMap referencesType types
