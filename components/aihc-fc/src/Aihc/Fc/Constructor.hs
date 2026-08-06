-- | Shared analysis of data constructors declared by System FC programs.
module Aihc.Fc.Constructor
  ( ConstructorTypes,
    declaredConstructorTypes,
    fcDataConstructorType,
    isDeclaredConstructor,
    typesEqual,
  )
where

import Aihc.Fc.Subst (freeRigidTyVarsOf, substType)
import Aihc.Fc.Syntax
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

type ConstructorTypes = Map Text [TcType]

-- | Recover the complete term-level constructor types carried implicitly by
-- the data declarations in a program.
declaredConstructorTypes :: FcProgram -> ConstructorTypes
declaredConstructorTypes (FcProgram tops) =
  Map.fromListWith
    (<>)
    [ (constructor, [fcDataConstructorType (tyVars <> existentialTyVars) fields resultType])
    | FcData typeName tyVars constructors <- tops,
      (constructor, fields) <- constructors,
      let existentialTyVars = filter (`notElem` tyVars) (freeRigidTyVarsOf fields)
          resultType = TcTyCon (TyCon typeName (length tyVars)) (map TcTyVar tyVars)
    ]

-- | Construct the polymorphic term type of a data constructor.
fcDataConstructorType :: [TyVarId] -> [TcType] -> TcType -> TcType
fcDataConstructorType tyVars fields resultType =
  foldr TcForAllTy (foldr TcFunTy resultType fields) tyVars

-- | Whether a variable occurrence is completely determined by one data
-- constructor declaration in the same program.
isDeclaredConstructor :: ConstructorTypes -> Var -> Bool
isDeclaredConstructor constructors variable =
  case Map.lookup (varName variable) constructors of
    Just [declaredType] -> typesEqual declaredType (varType variable)
    _ -> False

-- | Structural type equality with alpha-equivalence for quantified variables.
typesEqual :: TcType -> TcType -> Bool
typesEqual (TcTyVar left) (TcTyVar right) = left == right
typesEqual (TcMetaTv left) (TcMetaTv right) = left == right
typesEqual (TcTyCon leftCon leftArgs) (TcTyCon rightCon rightArgs) =
  leftCon == rightCon
    && length leftArgs == length rightArgs
    && and (zipWith typesEqual leftArgs rightArgs)
typesEqual (TcFunTy leftArg leftResult) (TcFunTy rightArg rightResult) =
  typesEqual leftArg rightArg && typesEqual leftResult rightResult
typesEqual (TcForAllTy leftVar leftBody) (TcForAllTy rightVar rightBody) =
  typesEqual leftBody (substType (Map.singleton rightVar (TcTyVar leftVar)) rightBody)
typesEqual (TcQualTy leftPredicates leftBody) (TcQualTy rightPredicates rightBody) =
  length leftPredicates == length rightPredicates
    && and (zipWith predicatesEqual leftPredicates rightPredicates)
    && typesEqual leftBody rightBody
typesEqual (TcAppTy leftFunction leftArg) (TcAppTy rightFunction rightArg) =
  typesEqual leftFunction rightFunction && typesEqual leftArg rightArg
typesEqual _ _ = False

predicatesEqual :: Pred -> Pred -> Bool
predicatesEqual (ClassPred leftClass leftArgs) (ClassPred rightClass rightArgs) =
  leftClass == rightClass
    && length leftArgs == length rightArgs
    && and (zipWith typesEqual leftArgs rightArgs)
predicatesEqual (EqPred leftA leftB) (EqPred rightA rightB) =
  typesEqual leftA rightA && typesEqual leftB rightB
predicatesEqual _ _ = False
