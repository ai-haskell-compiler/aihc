-- | Make System FC local names easier to read.
--
-- Each optimizer pass ends with a tidy, so a whole program goes through
-- this module many times. Two rules keep that cheap in memory. A part that
-- the tidy does not change stays the same heap object. The result is built
-- strictly, so no unevaluated tidy keeps the program of an earlier pass.
module Aihc.Fc.Tidy
  ( tidyProgram,
    tidyProgramWithTidiedImports,
    tidyTypeEnv,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv (..))
import Aihc.Tc.Types (Unique (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data TidyEnv = TidyEnv
  { tidyNames :: !(Map Name Name),
    tidyUsed :: !(Map Text (Set Int))
  }

emptyTidyEnv :: TidyEnv
emptyTidyEnv = TidyEnv Map.empty Map.empty

-- | The tidy of one part: the part does not change, or it is a new object.
data Tidied a
  = Same
  | Changed !a

-- | The part after its tidy.
result :: a -> Tidied a -> a
result old part =
  case part of
    Same -> old
    Changed new -> new

-- | Keep a part that did not change, else tidy it.
tidied :: (a -> Tidied a) -> a -> a
tidied tidy old = result old (tidy old)

-- | Make a new object from its parts when a part changed. The parts go
-- into the object evaluated, so the object does not keep the old parts.
rebuild1 :: (a -> r) -> a -> Tidied a -> Tidied r
rebuild1 make _ ta =
  case ta of
    Same -> Same
    Changed a' -> Changed (make a')

rebuild2 :: (a -> b -> r) -> a -> Tidied a -> b -> Tidied b -> Tidied r
rebuild2 make a ta b tb =
  case (ta, tb) of
    (Same, Same) -> Same
    _ ->
      let !a' = result a ta
          !b' = result b tb
       in Changed (make a' b')

rebuild3 :: (a -> b -> c -> r) -> a -> Tidied a -> b -> Tidied b -> c -> Tidied c -> Tidied r
rebuild3 make a ta b tb c tc =
  case (ta, tb, tc) of
    (Same, Same, Same) -> Same
    _ ->
      let !a' = result a ta
          !b' = result b tb
          !c' = result c tc
       in Changed (make a' b' c')

rebuild4 :: (a -> b -> c -> d -> r) -> a -> Tidied a -> b -> Tidied b -> c -> Tidied c -> d -> Tidied d -> Tidied r
rebuild4 make a ta b tb c tc d td =
  case (ta, tb, tc, td) of
    (Same, Same, Same, Same) -> Same
    _ ->
      let !a' = result a ta
          !b' = result b tb
          !c' = result c tc
          !d' = result d td
       in Changed (make a' b' c' d')

tidyNonEmpty :: (a -> Tidied a) -> NE.NonEmpty a -> Tidied (NE.NonEmpty a)
tidyNonEmpty tidy (first NE.:| rest) = rebuild2 (NE.:|) first (tidy first) rest (tidyList tidy rest)

-- | Tidy each element of a list.
tidyList :: (a -> Tidied a) -> [a] -> Tidied [a]
tidyList tidy olds = collect olds (map tidy olds)

-- | Join the tidies of the elements of a list. Each tidy is evaluated
-- before the list is known to be the same or new.
collect :: [a] -> [Tidied a] -> Tidied [a]
collect olds parts = go False parts
  where
    go changed remaining =
      case remaining of
        [] -> if changed then Changed (rebuildList olds parts) else Same
        Same : rest -> go changed rest
        Changed _ : rest -> go True rest

rebuildList :: [a] -> [Tidied a] -> [a]
rebuildList olds parts =
  case (olds, parts) of
    (old : restOlds, part : restParts) ->
      let !new = result old part
          !rest = rebuildList restOlds restParts
       in new : rest
    _ -> []

-- | Give each local name the lowest number that its lexical scope permits.
tidyProgram :: Program -> Program
tidyProgram program =
  let !imports = tidyImports (programImports program)
      !decls = tidyDecls (programDecls program)
   in program {programImports = imports, programDecls = decls}

tidyProgramWithTidiedImports :: Program -> Program
tidyProgramWithTidiedImports program =
  let !decls = tidyDecls (programDecls program)
   in program {programDecls = decls}

tidyDecls :: [Decl] -> [Decl]
tidyDecls decls = result decls (tidyList tidyDecl decls)

tidyTypeEnv :: TypeEnv -> TypeEnv
tidyTypeEnv env =
  let !headers = Map.map (tidied (tidyType emptyTidyEnv)) (teHeaders env)
      !synonyms = Map.map (tidied (tidyType emptyTidyEnv)) (teSynonyms env)
      !axioms = Map.map (tidied tidyAxiomDecl) (teAxioms env)
      !binders = Map.map (tidied (tidyType emptyTidyEnv)) (teBinders env)
   in env
        { teHeaders = headers,
          teSynonyms = synonyms,
          teAxioms = axioms,
          teBinders = binders
        }

tidyImports :: Imports -> Imports
tidyImports imports =
  let !headers = Map.map (tidied (tidyType emptyTidyEnv)) (importHeaders imports)
      !synonyms = Map.map (tidied (tidyType emptyTidyEnv)) (importSynonyms imports)
      !axioms = Map.map (tidied tidyAxiomDecl) (importAxioms imports)
      !binders = Map.map (tidied (tidyType emptyTidyEnv)) (importBinders imports)
   in imports
        { importHeaders = headers,
          importSynonyms = synonyms,
          importAxioms = axioms,
          importBinders = binders
        }

tidyDecl :: Decl -> Tidied Decl
tidyDecl decl =
  case decl of
    DeclType declaration ->
      let (binders, env) = tidyBinders emptyTidyEnv (typeBinders declaration)
       in rebuild3
            (\binders' result' constructors' -> DeclType declaration {typeBinders = binders', typeResult = result', typeCons = constructors'})
            (typeBinders declaration)
            binders
            (typeResult declaration)
            (tidyType env (typeResult declaration))
            (typeCons declaration)
            (tidyList tidyConDecl (typeCons declaration))
    DeclSynonym declaration ->
      let (binders, env) = tidyBinders emptyTidyEnv (synBinders declaration)
       in rebuild3
            (\binders' result' body' -> DeclSynonym declaration {synBinders = binders', synResult = result', synBody = body'})
            (synBinders declaration)
            binders
            (synResult declaration)
            (tidyType env (synResult declaration))
            (synBody declaration)
            (tidyType env (synBody declaration))
    DeclAxiom declaration -> rebuild1 DeclAxiom declaration (tidyAxiomDecl declaration)
    DeclVal declaration ->
      rebuild2
        (\type' body' -> DeclVal declaration {valType = type', valBody = body'})
        (valType declaration)
        (tidyType emptyTidyEnv (valType declaration))
        (valBody declaration)
        (tidyExpr emptyTidyEnv (valBody declaration))
    DeclRule declaration ->
      let (typeBinders, typeEnv) = tidyBinders emptyTidyEnv (ruleTypeBinders declaration)
          (binders, env) = tidyBinders typeEnv (ruleBinders declaration)
          types = rebuild2 (,) (ruleTypeBinders declaration) typeBinders (ruleBinders declaration) binders
       in rebuild4
            ( \(typeBinders', binders') type' lhs' rhs' ->
                DeclRule
                  declaration
                    { ruleTypeBinders = typeBinders',
                      ruleBinders = binders',
                      ruleType = type',
                      ruleLhs = lhs',
                      ruleRhs = rhs'
                    }
            )
            (ruleTypeBinders declaration, ruleBinders declaration)
            types
            (ruleType declaration)
            (tidyType env (ruleType declaration))
            (ruleLhs declaration)
            (tidyExpr env (ruleLhs declaration))
            (ruleRhs declaration)
            (tidyExpr env (ruleRhs declaration))

tidyConDecl :: ConDecl -> Tidied ConDecl
tidyConDecl declaration =
  rebuild1 (\type' -> declaration {conType = type'}) (conType declaration) (tidyType emptyTidyEnv (conType declaration))

tidyAxiomDecl :: AxiomDecl -> Tidied AxiomDecl
tidyAxiomDecl declaration =
  let (binders, env) = tidyBinders emptyTidyEnv (axiomBinders declaration)
   in rebuild3
        (\binders' left' right' -> declaration {axiomBinders = binders', axiomLeft = left', axiomRight = right'})
        (axiomBinders declaration)
        binders
        (axiomLeft declaration)
        (tidyType env (axiomLeft declaration))
        (axiomRight declaration)
        (tidyType env (axiomRight declaration))

tidyType :: TidyEnv -> Type -> Tidied Type
tidyType env ty =
  case ty of
    TyVar name -> rebuild1 TyVar name (tidyUse env name)
    TyCon name -> rebuild1 TyCon name (tidyUse env name)
    TyApp function argument ->
      rebuild2 TyApp function (tidyType env function) argument (tidyType env argument)
    TyFun r1 r2 argument resultType ->
      rebuild4
        TyFun
        r1
        (tidyType env r1)
        r2
        (tidyType env r2)
        argument
        (tidyType env argument)
        resultType
        (tidyType env resultType)
    TyForAll binder body ->
      let (binder', bodyEnv) = tidyBinder env binder
       in rebuild2 TyForAll binder binder' body (tidyType bodyEnv body)
    TyEq left right -> rebuild2 TyEq left (tidyType env left) right (tidyType env right)
    TyLit kindName literal -> rebuild1 (`TyLit` literal) kindName (tidyUse env kindName)

tidyExpr :: TidyEnv -> Expr -> Tidied Expr
tidyExpr env expr =
  case expr of
    ExVar name -> rebuild1 ExVar name (tidyUse env name)
    ExLit literal ty -> rebuild2 ExLit literal (tidyLiteral env literal) ty (tidyType env ty)
    ExApp function argument ->
      rebuild2 ExApp function (tidyExpr env function) argument (tidyExpr env argument)
    ExTyApp function argument ->
      rebuild2 ExTyApp function (tidyExpr env function) argument (tidyType env argument)
    ExForeignCall call types arguments ->
      rebuild3
        ExForeignCall
        call
        -- The foreign type is closed, but its binders take names that no
        -- enclosing binder has, so that no binder of the declaration repeats.
        (rebuild1 (\type' -> call {foreignCallType = type'}) (foreignCallType call) (tidyType env (foreignCallType call)))
        types
        (tidyList (tidyType env) types)
        arguments
        (tidyList (tidyExpr env) arguments)
    ExLam binder body ->
      let (binder', bodyEnv) = tidyBinder env binder
       in rebuild2 ExLam binder binder' body (tidyExpr bodyEnv body)
    ExTyLam binder body ->
      let (binder', bodyEnv) = tidyBinder env binder
       in rebuild2 ExTyLam binder binder' body (tidyExpr bodyEnv body)
    ExLet bind body ->
      let (binder', bodyEnv) = tidyBinder env (bindBinder bind)
          bind' = rebuild2 Bind (bindBinder bind) binder' (bindRhs bind) (tidyExpr env (bindRhs bind))
       in rebuild2 ExLet bind bind' body (tidyExpr bodyEnv body)
    ExRec binds body ->
      let (binders, bodyEnv) = tidyEachBinder env (map bindBinder binds)
          binds' = zipWith (tidyRecBind bodyEnv) binders binds
       in rebuild2 ExRec binds (collect binds binds') body (tidyExpr bodyEnv body)
    ExCase scrutinee binder alternatives ->
      let (binder', caseEnv) = case binder of
            Nothing -> (Same, env)
            Just named
              | all (\alternative -> binderName named `Set.notMember` (exprFreeNames (altRhs alternative) `Set.difference` Set.fromList (map binderName (altBinders alternative)))) alternatives -> (Changed Nothing, env)
              | otherwise -> let (named', inner) = tidyBinder env named in (rebuild1 Just named named', inner)
       in rebuild3
            ExCase
            scrutinee
            (tidyExpr env scrutinee)
            binder
            binder'
            alternatives
            (tidyNonEmpty (tidyAlt caseEnv) alternatives)
    ExAbsurd scrutinee resultType -> rebuild2 ExAbsurd scrutinee (tidyExpr env scrutinee) resultType (tidyType env resultType)
    ExCoercion proof -> rebuild1 ExCoercion proof (tidyCoercion env proof)
    ExCast body coercion ->
      rebuild2 ExCast body (tidyExpr env body) coercion (tidyCoercion env coercion)

tidyRecBind :: TidyEnv -> Tidied Binder -> Bind -> Tidied Bind
tidyRecBind env binder bind =
  rebuild2 Bind (bindBinder bind) binder (bindRhs bind) (tidyExpr env (bindRhs bind))

tidyAlt :: TidyEnv -> Alt -> Tidied Alt
tidyAlt env alternative =
  let (typeBinders, typeEnv) = tidyBinders env (altTypeBinders alternative)
      (binders, rhsEnv) = tidyBinders typeEnv (altBinders alternative)
   in rebuild4
        Alt
        (altCon alternative)
        (tidyAltCon env (altCon alternative))
        (altTypeBinders alternative)
        typeBinders
        (altBinders alternative)
        binders
        (altRhs alternative)
        (tidyExpr rhsEnv (altRhs alternative))

tidyAltCon :: TidyEnv -> AltCon -> Tidied AltCon
tidyAltCon env alternative =
  case alternative of
    AltData name -> rebuild1 AltData name (tidyUse env name)
    AltLit literal -> rebuild1 AltLit literal (tidyLiteral env literal)
    AltDefault -> Same

tidyLiteral :: TidyEnv -> Literal -> Tidied Literal
tidyLiteral env literal =
  case literal of
    LitInt representation value -> rebuild1 (`LitInt` value) representation (tidyType env representation)
    LitChar representation value -> rebuild1 (`LitChar` value) representation (tidyType env representation)
    LitAddr representation value -> rebuild1 (`LitAddr` value) representation (tidyType env representation)

tidyCoercion :: TidyEnv -> Coercion -> Tidied Coercion
tidyCoercion env coercion =
  case coercion of
    CoVar name -> rebuild1 CoVar name (tidyUse env name)
    CoRefl ty -> rebuild1 CoRefl ty (tidyType env ty)
    CoSym inner -> rebuild1 CoSym inner (tidyCoercion env inner)
    CoTrans left right ->
      rebuild2 CoTrans left (tidyCoercion env left) right (tidyCoercion env right)
    CoApp function argument ->
      rebuild2 CoApp function (tidyCoercion env function) argument (tidyCoercion env argument)
    CoNth index proof -> rebuild1 (CoNth index) proof (tidyCoercion env proof)
    CoFun domain range ->
      rebuild2 CoFun domain (tidyCoercion env domain) range (tidyCoercion env range)
    CoForAll binder body ->
      let (binder', bodyEnv) = tidyBinder env binder
       in rebuild2 CoForAll binder binder' body (tidyCoercion bodyEnv body)
    CoTyConApp name arguments ->
      rebuild2 CoTyConApp name (tidyUse env name) arguments (tidyList (tidyCoercion env) arguments)
    CoAxiom name arguments ->
      rebuild2 CoAxiom name (tidyUse env name) arguments (tidyList (tidyType env) arguments)

tidyBinders :: TidyEnv -> [Binder] -> (Tidied [Binder], TidyEnv)
tidyBinders env binders =
  let (tidiedBinders, finalEnv) = tidyEachBinder env binders
   in (collect binders tidiedBinders, finalEnv)

tidyEachBinder :: TidyEnv -> [Binder] -> ([Tidied Binder], TidyEnv)
tidyEachBinder env binders =
  case binders of
    [] -> ([], env)
    binder : rest ->
      let (binder', nextEnv) = tidyBinder env binder
          (rest', finalEnv) = tidyEachBinder nextEnv rest
       in (binder' : rest', finalEnv)

tidyBinder :: TidyEnv -> Binder -> (Tidied Binder, TidyEnv)
tidyBinder env binder =
  let oldName = binderName binder
      newName = tidyBinderName env oldName
      name'
        | nameOrigin newName == nameOrigin oldName = Same
        | otherwise = Changed newName
      binder' = rebuild2 Binder oldName name' (binderType binder) (tidyType env (binderType binder))
   in (binder', bindName env oldName newName)

tidyBinderName :: TidyEnv -> Name -> Name
tidyBinderName env name =
  case nameOrigin name of
    OriginLocal {} -> name {nameOrigin = OriginLocal (Unique (lowestUnused used))}
      where
        used = Map.findWithDefault Set.empty (nameText name) (tidyUsed env)
    OriginTop {} -> name

bindName :: TidyEnv -> Name -> Name -> TidyEnv
bindName env oldName newName =
  case nameOrigin newName of
    OriginLocal (Unique unique) ->
      env
        { tidyNames = Map.insert oldName newName (tidyNames env),
          tidyUsed = Map.insertWith Set.union (nameText newName) (Set.singleton unique) (tidyUsed env)
        }
    OriginTop {} -> env

-- | A use of a name. The tidy changes only the unique of a local name, so
-- a use whose origin stays the same keeps its object.
tidyUse :: TidyEnv -> Name -> Tidied Name
tidyUse env name =
  case nameOrigin name of
    OriginLocal {} ->
      case Map.lookup name (tidyNames env) of
        Just new | nameOrigin new /= nameOrigin name -> Changed new
        _ -> Same
    OriginTop {} -> Same

lowestUnused :: Set Int -> Int
lowestUnused used = go 0
  where
    go candidate
      | candidate `Set.member` used = go (candidate + 1)
      | otherwise = candidate
