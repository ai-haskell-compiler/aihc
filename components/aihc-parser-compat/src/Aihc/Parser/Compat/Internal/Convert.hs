{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.Parser.Compat.Internal.Convert
  ( toGhcHsDecl,
    toGhcHsExpr,
    toGhcLHsDecl,
    toGhcLHsExpr,
  )
where

import Aihc.Parser.Syntax qualified as A
import Data.ByteString.Char8 qualified as BS
import Data.Char (GeneralCategory (..), generalCategory, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GHC.Builtin.Types (nilDataCon, tupleDataCon, tupleTyCon, unrestrictedFunTyConName)
import GHC.Data.FastString (mkFastString)
import GHC.Hs
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.Name.Occurrence (OccName, mkDataOcc, mkVarOcc)
import GHC.Types.Name.Reader (RdrName, getRdrName, mkRdrQual, mkRdrUnqual)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), noSrcSpan, unLoc)
import Language.Haskell.Syntax.Basic (FieldLabelString (..), LexicalFixity (..))
import Language.Haskell.Syntax.Basic qualified as Hs
import Language.Haskell.Syntax.Specificity (Specificity (..))

toGhcHsDecl :: A.Decl -> HsDecl GhcPs
toGhcHsDecl decl =
  case A.peelDeclAnn decl of
    A.DeclAnn _ inner -> toGhcHsDecl inner
    A.DeclValue value -> ValD noExtField (valueBind value)
    A.DeclTypeSig names ty -> SigD noExtField (typeSig names ty)
    A.DeclPatSyn patSyn -> ValD noExtField (patSynBind patSyn)
    A.DeclPatSynSig names ty -> SigD noExtField (PatSynSig noAnn (map (lA . toRdrName . A.qualifyName Nothing) names) (toSigType ty))
    A.DeclStandaloneKindSig name kind -> KindSigD noExtField (StandaloneKindSig noAnn (lA (toRdrName (A.qualifyName Nothing name))) (toSigType kind))
    A.DeclFixity assoc namespace prec ops -> SigD noExtField (FixSig noAnn (fixitySig assoc namespace prec ops))
    A.DeclRoleAnnotation ann -> RoleAnnotD noExtField (roleAnnotDecl ann)
    A.DeclTypeSyn syn -> TyClD noExtField (typeSynDecl syn)
    A.DeclTypeData dd -> TyClD noExtField (dataTyClDecl TypeData dd)
    A.DeclData dd -> TyClD noExtField (dataTyClDecl OrdinaryData dd)
    A.DeclNewtype nd -> TyClD noExtField (newtypeTyClDecl nd)
    A.DeclClass cd -> TyClD noExtField (classTyClDecl cd)
    A.DeclInstance inst -> InstD noExtField (ClsInstD noExtField (classInstDecl inst))
    A.DeclStandaloneDeriving sd -> DerivD noExtField (derivDecl sd)
    A.DeclDefault tys -> DefD noExtField (DefaultDecl (noAnn, noAnn, noAnn) Nothing (map toLHsType tys))
    A.DeclSplice expr -> SpliceD noExtField (spliceDecl expr)
    A.DeclForeign fd -> ForD noExtField (foreignDecl fd)
    A.DeclTypeFamilyDecl tf -> TyClD noExtField (FamDecl noExtField (typeFamilyDecl TopLevel tf))
    A.DeclDataFamilyDecl df -> TyClD noExtField (FamDecl noExtField (dataFamilyDecl TopLevel df))
    A.DeclTypeFamilyInst tfi -> InstD noExtField (TyFamInstD noExtField (typeFamilyInstDecl tfi))
    A.DeclDataFamilyInst dfi -> InstD noExtField (DataFamInstD noExtField (dataFamilyInstDecl dfi))
    A.DeclPragma pragma -> SigD noExtField (pragmaSig pragma)

toGhcLHsDecl :: A.Decl -> LHsDecl GhcPs
toGhcLHsDecl = lA . toGhcHsDecl

data DataFlavor = OrdinaryData | TypeData

typeSig :: [A.BinderName] -> A.Type -> Sig GhcPs
typeSig names ty =
  TypeSig noAnn (map (lA . toRdrName . A.qualifyName Nothing) names) (toSigWcType ty)

toSigType :: A.Type -> LHsSigType GhcPs
toSigType ty =
  case A.peelTypeAnn ty of
    A.TAnn _ inner -> toSigType inner
    A.TForall (A.ForallTelescope A.ForallInvisible binders) body ->
      lA (HsSig noExtField (HsOuterExplicit noAnn (map toInvisibleTyVarBndr binders)) (toLHsType body))
    _ -> lA (HsSig noExtField (HsOuterImplicit noExtField) (toLHsType ty))

fixitySig :: A.FixityAssoc -> Maybe A.IEEntityNamespace -> Maybe Int -> [A.OperatorName] -> FixitySig GhcPs
fixitySig assoc namespace prec ops =
  FixitySig (namespaceSpecifier namespace) (map (lA . toRdrName . A.qualifyName Nothing) ops) (Hs.Fixity (maybe 9 fromIntegral prec) (fixityDirection assoc))

namespaceSpecifier :: Maybe A.IEEntityNamespace -> NamespaceSpecifier
namespaceSpecifier namespace =
  case namespace of
    Nothing -> NoNamespaceSpecifier
    Just A.IEEntityNamespaceType -> TypeNamespaceSpecifier noAnn
    Just A.IEEntityNamespaceData -> DataNamespaceSpecifier noAnn
    Just A.IEEntityNamespacePattern -> NoNamespaceSpecifier

fixityDirection :: A.FixityAssoc -> Hs.FixityDirection
fixityDirection assoc =
  case assoc of
    A.Infix -> Hs.InfixN
    A.InfixL -> Hs.InfixL
    A.InfixR -> Hs.InfixR

roleAnnotDecl :: A.RoleAnnotation -> RoleAnnotDecl GhcPs
roleAnnotDecl ann =
  RoleAnnotDecl noAnn (lA (toRdrName (A.qualifyName Nothing (A.roleAnnotationName ann)))) (map (lA . role) (A.roleAnnotationRoles ann))

role :: A.Role -> Maybe Hs.Role
role role' =
  case role' of
    A.RoleNominal -> Just Hs.Nominal
    A.RoleRepresentational -> Just Hs.Representational
    A.RolePhantom -> Just Hs.Phantom
    A.RoleInfer -> Nothing

typeSynDecl :: A.TypeSynDecl -> TyClDecl GhcPs
typeSynDecl syn =
  let (name, params, fixity) = binderHeadParts (A.typeSynHead syn)
   in SynDecl noAnn (lA (toRdrName (A.qualifyName Nothing name))) (qTyVars params) fixity (toLHsType (A.typeSynBody syn))

dataTyClDecl :: DataFlavor -> A.DataDecl -> TyClDecl GhcPs
dataTyClDecl flavor dd =
  let (name, params, fixity) = binderHeadParts (A.dataDeclHead dd)
   in DataDecl
        noExtField
        (lA (toRdrName (A.qualifyName Nothing name)))
        (qTyVars params)
        fixity
        ( hsDataDefn
            flavor
            (A.dataDeclContext dd)
            (A.dataDeclKind dd)
            (DataTypeCons (isTypeData flavor) (map toLConDecl (A.dataDeclConstructors dd)))
            (A.dataDeclDeriving dd)
        )

newtypeTyClDecl :: A.NewtypeDecl -> TyClDecl GhcPs
newtypeTyClDecl nd =
  let (name, params, fixity) = binderHeadParts (A.newtypeDeclHead nd)
      con =
        case A.newtypeDeclConstructor nd of
          Just ctor -> toLConDecl ctor
          Nothing -> lA (ConDeclH98 noAnn (lA (mkRdrUnqual (mkDataOcc "MissingNewtypeConstructor"))) False [] Nothing (PrefixCon []) Nothing)
   in DataDecl
        noExtField
        (lA (toRdrName (A.qualifyName Nothing name)))
        (qTyVars params)
        fixity
        (hsDataDefn OrdinaryData (A.newtypeDeclContext nd) (A.newtypeDeclKind nd) (NewTypeCon con) (A.newtypeDeclDeriving nd))

hsDataDefn :: DataFlavor -> [A.Type] -> Maybe A.Type -> DataDefnCons (LConDecl GhcPs) -> [A.DerivingClause] -> HsDataDefn GhcPs
hsDataDefn flavor context kind cons derivingClauses =
  HsDataDefn noAnn (toMaybeContext context) Nothing (toLHsType <$> kind) cons (map derivingClause derivingClauses)
  where
    _ = flavor

isTypeData :: DataFlavor -> Bool
isTypeData flavor =
  case flavor of
    OrdinaryData -> False
    TypeData -> True

toLConDecl :: A.DataConDecl -> LConDecl GhcPs
toLConDecl = lA . toConDecl

toConDecl :: A.DataConDecl -> ConDecl GhcPs
toConDecl ctor =
  case A.peelDataConAnn ctor of
    A.DataConAnn _ inner -> toConDecl inner
    A.PrefixCon foralls context name fields ->
      ConDeclH98 noAnn (lA (toRdrName (A.qualifyName Nothing name))) (not (null foralls)) (map toInvisibleTyVarBndr foralls) (toMaybeContext context) (PrefixCon (map conField fields)) Nothing
    A.InfixCon foralls context lhs name rhs ->
      ConDeclH98 noAnn (lA (toRdrName (A.qualifyName Nothing name))) (not (null foralls)) (map toInvisibleTyVarBndr foralls) (toMaybeContext context) (InfixCon (conField lhs) (conField rhs)) Nothing
    A.RecordCon foralls context name fields ->
      ConDeclH98 noAnn (lA (toRdrName (A.qualifyName Nothing name))) (not (null foralls)) (map toInvisibleTyVarBndr foralls) (toMaybeContext context) (RecCon (lA (map fieldDecl fields))) Nothing
    A.GadtCon foralls context names body ->
      let (details, resultTy) = gadtDetailsAndResult body
       in ConDeclGADT noAnn (nonEmpty (map (lA . toRdrName . A.qualifyName Nothing) names)) (lA (HsOuterImplicit noExtField)) (map forallTele foralls) (toMaybeContext context) details (toGadtResultType details resultTy) Nothing
    A.TupleCon foralls context flavor fields ->
      ConDeclH98 noAnn (lA (getRdrName (tupleDataCon (boxity flavor) (length fields)))) (not (null foralls)) (map toInvisibleTyVarBndr foralls) (toMaybeContext context) (PrefixCon (map conField fields)) Nothing
    A.UnboxedSumCon foralls context _pos _arity field ->
      ConDeclH98 noAnn (lA (mkRdrUnqual (mkDataOcc "UnboxedSumCon"))) (not (null foralls)) (map toInvisibleTyVarBndr foralls) (toMaybeContext context) (PrefixCon [conField field]) Nothing
    A.ListCon foralls context ->
      ConDeclH98 noAnn (lA (getRdrName nilDataCon)) (not (null foralls)) (map toInvisibleTyVarBndr foralls) (toMaybeContext context) (PrefixCon []) Nothing

conField :: A.BangType -> HsConDeclField GhcPs
conField bt =
  let (fieldUnpackedness, fieldStrictness, fieldType) = constructorFieldBang bt
   in CDF conFieldExt fieldUnpackedness fieldStrictness (HsUnannotated EpPatBind) fieldType Nothing

gadtConField :: (A.BangType, A.ArrowKind) -> HsConDeclField GhcPs
gadtConField (bt, arrow) =
  let (fieldUnpackedness, fieldStrictness, fieldType) = constructorFieldBang bt
   in CDF conFieldExt fieldUnpackedness fieldStrictness (multAnn arrow) fieldType Nothing

constructorFieldBang :: A.BangType -> (SrcUnpackedness, SrcStrictness, LHsType GhcPs)
constructorFieldBang bt
  | NoSrcStrict <- strictness bt =
      (unpackedness bt, NoSrcStrict, toLHsType (A.bangType bt))
  | A.TFun arrow lhs rhs <- A.peelTypeAnn (A.bangType bt) =
      (NoSrcUnpack, NoSrcStrict, lA (HsFunTy noExtField (multAnn arrow) (toLeadingBangedLHsType bt lhs) (toLHsType rhs)))
  | A.TApp fun arg <- A.peelTypeAnn (A.bangType bt) =
      (NoSrcUnpack, NoSrcStrict, lA (HsAppTy noExtField (toLeadingBangedLHsType bt fun) (toLHsType arg)))
  | A.TTypeApp fun arg <- A.peelTypeAnn (A.bangType bt) =
      (NoSrcUnpack, NoSrcStrict, lA (HsAppKindTy noAnn (toLeadingBangedLHsType bt fun) (toLHsKindAppArg arg)))
  | A.TInfix lhs op promotion rhs <- A.peelTypeAnn (A.bangType bt) =
      (NoSrcUnpack, NoSrcStrict, lA (HsOpTy noExtField (promotionFlag promotion) (toLeadingBangedLHsType bt lhs) (lA (toRdrName op)) (toLHsType rhs)))
  | A.TContext context@(_ : _) body <- A.peelTypeAnn (A.bangType bt) =
      (NoSrcUnpack, NoSrcStrict, lA (HsQualTy noExtField (lA [toLeadingBangedLHsType bt (contextBangTarget context)]) (toLHsType body)))
  | otherwise =
      (unpackedness bt, strictness bt, toLHsType (A.bangType bt))

toLeadingBangedLHsType :: A.BangType -> A.Type -> LHsType GhcPs
toLeadingBangedLHsType bt ty =
  case A.peelTypeAnn ty of
    A.TApp fun arg -> lA (HsAppTy noExtField (toLeadingBangedLHsType bt fun) (toLHsType arg))
    A.TTypeApp fun arg -> lA (HsAppKindTy noAnn (toLeadingBangedLHsType bt fun) (toLHsKindAppArg arg))
    A.TInfix lhs op promotion rhs -> lA (HsOpTy noExtField (promotionFlag promotion) (toLeadingBangedLHsType bt lhs) (lA (toRdrName op)) (toLHsType rhs))
    _ -> toBangedLHsType bt ty

toBangedLHsType :: A.BangType -> A.Type -> LHsType GhcPs
toBangedLHsType bt ty =
  lA (XHsType (HsBangTy noAnn (HsSrcBang NoSourceText (unpackedness bt) (strictness bt)) (toLHsType ty)))

contextBangTarget :: [A.Type] -> A.Type
contextBangTarget [ty] = ty
contextBangTarget tys = A.TTuple A.Boxed A.Unpromoted tys

conFieldExt :: XConDeclField GhcPs
conFieldExt = ((EpaSpan noSrcSpan, noAnn, EpaSpan noSrcSpan), NoSourceText)

fieldDecl :: A.FieldDecl -> LHsConDeclRecField GhcPs
fieldDecl fd =
  lA (HsConDeclRecField noExtField (map (lA . fieldOcc . A.qualifyName Nothing) (A.fieldNames fd)) spec)
  where
    spec =
      (conField (A.fieldType fd))
        { cdf_multiplicity = maybe (HsUnannotated EpPatBind) (HsExplicitMult (noAnn, EpPatBind) . toLHsType) (A.fieldMultiplicity fd)
        }

gadtDetailsAndResult :: A.GadtBody -> (HsConDeclGADTDetails GhcPs, A.Type)
gadtDetailsAndResult body =
  case body of
    A.GadtPrefixBody args resultTy ->
      let (extraArgs, finalResult) = splitGadtResultType resultTy
       in (PrefixConGADT noExtField (map gadtConField (args <> extraArgs)), finalResult)
    A.GadtRecordBody fields resultTy -> (RecConGADT noAnn (lA (map fieldDecl fields)), resultTy)

splitGadtResultType :: A.Type -> ([(A.BangType, A.ArrowKind)], A.Type)
splitGadtResultType ty =
  case A.peelTypeAnn ty of
    A.TParen inner -> splitGadtResultType inner
    A.TFun arrow lhs rhs ->
      let (args, resultTy) = splitGadtResultType rhs
       in ((A.BangType [] [] False False lhs, arrow) : args, resultTy)
    _ -> ([], ty)

toGadtResultType :: HsConDeclGADTDetails GhcPs -> A.Type -> LHsType GhcPs
toGadtResultType details ty =
  case details of
    PrefixConGADT {} ->
      case A.peelTypeAnn ty of
        A.TParen inner
          | isStarType inner ->
              lA (HsStarTy noExtField False)
        _ -> toLHsType ty
    RecConGADT {} -> toLHsType ty

isStarType :: A.Type -> Bool
isStarType ty =
  case A.peelTypeAnn ty of
    A.TStar -> True
    _ -> False

strictness :: A.BangType -> SrcStrictness
strictness bt
  | A.bangStrict bt = SrcStrict
  | A.bangLazy bt = SrcLazy
  | otherwise = NoSrcStrict

unpackedness :: A.BangType -> SrcUnpackedness
unpackedness bt =
  case [kind | pragma <- A.bangPragmas bt, A.PragmaUnpack kind <- [A.pragmaType pragma]] of
    A.UnpackPragma : _ -> SrcUnpack
    A.NoUnpackPragma : _ -> SrcNoUnpack
    [] -> NoSrcUnpack

classTyClDecl :: A.ClassDecl -> TyClDecl GhcPs
classTyClDecl cd =
  let (name, params, fixity) = binderHeadParts (A.classDeclHead cd)
      (sigs, binds, ats, atDefs) = classItems (A.classDeclItems cd)
   in ClassDecl (noAnn, EpNoLayout, NoAnnSortKey) (toContext <$> A.classDeclContext cd) (lA (toRdrName (A.qualifyName Nothing name))) (qTyVars params) fixity (map funDep (A.classDeclFundeps cd)) sigs binds ats atDefs []

classItems :: [A.ClassDeclItem] -> ([LSig GhcPs], LHsBinds GhcPs, [LFamilyDecl GhcPs], [LTyFamDefltDecl GhcPs])
classItems = foldMap classItem

classItem :: A.ClassDeclItem -> ([LSig GhcPs], LHsBinds GhcPs, [LFamilyDecl GhcPs], [LTyFamDefltDecl GhcPs])
classItem item =
  case A.peelClassDeclItemAnn item of
    A.ClassItemAnn _ inner -> classItem inner
    A.ClassItemTypeSig names ty -> ([lA (ClassOpSig noAnn False (map (lA . toRdrName . A.qualifyName Nothing) names) (toSigType ty))], [], [], [])
    A.ClassItemDefaultSig name ty -> ([lA (ClassOpSig noAnn True [lA (toRdrName (A.qualifyName Nothing name))] (toSigType ty))], [], [], [])
    A.ClassItemFixity assoc namespace prec ops -> ([lA (FixSig noAnn (fixitySig assoc namespace prec ops))], [], [], [])
    A.ClassItemDefault value -> ([], [lA (valueBind value)], [], [])
    A.ClassItemTypeFamilyDecl tf -> ([], [], [lA (typeFamilyDecl NotTopLevel tf)], [])
    A.ClassItemDataFamilyDecl df -> ([], [], [lA (dataFamilyDecl NotTopLevel df)], [])
    A.ClassItemDefaultTypeInst tfi -> ([], [], [], [lA (typeFamilyInstDecl tfi)])
    A.ClassItemPragma pragma -> ([lA (pragmaSig pragma)], [], [], [])

classInstDecl :: A.InstanceDecl -> ClsInstDecl GhcPs
classInstDecl inst =
  let (binds, sigs, tyFamInsts, dataFamInsts) = instanceItems (A.instanceDeclItems inst)
   in ClsInstDecl
        (Nothing, noAnn, NoAnnSortKey)
        (instanceSigType (A.instanceDeclForall inst) (A.instanceDeclContext inst) (A.instanceDeclHead inst))
        binds
        sigs
        tyFamInsts
        dataFamInsts
        Nothing

instanceItems :: [A.InstanceDeclItem] -> (LHsBinds GhcPs, [LSig GhcPs], [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs])
instanceItems = foldMap instanceItem

instanceItem :: A.InstanceDeclItem -> (LHsBinds GhcPs, [LSig GhcPs], [LTyFamInstDecl GhcPs], [LDataFamInstDecl GhcPs])
instanceItem item =
  case A.peelInstanceDeclItemAnn item of
    A.InstanceItemAnn _ inner -> instanceItem inner
    A.InstanceItemBind value -> ([lA (valueBind value)], [], [], [])
    A.InstanceItemTypeSig names ty -> ([], [lA (typeSig names ty)], [], [])
    A.InstanceItemFixity assoc namespace prec ops -> ([], [lA (FixSig noAnn (fixitySig assoc namespace prec ops))], [], [])
    A.InstanceItemTypeFamilyInst tfi -> ([], [], [lA (typeFamilyInstDecl tfi)], [])
    A.InstanceItemDataFamilyInst dfi -> ([], [], [], [lA (dataFamilyInstDecl dfi)])
    A.InstanceItemPragma pragma -> ([], [lA (pragmaSig pragma)], [], [])

instanceSigType :: [A.TyVarBinder] -> [A.Type] -> A.Type -> LHsSigType GhcPs
instanceSigType binders context headTy =
  lA (HsSig noExtField outer (toLHsType (applyContext context headTy)))
  where
    outer =
      case binders of
        [] -> HsOuterImplicit noExtField
        _ -> HsOuterExplicit noAnn (map toInvisibleTyVarBndr binders)

derivDecl :: A.StandaloneDerivingDecl -> DerivDecl GhcPs
derivDecl sd =
  DerivDecl (Nothing, (noAnn, noAnn)) (HsWC noExtField (instanceSigType (A.standaloneDerivingForall sd) (A.standaloneDerivingContext sd) (A.standaloneDerivingHead sd))) (toLDerivStrategy <$> A.standaloneDerivingStrategy sd) Nothing

typeFamilyDecl :: TopLevelFlag -> A.TypeFamilyDecl -> FamilyDecl GhcPs
typeFamilyDecl topLevel tf =
  let (name, params, fixity) = typeFamilyHeadParts (A.typeFamilyDeclHeadForm tf) (A.typeFamilyDeclHead tf) (A.typeFamilyDeclParams tf)
      (resultSig, injectivity) = familyResult (A.typeFamilyDeclResultSig tf)
      info = maybe OpenTypeFamily (ClosedTypeFamily . Just . map (lA . typeFamilyEqn)) (A.typeFamilyDeclEquations tf)
   in FamilyDecl noAnn info topLevel (lA (toRdrName name)) (qTyVars params) fixity (lA resultSig) injectivity

dataFamilyDecl :: TopLevelFlag -> A.DataFamilyDecl -> FamilyDecl GhcPs
dataFamilyDecl topLevel df =
  let (name, params, fixity) = binderHeadParts (A.dataFamilyDeclHead df)
   in FamilyDecl noAnn DataFamily topLevel (lA (toRdrName (A.qualifyName Nothing name))) (qTyVars params) fixity (lA (maybe (NoSig noExtField) (KindSig noExtField . toLHsType) (A.dataFamilyDeclKind df))) Nothing

typeFamilyInstDecl :: A.TypeFamilyInst -> TyFamInstDecl GhcPs
typeFamilyInstDecl inst =
  TyFamInstDecl (noAnn, noAnn) (typeFamilyInstEqn inst)

typeFamilyInstEqn :: A.TypeFamilyInst -> TyFamInstEqn GhcPs
typeFamilyInstEqn inst =
  let (name, args, fixity) = familyLhsParts (A.typeFamilyInstHeadForm inst) (A.typeFamilyInstLhs inst)
   in FamEqn ([], [], noAnn) (lA (toRdrName name)) (outerFamBndrs (A.typeFamilyInstForall inst)) (map typeArg args) fixity (toLHsType (A.typeFamilyInstRhs inst))

typeFamilyEqn :: A.TypeFamilyEq -> TyFamInstEqn GhcPs
typeFamilyEqn eq =
  let (name, args, fixity) = familyLhsParts (A.typeFamilyEqHeadForm eq) (A.typeFamilyEqLhs eq)
   in FamEqn ([], [], noAnn) (lA (toRdrName name)) (outerFamBndrs (A.typeFamilyEqForall eq)) (map typeArg args) fixity (toLHsType (A.typeFamilyEqRhs eq))

dataFamilyInstDecl :: A.DataFamilyInst -> DataFamInstDecl GhcPs
dataFamilyInstDecl inst =
  DataFamInstDecl $
    FamEqn
      ([], [], noAnn)
      (lA (toRdrName name))
      (outerFamBndrs (A.dataFamilyInstForall inst))
      (map typeArg args)
      fixity
      ( hsDataDefn
          OrdinaryData
          []
          (A.dataFamilyInstKind inst)
          cons
          (A.dataFamilyInstDeriving inst)
      )
  where
    (name, args, fixity) = familyLhsParts (inferTypeHeadForm (A.dataFamilyInstHead inst)) (A.dataFamilyInstHead inst)
    cons
      | A.dataFamilyInstIsNewtype inst,
        con : _ <- A.dataFamilyInstConstructors inst =
          NewTypeCon (toLConDecl con)
      | otherwise =
          DataTypeCons False (map toLConDecl (A.dataFamilyInstConstructors inst))

foreignDecl :: A.ForeignDecl -> ForeignDecl GhcPs
foreignDecl fd =
  case A.foreignDirection fd of
    A.ForeignImport -> ForeignImport (noAnn, noAnn, noAnn) name sig (CImport (lA NoSourceText) (lA cc) (lA safety) header importSpec)
    A.ForeignExport -> ForeignExport (noAnn, noAnn, noAnn) name sig (CExport (lA NoSourceText) (lA (CExportStatic NoSourceText exportLabel cc)))
  where
    name = lA (toRdrName (A.qualifyName Nothing (A.foreignName fd)))
    sig = toSigType (A.foreignType fd)
    cc = callConv (A.foreignCallConv fd)
    safety = maybe PlaySafe foreignSafety (A.foreignSafety fd)
    (header, importSpec) = foreignImportSpec (A.foreignEntity fd)
    exportLabel = mkFastString (T.unpack (A.unqualifiedNameText (A.foreignName fd)))

spliceDecl :: A.Expr -> SpliceDecl GhcPs
spliceDecl expr =
  SpliceDecl noExtField (lA (HsUntypedSpliceExpr noAnn (declSpliceExpr expr))) (declSpliceDecoration expr)

declSpliceDecoration :: A.Expr -> SpliceDecoration
declSpliceDecoration expr =
  case A.peelExprAnn expr of
    A.ETHSplice {} -> DollarSplice
    A.ETHTypedSplice {} -> DollarSplice
    _ -> BareSplice

declSpliceExpr :: A.Expr -> LHsExpr GhcPs
declSpliceExpr expr =
  case A.peelExprAnn expr of
    A.ETHSplice body -> toGhcLHsExpr body
    A.ETHTypedSplice body -> toGhcLHsExpr body
    _ -> toGhcLHsExpr expr

pragmaSig :: A.Pragma -> Sig GhcPs
pragmaSig pragma =
  case A.pragmaType pragma of
    A.PragmaInline kind target ->
      InlineSig noAnn (lA (toRdrName (A.nameFromText target))) (inlinePragma kind)
    _ ->
      InlineSig noAnn (lA (mkRdrUnqual (mkVarOcc "unsupportedPragma"))) (inlinePragma "INLINE")

inlinePragma :: T.Text -> InlinePragma
inlinePragma kind =
  InlinePragma NoSourceText inlineSpec Nothing activation FunLike
  where
    upper = T.toUpper kind
    inlineSpec
      | upper == "NOINLINE" = NoInline NoSourceText
      | upper == "INLINABLE" || upper == "INLINEABLE" = Inlinable NoSourceText
      | otherwise = Inline NoSourceText
    activation
      | upper == "NOINLINE" = NeverActive
      | otherwise = AlwaysActive

patSynBind :: A.PatSynDecl -> HsBind GhcPs
patSynBind ps =
  PatSynBind noExtField $
    PSB
      noAnn
      (lA (toRdrName (A.qualifyName Nothing (A.patSynDeclName ps))))
      (patSynDetails (A.patSynDeclArgs ps))
      (toLPat (A.patSynDeclPat ps))
      (patSynDir (A.patSynDeclDir ps))

patSynDetails :: A.PatSynArgs -> HsPatSynDetails GhcPs
patSynDetails args =
  case args of
    A.PatSynPrefixArgs names -> PrefixCon (map (lA . mkRdrUnqual . mkVarOcc . T.unpack) names)
    A.PatSynInfixArgs lhs rhs -> InfixCon (lA (mkRdrUnqual (mkVarOcc (T.unpack lhs)))) (lA (mkRdrUnqual (mkVarOcc (T.unpack rhs))))
    A.PatSynRecordArgs fields -> RecCon [RecordPatSynField (fieldOcc (A.qualifyName Nothing (A.mkUnqualifiedName A.NameVarId field))) (lA (mkRdrUnqual (mkVarOcc (T.unpack field)))) | field <- fields]

patSynDir :: A.PatSynDir -> HsPatSynDir GhcPs
patSynDir dir =
  case dir of
    A.PatSynUnidirectional -> Unidirectional
    A.PatSynBidirectional -> ImplicitBidirectional
    A.PatSynExplicitBidirectional matches ->
      ExplicitBidirectional (matchGroup (FunRhs (lA (mkRdrUnqual (mkVarOcc "pattern"))) Prefix NoSrcStrict noAnn) (map (functionMatch (A.mkUnqualifiedName A.NameVarId "pattern")) matches))

derivingClause :: A.DerivingClause -> LHsDerivingClause GhcPs
derivingClause clause =
  lA (HsDerivingClause noAnn (toLDerivStrategy <$> A.derivingStrategy clause) (lA (derivingClauseTys (A.derivingClasses clause))))

derivingClauseTys :: Either A.Name [A.Type] -> DerivClauseTys GhcPs
derivingClauseTys classes =
  case classes of
    Left name -> DctSingle noExtField (toSigType (A.TCon name A.Unpromoted))
    Right tys -> DctMulti noExtField (map toSigType tys)

toLDerivStrategy :: A.DerivingStrategy -> LDerivStrategy GhcPs
toLDerivStrategy strategy =
  lA $
    case strategy of
      A.DerivingStock -> StockStrategy noAnn
      A.DerivingNewtype -> NewtypeStrategy noAnn
      A.DerivingAnyclass -> AnyclassStrategy noAnn
      A.DerivingVia ty -> ViaStrategy (XViaStrategyPs noAnn (toSigType ty))

funDep :: A.FunctionalDependency -> LHsFunDep GhcPs
funDep dep =
  lA (FunDep noAnn (map (lA . mkRdrUnqual . mkVarOcc . T.unpack) (A.functionalDependencyDeterminers dep)) (map (lA . mkRdrUnqual . mkVarOcc . T.unpack) (A.functionalDependencyDetermined dep)))

qTyVars :: [A.TyVarBinder] -> LHsQTyVars GhcPs
qTyVars params =
  HsQTvs noExtField (map toQTyVarBndr params)

toQTyVarBndr :: A.TyVarBinder -> LHsTyVarBndr (HsBndrVis GhcPs) GhcPs
toQTyVarBndr binder =
  toTyVarBndr (bndrVis binder) binder

bndrVis :: A.TyVarBinder -> HsBndrVis GhcPs
bndrVis binder =
  case A.tyVarBinderVisibility binder of
    A.TyVarBVisible -> HsBndrRequired noExtField
    A.TyVarBInvisible -> HsBndrInvisible noAnn

binderHeadParts :: A.BinderHead A.UnqualifiedName -> (A.UnqualifiedName, [A.TyVarBinder], LexicalFixity)
binderHeadParts head' =
  (A.binderHeadName head', A.binderHeadParams head', binderHeadFixity head')

binderHeadFixity :: A.BinderHead name -> LexicalFixity
binderHeadFixity head' =
  case A.binderHeadForm head' of
    A.TypeHeadPrefix -> Prefix
    A.TypeHeadInfix -> Infix

typeFamilyHeadParts :: A.TypeHeadForm -> A.Type -> [A.TyVarBinder] -> (A.Name, [A.TyVarBinder], LexicalFixity)
typeFamilyHeadParts form headTy params =
  case form of
    A.TypeHeadPrefix -> (typeHeadName headTy, params, Prefix)
    A.TypeHeadInfix -> (typeHeadName headTy, params, Infix)

typeHeadName :: A.Type -> A.Name
typeHeadName ty =
  case A.peelTypeHead ty of
    A.TCon name _ -> name
    A.TInfix _ name _ _ -> name
    _ -> A.qualifyName Nothing (A.mkUnqualifiedName A.NameConId "UnsupportedFamily")

familyLhsParts :: A.TypeHeadForm -> A.Type -> (A.Name, [A.Type], LexicalFixity)
familyLhsParts form lhs =
  case form of
    A.TypeHeadInfix ->
      case A.peelTypeHead lhs of
        A.TInfix left name _ right -> (name, [left, right], Infix)
        _ -> prefixFamilyLhs lhs
    A.TypeHeadPrefix -> prefixFamilyLhs lhs

prefixFamilyLhs :: A.Type -> (A.Name, [A.Type], LexicalFixity)
prefixFamilyLhs lhs =
  let (fun, args) = typeAppSpine lhs
   in (typeHeadName fun, args, Prefix)

inferTypeHeadForm :: A.Type -> A.TypeHeadForm
inferTypeHeadForm ty =
  case A.peelTypeHead ty of
    A.TInfix {} -> A.TypeHeadInfix
    _ -> A.TypeHeadPrefix

typeAppSpine :: A.Type -> (A.Type, [A.Type])
typeAppSpine ty =
  go ty []
  where
    go current args =
      case A.peelTypeAnn current of
        A.TApp fun arg -> go fun (arg : args)
        A.TParen inner -> go inner args
        other -> (other, args)

typeArg :: A.Type -> LHsTypeArg GhcPs
typeArg = HsValArg noExtField . toLHsType

outerFamBndrs :: [A.TyVarBinder] -> HsOuterFamEqnTyVarBndrs GhcPs
outerFamBndrs binders =
  case binders of
    [] -> HsOuterImplicit noExtField
    _ -> HsOuterExplicit noAnn (map (toTyVarBndr ()) binders)

familyResult :: Maybe A.TypeFamilyResultSig -> (FamilyResultSig GhcPs, Maybe (LInjectivityAnn GhcPs))
familyResult result =
  case result of
    Nothing -> (NoSig noExtField, Nothing)
    Just (A.TypeFamilyKindSig kind) -> (KindSig noExtField (toLHsType kind), Nothing)
    Just (A.TypeFamilyTyVarSig binder) -> (TyVarSig noExtField (toTyVarBndr () binder), Nothing)
    Just (A.TypeFamilyInjectiveSig binder injectivity) ->
      ( TyVarSig noExtField (toTyVarBndr () binder),
        Just (lA (InjectivityAnn noAnn (lA (mkRdrUnqual (mkVarOcc (T.unpack (A.typeFamilyInjectivityResult injectivity))))) (map (lA . mkRdrUnqual . mkVarOcc . T.unpack) (A.typeFamilyInjectivityDetermined injectivity))))
      )

toMaybeContext :: [A.Type] -> Maybe (LHsContext GhcPs)
toMaybeContext [] = Nothing
toMaybeContext context = Just (toContext context)

toContext :: [A.Type] -> LHsContext GhcPs
toContext context = lA (map toLHsType context)

applyContext :: [A.Type] -> A.Type -> A.Type
applyContext [] ty = ty
applyContext context ty = A.TContext context ty

callConv :: A.CallConv -> CCallConv
callConv cc =
  case cc of
    A.CCall -> CCallConv
    A.StdCall -> StdCallConv
    A.CApi -> CApiConv
    A.CPrim -> PrimCallConv

foreignSafety :: A.ForeignSafety -> Safety
foreignSafety safety =
  case safety of
    A.Safe -> PlaySafe
    A.Unsafe -> PlayRisky
    A.Interruptible -> PlayInterruptible

foreignImportSpec :: A.ForeignEntitySpec -> (Maybe Header, CImportSpec)
foreignImportSpec spec =
  case spec of
    A.ForeignEntityDynamic -> (Nothing, CFunction DynamicTarget)
    A.ForeignEntityWrapper -> (Nothing, CWrapper)
    A.ForeignEntityStatic name -> (Nothing, CFunction (staticTarget (fromMaybe "static" name)))
    A.ForeignEntityAddress name -> (Nothing, CLabel (mkFastString (T.unpack (fromMaybe "&" name))))
    A.ForeignEntityNamed name -> namedImportSpec name
    A.ForeignEntityOmitted -> (Nothing, CFunction (staticTarget ""))

namedImportSpec :: T.Text -> (Maybe Header, CImportSpec)
namedImportSpec name =
  case T.words name of
    [headerName, symbol]
      | ".h" `T.isSuffixOf` headerName ->
          (Just (Header NoSourceText (mkFastString (T.unpack headerName))), CFunction (staticTarget symbol))
    _ ->
      (Nothing, CFunction (staticTarget name))

staticTarget :: T.Text -> CCallTarget
staticTarget name =
  StaticTarget NoSourceText (mkFastString (T.unpack name)) Nothing True

toGhcHsExpr :: A.Expr -> HsExpr GhcPs
toGhcHsExpr expr =
  case A.peelExprAnn expr of
    A.EAnn _ inner -> toGhcHsExpr inner
    A.EVar name -> HsVar noExtField (lA (toRdrName name))
    A.ETypeSyntax _ ty -> HsEmbTy noAnn (toWcType ty)
    A.EInt value numeric raw -> integerExpr value numeric raw
    A.EFloat value floatTy raw -> floatExpr value floatTy raw
    A.EChar value _ -> HsLit noExtField (HsChar NoSourceText value)
    A.ECharHash value _ -> HsLit noExtField (HsCharPrim NoSourceText value)
    A.EString value _ -> HsLit noExtField (HsString NoSourceText (mkFastString (T.unpack value)))
    A.EStringHash value _ -> HsLit noExtField (HsStringPrim NoSourceText (BS.pack (T.unpack value)))
    A.EOverloadedLabel value _ -> HsOverLabel NoSourceText (mkFastString (T.unpack value))
    A.EQuasiQuote quoter body -> HsUntypedSplice noExtField (HsQuasiQuote noExtField (lA (toRdrName (A.nameFromText quoter))) (lA (mkFastString (T.unpack body))))
    A.EIf cond yes no -> HsIf noAnn (toGhcLHsExpr cond) (toGhcLHsExpr yes) (toGhcLHsExpr no)
    A.EMultiWayIf rhss -> HsMultiIf noAnn (nonEmpty (map toGRHS rhss))
    A.ELambdaPats pats body -> HsLam noAnn LamSingle (matchGroup (LamAlt LamSingle) [match (LamAlt LamSingle) pats (A.UnguardedRhs [] body Nothing)])
    A.ELambdaCase alts -> HsLam noAnn LamCase (matchGroup (LamAlt LamCase) (map (caseAltMatchWith (LamAlt LamCase)) alts))
    A.ELambdaCases alts -> HsLam noAnn LamCases (matchGroup (LamAlt LamCases) (map (lambdaCasesAltMatchWith (LamAlt LamCases)) alts))
    A.EInfix lhs op rhs -> OpApp noExtField (toGhcLHsExpr lhs) (operatorExpr op) (toGhcLHsExpr rhs)
    A.ENegate inner -> NegApp noAnn (toGhcLHsExpr inner) noSyntaxExpr
    A.ESectionL lhs op -> SectionL noExtField (toGhcLHsExpr lhs) (operatorExpr op)
    A.ESectionR op rhs -> SectionR noExtField (operatorExpr op) (toGhcLHsExpr rhs)
    A.ELetDecls decls body -> HsLet noAnn (localBinds decls) (toGhcLHsExpr body)
    A.ECase scrut alts -> HsCase noAnn (toGhcLHsExpr scrut) (matchGroup CaseAlt (map caseAltMatch alts))
    A.EDo stmts flavor -> HsDo noAnn (doFlavor flavor) (lA (doStmts stmts))
    A.EListComp body stmts -> HsDo noAnn ListComp (lA (compStmts body stmts))
    A.EListCompParallel body stmtss -> HsDo noAnn ListComp (lA (parCompStmts body stmtss))
    A.EArithSeq seq' -> ArithSeq noAnn Nothing (arithSeqInfo seq')
    A.ERecordCon con fields wildcard -> RecordCon recordAnn (lA (toRdrName con)) (recordBinds fields wildcard)
    A.ERecordUpd target fields | Just con <- recordConTarget target -> RecordCon recordAnn (lA con) (recordBinds fields False)
    A.ERecordUpd target fields -> RecordUpd recordAnn (toGhcLHsExpr target) (RegularRecUpdFields noExtField (map recordUpdField fields))
    A.EGetField base field -> HsGetField noExtField (toGhcLHsExpr base) (lA (dotField field))
    A.EGetFieldProjection fields -> HsProjection noAnn (nonEmpty (map dotField fields))
    A.ETypeSig inner ty -> ExprWithTySig noAnn (toGhcLHsExpr inner) (toSigWcType ty)
    A.EParen inner
      | A.EGetFieldProjection {} <- A.peelExprAnn inner -> toGhcHsExpr inner
      | otherwise -> HsPar parAnn (toGhcLHsExpr inner)
    A.EList [] -> HsVar noExtField (lA (getRdrName nilDataCon))
    A.EList elems -> ExplicitList noAnn (map toGhcLHsExpr elems)
    A.ETuple flavor elems -> tupleExpr flavor elems
    A.EUnboxedSum altIdx arity inner -> ExplicitSum noAnn (altIdx + 1) arity (toGhcLHsExpr inner)
    A.ETypeApp inner ty -> HsAppType noAnn (toGhcLHsExpr inner) (toWcType ty)
    A.EApp fun arg -> HsApp noExtField (toGhcLHsExpr fun) (toGhcLHsExpr arg)
    A.ETHExpQuote body -> HsUntypedBracket noExtField (ExpBr noAnn (toGhcLHsExpr body))
    A.ETHTypedQuote body -> HsTypedBracket noAnn (toGhcLHsExpr body)
    A.ETHDeclQuote decls -> HsUntypedBracket noExtField (DecBrL noAnn (map toGhcLHsDecl decls))
    A.ETHTypeQuote ty -> HsUntypedBracket noExtField (TypBr noAnn (toLHsType ty))
    A.ETHPatQuote pat -> HsUntypedBracket noExtField (PatBr noAnn (toLPat pat))
    A.ETHNameQuote quoted -> HsUntypedBracket noExtField (VarBr noAnn True (quotedExprName quoted))
    A.ETHTypeNameQuote ty -> HsUntypedBracket noExtField (VarBr noAnn False (quotedTypeName ty))
    A.ETHSplice body -> HsUntypedSplice noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr body))
    A.ETHTypedSplice body -> HsTypedSplice noExtField (HsTypedSpliceExpr noAnn (toGhcLHsExpr body))
    A.EProc pat cmd -> HsProc noAnn (toLPat pat) (lA (HsCmdTop noExtField (toLHsCmd cmd)))
    A.EPragma pragma inner -> pragmaExpr pragma inner

toGhcLHsExpr :: A.Expr -> LHsExpr GhcPs
toGhcLHsExpr = lA . toGhcHsExpr

pragmaExpr :: A.Pragma -> A.Expr -> HsExpr GhcPs
pragmaExpr pragma inner =
  case A.pragmaType pragma of
    A.PragmaSCC label ->
      HsPragE noExtField (HsPragSCC noAnn (stringLiteral label)) (toGhcLHsExpr inner)
    _ -> toGhcHsExpr inner

stringLiteral :: T.Text -> StringLiteral
stringLiteral label =
  StringLiteral
    { sl_st = NoSourceText,
      sl_fs = mkFastString (T.unpack label),
      sl_tc = Nothing
    }

integerExpr :: Integer -> A.NumericType -> T.Text -> HsExpr GhcPs
integerExpr value numeric raw =
  case numeric of
    A.TInteger -> HsOverLit noExtField (mkHsIntegral (IL (sourceText raw) False value))
    A.TIntHash -> HsLit noExtField (HsIntPrim NoSourceText value)
    A.TWordHash -> HsLit noExtField (HsWordPrim NoSourceText value)
    A.TInt8Hash -> HsLit noExtField (HsInt8Prim NoSourceText value)
    A.TInt16Hash -> HsLit noExtField (HsInt16Prim NoSourceText value)
    A.TInt32Hash -> HsLit noExtField (HsInt32Prim NoSourceText value)
    A.TInt64Hash -> HsLit noExtField (HsInt64Prim NoSourceText value)
    A.TWord8Hash -> HsLit noExtField (HsWord8Prim NoSourceText value)
    A.TWord16Hash -> HsLit noExtField (HsWord16Prim NoSourceText value)
    A.TWord32Hash -> HsLit noExtField (HsWord32Prim NoSourceText value)
    A.TWord64Hash -> HsLit noExtField (HsWord64Prim NoSourceText value)

floatExpr :: Rational -> A.FloatType -> T.Text -> HsExpr GhcPs
floatExpr value floatTy raw =
  let lit = fractionalLit value raw
   in case floatTy of
        A.TFractional -> HsOverLit noExtField (mkHsFractional lit)
        A.TFloatHash -> HsLit noExtField (HsFloatPrim noExtField lit)
        A.TDoubleHash -> HsLit noExtField (HsDoublePrim noExtField lit)

fractionalLit :: Rational -> T.Text -> FractionalLit
fractionalLit value raw =
  FL (sourceText raw) False significand' exponent' Base10
  where
    exponent' = fractionalExponent raw
    significand' = value / (10 ^^ exponent')

fractionalExponent :: T.Text -> Integer
fractionalExponent raw =
  explicitExponent - fromIntegral fractionalDigits
  where
    token = filter (/= '_') (T.unpack raw)
    tokenSignificand = takeWhile (\c -> c /= 'e' && c /= 'E' && c /= '#') token
    exponentText =
      case dropWhile (\c -> c /= 'e' && c /= 'E') token of
        [] -> ""
        (_ : rest) -> takeWhile exponentChar rest
    explicitExponent =
      case reads exponentText of
        [(value, "")] -> value
        _ -> 0
    fractionalDigits =
      case dropWhile (/= '.') tokenSignificand of
        [] -> 0
        (_ : rest) -> length (takeWhile isDigit rest)
    exponentChar c = c == '+' || c == '-' || isDigit c

tupleExpr :: A.TupleFlavor -> [Maybe A.Expr] -> HsExpr GhcPs
tupleExpr flavor elems =
  case elems of
    [] -> HsVar noExtField (lA (getRdrName (tupleDataCon (boxity flavor) 0)))
    _
      | all nullary elems -> HsVar noExtField (lA (getRdrName (tupleDataCon (boxity flavor) (length elems))))
      | otherwise -> ExplicitTuple tupleAnn (map tupArg elems) (boxity flavor)

tupArg :: Maybe A.Expr -> HsTupArg GhcPs
tupArg =
  maybe (Missing noAnn) (Present noExtField . toGhcLHsExpr)

nullary :: Maybe a -> Bool
nullary Nothing = True
nullary Just {} = False

operatorExpr :: A.Name -> LHsExpr GhcPs
operatorExpr = lA . HsVar noExtField . lA . toRdrName

recordBinds :: [A.RecordField A.Expr] -> Bool -> HsRecordBinds GhcPs
recordBinds fields wildcard =
  HsRecFields noExtField (map recordField fields) (if wildcard then Just (lA (RecFieldsDotDot 0)) else Nothing)

recordField :: A.RecordField A.Expr -> LHsRecField GhcPs (LHsExpr GhcPs)
recordField (A.RecordField name value pun) =
  lA (HsFieldBind (Just noAnn) (lA (fieldOcc name)) (toGhcLHsExpr value) pun)

recordUpdField :: A.RecordField A.Expr -> LHsRecUpdField GhcPs GhcPs
recordUpdField (A.RecordField name value pun) =
  lA (HsFieldBind (Just noAnn) (lA (fieldOcc name)) (toGhcLHsExpr value) pun)

recordConTarget :: A.Expr -> Maybe RdrName
recordConTarget expr =
  case A.peelExprAnn expr of
    A.EAnn _ inner -> recordConTarget inner
    A.EVar name
      | isConName name -> Just (toRdrName name)
      | otherwise -> Nothing
    A.EList [] -> Just (getRdrName nilDataCon)
    A.ETuple flavor [] -> Just (getRdrName (tupleDataCon (boxity flavor) 0))
    A.ETuple flavor elems | all nullary elems -> Just (getRdrName (tupleDataCon (boxity flavor) (length elems)))
    _ -> Nothing

isConName :: A.Name -> Bool
isConName name =
  case A.nameType name of
    A.NameConId -> True
    A.NameConSym -> True
    _ -> False

fieldOcc :: A.Name -> FieldOcc GhcPs
fieldOcc name = FieldOcc noExtField (lA (toRdrName name))

dotField :: A.Name -> DotFieldOcc GhcPs
dotField name = DotFieldOcc noAnn (lA (FieldLabelString (mkFastString (T.unpack (A.nameText name)))))

doFlavor :: A.DoFlavor -> HsDoFlavour
doFlavor flavor =
  case flavor of
    A.DoPlain -> DoExpr Nothing
    A.DoMdo -> MDoExpr Nothing
    A.DoQualified modName -> DoExpr (Just (mkModuleName (T.unpack modName)))
    A.DoQualifiedMdo modName -> MDoExpr (Just (mkModuleName (T.unpack modName)))

doStmts :: [A.DoStmt A.Expr] -> [ExprLStmt GhcPs]
doStmts [] = []
doStmts [A.DoExpr expr] = [lA (LastStmt noExtField (toGhcLHsExpr expr) Nothing noSyntaxExpr)]
doStmts (stmt : rest) = toDoStmt stmt : doStmts rest

toDoStmt :: A.DoStmt A.Expr -> ExprLStmt GhcPs
toDoStmt stmt =
  lA $
    case A.peelDoStmtAnn stmt of
      A.DoAnn _ inner -> unLoc (toDoStmt inner)
      A.DoBind pat body -> BindStmt noAnn (toLPat pat) (toGhcLHsExpr body)
      A.DoLetDecls decls -> LetStmt noAnn (localBinds decls)
      A.DoExpr body -> BodyStmt noExtField (toGhcLHsExpr body) noSyntaxExpr noSyntaxExpr
      A.DoRecStmt stmts ->
        RecStmt
          noAnn
          (lA (map toDoStmt stmts))
          []
          []
          noSyntaxExpr
          noSyntaxExpr
          noSyntaxExpr

compStmts :: A.Expr -> [A.CompStmt] -> [ExprLStmt GhcPs]
compStmts body stmts = compQualStmts stmts <> [lA (LastStmt noExtField (toGhcLHsExpr body) Nothing noSyntaxExpr)]

parCompStmts :: A.Expr -> [[A.CompStmt]] -> [ExprLStmt GhcPs]
parCompStmts body branches =
  [ lA $
      ParStmt
        noExtField
        (nonEmpty (map parBlock branches))
        parsedNoExpr
        noSyntaxExpr,
    lA (LastStmt noExtField (toGhcLHsExpr body) Nothing noSyntaxExpr)
  ]

parBlock :: [A.CompStmt] -> ParStmtBlock GhcPs GhcPs
parBlock stmts = ParStmtBlock noExtField (compQualStmts stmts) [] noSyntaxExpr

parsedNoExpr :: HsExpr GhcPs
parsedNoExpr = HsLit noExtField (HsString (SourceText "noExpr") (mkFastString "noExpr"))

compQualStmts :: [A.CompStmt] -> [ExprLStmt GhcPs]
compQualStmts = foldl step []
  where
    step acc stmt =
      case toCompStmt acc stmt of
        (converted, True) -> [converted]
        (converted, False) -> acc <> [converted]

toCompStmt :: [ExprLStmt GhcPs] -> A.CompStmt -> (ExprLStmt GhcPs, Bool)
toCompStmt previous stmt =
  let transformed stmt' = (lA stmt', True)
      plain stmt' = (lA stmt', False)
   in case A.peelCompStmtAnn stmt of
        A.CompAnn _ inner -> toCompStmt previous inner
        A.CompGen pat expr -> plain (BindStmt noAnn (toLPat pat) (toGhcLHsExpr expr))
        A.CompGuard expr -> plain (BodyStmt noExtField (toGhcLHsExpr expr) noSyntaxExpr noSyntaxExpr)
        A.CompLetDecls decls -> plain (LetStmt noAnn (localBinds decls))
        A.CompThen expr -> transformed (transStmt ThenForm previous expr Nothing)
        A.CompThenBy usingExpr byExpr -> transformed (transStmt ThenForm previous usingExpr (Just byExpr))
        A.CompGroupUsing expr -> transformed (transStmt GroupForm previous expr Nothing)
        A.CompGroupByUsing byExpr usingExpr -> transformed (transStmt GroupForm previous usingExpr (Just byExpr))

transStmt :: TransForm -> [ExprLStmt GhcPs] -> A.Expr -> Maybe A.Expr -> StmtLR GhcPs GhcPs (LHsExpr GhcPs)
transStmt form previous usingExpr byExpr =
  TransStmt noAnn form previous [] (toGhcLHsExpr usingExpr) (toGhcLHsExpr <$> byExpr) noSyntaxExpr noSyntaxExpr parsedNoExpr

arithSeqInfo :: A.ArithSeq -> ArithSeqInfo GhcPs
arithSeqInfo seq' =
  case A.peelArithSeqAnn seq' of
    A.ArithSeqAnn _ inner -> arithSeqInfo inner
    A.ArithSeqFrom from -> From (toGhcLHsExpr from)
    A.ArithSeqFromThen from next -> FromThen (toGhcLHsExpr from) (toGhcLHsExpr next)
    A.ArithSeqFromTo from to -> FromTo (toGhcLHsExpr from) (toGhcLHsExpr to)
    A.ArithSeqFromThenTo from next to -> FromThenTo (toGhcLHsExpr from) (toGhcLHsExpr next) (toGhcLHsExpr to)

toLPat :: A.Pattern -> LPat GhcPs
toLPat = lA . toPat

toPat :: A.Pattern -> Pat GhcPs
toPat pat =
  case A.peelPatternAnn pat of
    A.PAnn _ inner -> toPat inner
    A.PVar name -> VarPat noExtField (lA (toRdrName (A.qualifyName Nothing name)))
    A.PTypeBinder _ -> WildPat noExtField
    A.PTypeSyntax _ ty -> EmbTyPat noAnn (HsTP noExtField (toLHsType ty))
    A.PWildcard -> WildPat noExtField
    A.PLit lit -> litPat lit
    A.PQuasiQuote quoter body -> SplicePat noExtField (HsQuasiQuote noExtField (lA (toRdrName (A.nameFromText quoter))) (lA (mkFastString (T.unpack body))))
    A.PTuple flavor [] -> ConPat conPatAnn (lA (getRdrName (tupleDataCon (boxity flavor) 0))) (PrefixCon [])
    A.PTuple flavor pats -> TuplePat tupleAnn (map toLPat pats) (boxity flavor)
    A.PUnboxedSum altIdx arity inner -> SumPat noAnn (toLPat inner) (altIdx + 1) arity
    A.PList [] -> ConPat conPatAnn (lA (getRdrName nilDataCon)) (PrefixCon [])
    A.PList pats -> ListPat noAnn (map toLPat pats)
    A.PCon con [] pats -> ConPat conPatAnn (lA (toRdrName con)) (PrefixCon (map toLPat pats))
    A.PCon con _ pats -> ConPat conPatAnn (lA (toRdrName con)) (PrefixCon (map toLPat pats))
    A.PInfix lhs op rhs -> ConPat conPatAnn (lA (toRdrName op)) (InfixCon (toLPat lhs) (toLPat rhs))
    A.PView expr inner -> ViewPat noAnn (toGhcLHsExpr expr) (toLPat inner)
    A.PAs name inner -> AsPat noAnn (lA (toRdrName (A.qualifyName Nothing name))) (toLPat inner)
    A.PStrict inner -> BangPat noAnn (toLPat inner)
    A.PIrrefutable inner -> LazyPat noAnn (toLPat inner)
    A.PNegLit lit -> negLitPat lit
    A.PParen inner -> ParPat parAnn (toLPat inner)
    A.PRecord con fields wildcard ->
      ConPat conPatAnn (lA (toRdrName con)) (RecCon (HsRecFields noExtField (map patRecordField fields) (if wildcard then Just (lA (RecFieldsDotDot 0)) else Nothing)))
    A.PTypeSig inner ty -> SigPat noAnn (toLPat inner) (HsPS noAnn (toLHsType ty))
    A.PSplice expr -> SplicePat noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr expr))

litPat :: A.Literal -> Pat GhcPs
litPat lit =
  case A.peelLiteralAnn lit of
    A.LitAnn _ inner -> litPat inner
    A.LitInt value numeric raw -> numLitPat value numeric raw
    A.LitFloat value floatTy raw -> floatLitPat value floatTy raw
    A.LitChar value _ -> LitPat noExtField (HsChar NoSourceText value)
    A.LitCharHash value _ -> LitPat noExtField (HsCharPrim NoSourceText value)
    A.LitString value _ -> LitPat noExtField (HsString NoSourceText (mkFastString (T.unpack value)))
    A.LitStringHash value _ -> LitPat noExtField (HsStringPrim NoSourceText (BS.pack (T.unpack value)))

negLitPat :: A.Literal -> Pat GhcPs
negLitPat lit =
  case A.peelLiteralAnn lit of
    A.LitInt value A.TInteger raw -> NPat noAnn (lA (mkHsIntegral (IL (sourceText raw) False value))) (Just noSyntaxExpr) noSyntaxExpr
    A.LitFloat value A.TFractional raw -> NPat noAnn (lA (mkHsFractional (fractionalLit value raw))) (Just noSyntaxExpr) noSyntaxExpr
    other -> litPat other

numLitPat :: Integer -> A.NumericType -> T.Text -> Pat GhcPs
numLitPat value numeric raw =
  case numeric of
    A.TInteger -> NPat noAnn (lA (mkHsIntegral (IL (sourceText raw) False value))) Nothing noSyntaxExpr
    _ -> case integerExpr value numeric raw of
      HsLit _ lit -> LitPat noExtField lit
      _ -> NPat noAnn (lA (mkHsIntegral (mkIntegralLit value))) Nothing noSyntaxExpr

floatLitPat :: Rational -> A.FloatType -> T.Text -> Pat GhcPs
floatLitPat value floatTy raw =
  case floatExpr value floatTy raw of
    HsLit _ lit -> LitPat noExtField lit
    _ -> NPat noAnn (lA (mkHsFractional (fractionalLit value raw))) Nothing noSyntaxExpr

patRecordField :: A.RecordField A.Pattern -> LHsRecField GhcPs (LPat GhcPs)
patRecordField (A.RecordField name value pun) =
  lA (HsFieldBind (Just noAnn) (lA (fieldOcc name)) (toLPat value) pun)

toLHsType :: A.Type -> LHsType GhcPs
toLHsType = lA . toHsType

toHsType :: A.Type -> HsType GhcPs
toHsType ty =
  case A.peelTypeAnn ty of
    A.TAnn _ inner -> toHsType inner
    A.TVar name -> HsTyVar noAnn NotPromoted (lA (toRdrName (A.qualifyName Nothing name)))
    A.TCon name A.Promoted
      | Just elems <- promotedListNameElems name ->
          HsExplicitListTy (noAnn, noAnn, noAnn) IsPromoted elems
    A.TCon name A.Promoted
      | Just elems <- promotedTupleNameElems name ->
          HsExplicitTupleTy (noAnn, noAnn, noAnn) IsPromoted elems
    A.TCon name promotion -> HsTyVar noAnn (promotionFlag promotion) (lA (toRdrName name))
    A.TBuiltinCon builtin -> builtinType builtin
    A.TImplicitParam name inner -> HsIParamTy noAnn (lA (HsIPName (mkFastString (T.unpack (T.dropWhile (== '?') name))))) (toLHsType inner)
    A.TTypeLit lit -> HsTyLit noExtField (typeLit lit)
    A.TStar -> HsStarTy noExtField False
    A.TQuasiQuote quoter body -> HsSpliceTy noExtField (HsQuasiQuote noExtField (lA (toRdrName (A.nameFromText quoter))) (lA (mkFastString (T.unpack body))))
    A.TForall telescope body ->
      case splitTrailingKindSig body of
        Just (inner, kind) -> HsKindSig noAnn (lA (HsForAllTy noExtField (forallTele telescope) (toLHsType inner))) (toLHsType kind)
        _ -> HsForAllTy noExtField (forallTele telescope) (toLHsType body)
    A.TApp fun arg -> HsAppTy noExtField (toLHsType fun) (toLHsType arg)
    A.TTypeApp fun arg -> HsAppKindTy noAnn (toLHsType fun) (toLHsKindAppArg arg)
    A.TInfix lhs op promotion rhs -> HsOpTy noExtField (promotionFlag promotion) (toLHsType lhs) (lA (toRdrName op)) (toLHsType rhs)
    A.TFun arrow lhs rhs -> HsFunTy noExtField (multAnn arrow) (toLHsType lhs) (toLHsType rhs)
    A.TTuple flavor promotion elems ->
      case promotion of
        A.Promoted -> HsExplicitTupleTy (noAnn, noAnn, noAnn) IsPromoted (map toLHsPromotedCollectionElem elems)
        A.Unpromoted -> HsTupleTy noAnn (tupleSort flavor) (map toLHsType elems)
    A.TUnboxedSum elems -> HsSumTy noAnn (map toLHsType elems)
    A.TList promotion elems ->
      case promotion of
        A.Promoted -> HsExplicitListTy (noAnn, noAnn, noAnn) IsPromoted (map toLHsPromotedCollectionElem elems)
        A.Unpromoted | [elemTy] <- elems -> HsListTy noAnn (toLHsType elemTy)
        A.Unpromoted -> HsExplicitListTy (noAnn, noAnn, noAnn) NotPromoted (map toLHsType elems)
    A.TParen inner -> HsParTy parAnn (toLHsType inner)
    A.TKindSig inner kind -> HsKindSig noAnn (toLHsType inner) (toLHsType kind)
    A.TContext context body ->
      case splitTrailingKindSig body of
        Just (inner, kind) -> HsKindSig noAnn (lA (HsQualTy noExtField (lA (map toLHsType context)) (toLHsType inner))) (toLHsType kind)
        _ -> HsQualTy noExtField (lA (map toLHsType context)) (toLHsType body)
    A.TSplice expr -> HsSpliceTy noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr expr))
    A.TWildcard -> HsWildCardTy noAnn

splitTrailingKindSig :: A.Type -> Maybe (A.Type, A.Type)
splitTrailingKindSig ty =
  case A.peelTypeAnn ty of
    A.TKindSig inner kind -> Just (inner, kind)
    A.TForall telescope body -> do
      (inner, kind) <- splitTrailingKindSig body
      Just (A.TForall telescope inner, kind)
    A.TContext context body -> do
      (inner, kind) <- splitTrailingKindSig body
      Just (A.TContext context inner, kind)
    _ -> Nothing

builtinType :: A.TypeBuiltinCon -> HsType GhcPs
builtinType builtin =
  case builtin of
    A.TBuiltinTuple arity -> HsTyVar noAnn NotPromoted (lA (getRdrName (tupleTyCon Boxed arity)))
    A.TBuiltinArrow -> HsTyVar noAnn NotPromoted (lA (getRdrName unrestrictedFunTyConName))
    A.TBuiltinList -> HsTyVar noAnn NotPromoted (lA (getRdrName nilDataCon))
    A.TBuiltinCons -> HsTyVar noAnn NotPromoted (lA (mkRdrUnqual (mkDataOcc ":")))

toLHsKindAppArg :: A.Type -> LHsType GhcPs
toLHsKindAppArg ty =
  case A.peelTypeAnn ty of
    -- GHC preserves parentheses around @*@ when it appears as an invisible
    -- kind argument.  For example, @f @*@ parses as an argument shaped like
    -- @HsParTy (HsStarTy ...)@, while ordinary @*@ in type position is just
    -- @HsStarTy@ under StarIsType.
    A.TStar -> lA (HsParTy parAnn (toLHsType ty))
    _ -> toLHsType ty

toLHsPromotedCollectionElem :: A.Type -> LHsType GhcPs
toLHsPromotedCollectionElem ty =
  case A.peelTypeAnn ty of
    A.TParen inner -> toLHsPromotedCollectionElem inner
    _ -> toLHsType ty

typeLit :: A.TypeLiteral -> HsTyLit GhcPs
typeLit lit =
  case lit of
    A.TypeLitInteger value _ -> HsNumTy NoSourceText value
    A.TypeLitSymbol value _ -> HsStrTy NoSourceText (mkFastString (T.unpack value))
    A.TypeLitChar value _ -> HsCharTy NoSourceText value

promotedTupleNameElems :: A.Name -> Maybe [LHsType GhcPs]
promotedTupleNameElems name
  | isNothing (A.nameQualifier name),
    Just body <- T.stripPrefix "(" (A.nameText name),
    Just inner <- T.stripSuffix ")" body,
    "," `T.isInfixOf` inner =
      traverse promotedTupleNameElem (splitTopLevelCommas inner)
  | otherwise = Nothing

promotedListNameElems :: A.Name -> Maybe [LHsType GhcPs]
promotedListNameElems name
  | isNothing (A.nameQualifier name),
    Just body <- T.stripPrefix "[" (A.nameText name),
    Just inner <- T.stripSuffix "]" body =
      traverse promotedTupleNameElem (splitTopLevelCommas inner)
  | otherwise = Nothing

promotedTupleNameElem :: T.Text -> Maybe (LHsType GhcPs)
promotedTupleNameElem raw =
  let text = T.strip raw
   in case text of
        "*" -> Just (lA (HsStarTy noExtField False))
        _
          | Just value <- decimalTypeName text ->
              Just (lA (HsTyLit noExtField (HsNumTy NoSourceText value)))
          | Just value <- stringTypeName text ->
              Just (lA (HsTyLit noExtField (HsStrTy NoSourceText (mkFastString (T.unpack value)))))
          | Just op <- T.stripPrefix "(" text >>= T.stripSuffix ")" ->
              Just (lA (HsTyVar noAnn NotPromoted (lA (qualifiedSymbolTextRdrName op))))
          | T.null text -> Nothing
          | otherwise ->
              Just (lA (HsTyVar noAnn NotPromoted (lA (toRdrName (A.nameFromText text)))))

splitTopLevelCommas :: T.Text -> [T.Text]
splitTopLevelCommas input =
  reverse (current : parts)
  where
    (_, current, parts) = T.foldl' step (0 :: Int, "", []) input
    step (depth, acc, done) char =
      case char of
        '(' -> (depth + 1, T.snoc acc char, done)
        ')' -> (max 0 (depth - 1), T.snoc acc char, done)
        ',' | depth == 0 -> (depth, "", acc : done)
        _ -> (depth, T.snoc acc char, done)

toWcType :: A.Type -> LHsWcType GhcPs
toWcType ty = HsWC noExtField (toLHsType ty)

toSigWcType :: A.Type -> LHsSigWcType GhcPs
toSigWcType ty =
  case A.peelTypeAnn ty of
    A.TAnn _ inner -> toSigWcType inner
    A.TForall (A.ForallTelescope A.ForallInvisible binders) body ->
      HsWC noExtField (lA (HsSig noExtField (HsOuterExplicit noAnn (map toInvisibleTyVarBndr binders)) (toLHsType body)))
    _ ->
      HsWC noExtField (lA (HsSig noExtField (HsOuterImplicit noExtField) (toLHsType ty)))

forallTele :: A.ForallTelescope -> HsForAllTelescope GhcPs
forallTele (A.ForallTelescope visibility binders) =
  case visibility of
    A.ForallVisible -> HsForAllVis noAnn (map (toTyVarBndr ()) binders)
    A.ForallInvisible -> HsForAllInvis noAnn (map toInvisibleTyVarBndr binders)

toInvisibleTyVarBndr :: A.TyVarBinder -> LHsTyVarBndr Specificity GhcPs
toInvisibleTyVarBndr binder =
  toTyVarBndr specificity binder
  where
    specificity =
      case A.tyVarBinderSpecificity binder of
        A.TyVarBInferred -> InferredSpec
        A.TyVarBSpecified -> SpecifiedSpec

toTyVarBndr :: flag -> A.TyVarBinder -> LHsTyVarBndr flag GhcPs
toTyVarBndr flag binder =
  lA (HsTvb noAnn flag (HsBndrVar noExtField (lA (mkRdrUnqual (mkVarOcc (T.unpack (A.tyVarBinderName binder)))))) kind)
  where
    kind = maybe (HsBndrNoKind noExtField) (HsBndrKind noExtField . toLHsType) (A.tyVarBinderKind binder)

multAnn :: A.ArrowKind -> HsMultAnn GhcPs
multAnn arrow =
  case arrow of
    A.ArrowUnrestricted -> HsUnannotated EpPatBind
    A.ArrowLinear -> HsLinearAnn noAnn
    A.ArrowExplicit ty -> HsExplicitMult (noAnn, EpPatBind) (toLHsType ty)

localBinds :: [A.Decl] -> HsLocalBinds GhcPs
localBinds decls =
  let (binds, sigs) = foldMap declBindSig decls
   in if null binds && null sigs
        then EmptyLocalBinds noExtField
        else HsValBinds noAnn (ValBinds NoAnnSortKey binds sigs)

whereBinds :: Maybe [A.Decl] -> HsLocalBinds GhcPs
whereBinds Nothing = EmptyLocalBinds noExtField
whereBinds (Just decls) =
  let (binds, sigs) = foldMap declBindSig decls
   in HsValBinds noAnn (ValBinds NoAnnSortKey binds sigs)

declBindSig :: A.Decl -> ([LHsBind GhcPs], [LSig GhcPs])
declBindSig decl =
  case A.peelDeclAnn decl of
    A.DeclAnn _ inner -> declBindSig inner
    A.DeclValue value -> ([lA (valueBind value)], [])
    A.DeclTypeSig names ty -> ([], [lA (TypeSig noAnn (map (lA . toRdrName . A.qualifyName Nothing) names) (toSigWcType ty))])
    A.DeclSplice expr -> ([lA (PatBind noExtField (lA (SplicePat noExtField (HsUntypedSpliceExpr noAnn (toGhcLHsExpr expr)))) (HsUnannotated EpPatBind) (grhss (A.UnguardedRhs [] (A.EVar "undefined") Nothing)))], [])
    _ -> ([], [])

valueBind :: A.ValueDecl -> HsBind GhcPs
valueBind value =
  case value of
    A.FunctionBind name matches ->
      FunBind noExtField (lA funName) (matchGroup (FunRhs (lA funName) Prefix NoSrcStrict noAnn) (map (functionMatch name) matches))
      where
        funName = toRdrName (A.qualifyName Nothing name)
    A.PatternBind A.NoMultiplicityTag pat rhs
      | Just name <- varPatName pat ->
          let funName = toRdrName (A.qualifyName Nothing name)
           in FunBind noExtField (lA funName) (matchGroup (FunRhs (lA funName) Prefix NoSrcStrict noAnn) [match (FunRhs (lA funName) Prefix NoSrcStrict noAnn) [] rhs])
    A.PatternBind A.NoMultiplicityTag pat rhs
      | Just name <- strictVarPatName pat ->
          let funName = toRdrName (A.qualifyName Nothing name)
           in FunBind noExtField (lA funName) (matchGroup (FunRhs (lA funName) Prefix SrcStrict noAnn) [match (FunRhs (lA funName) Prefix SrcStrict noAnn) [] rhs])
    A.PatternBind multiplicity pat rhs ->
      PatBind noExtField (toLPat pat) (patMult multiplicity) (grhss rhs)

varPatName :: A.Pattern -> Maybe A.UnqualifiedName
varPatName pat =
  case A.peelPatternAnn pat of
    A.PAnn _ inner -> varPatName inner
    A.PVar name -> Just name
    _ -> Nothing

strictVarPatName :: A.Pattern -> Maybe A.UnqualifiedName
strictVarPatName pat =
  case A.peelPatternAnn pat of
    A.PAnn _ inner -> strictVarPatName inner
    A.PStrict inner ->
      case A.peelPatternAnn inner of
        A.PAnn _ inner' -> strictVarPatName (A.PStrict inner')
        A.PVar name -> Just name
        _ -> Nothing
    _ -> Nothing

functionMatch :: A.BinderName -> A.Match -> Match GhcPs (LHsExpr GhcPs)
functionMatch name (A.Match _ headForm pats rhs) =
  Match noExtField FunRhs {mc_fun = lA (toRdrName (A.qualifyName Nothing name)), mc_fixity = fixity, mc_strictness = NoSrcStrict, mc_an = noAnn} (lA (map toLPat pats)) (grhss rhs)
  where
    fixity =
      case headForm of
        A.MatchHeadPrefix -> Prefix
        A.MatchHeadInfix -> Infix

caseAltMatch :: A.CaseAlt A.Expr -> Match GhcPs (LHsExpr GhcPs)
caseAltMatch = caseAltMatchWith CaseAlt

caseAltMatchWith :: HsMatchContext (LIdP GhcPs) -> A.CaseAlt A.Expr -> Match GhcPs (LHsExpr GhcPs)
caseAltMatchWith context (A.CaseAlt _ pat rhs) = match context [pat] rhs

lambdaCasesAltMatchWith :: HsMatchContext (LIdP GhcPs) -> A.LambdaCaseAlt -> Match GhcPs (LHsExpr GhcPs)
lambdaCasesAltMatchWith context (A.LambdaCaseAlt _ pats rhs) = match context pats rhs

match :: HsMatchContext (LIdP GhcPs) -> [A.Pattern] -> A.Rhs A.Expr -> Match GhcPs (LHsExpr GhcPs)
match context pats rhs =
  Match noExtField context (lA (map toLPat pats)) (grhss rhs)

matchGroup :: HsMatchContext (LIdP GhcPs) -> [Match GhcPs (LHsExpr GhcPs)] -> MatchGroup GhcPs (LHsExpr GhcPs)
matchGroup _ matches =
  MG FromSource (lA (map lA matches))

grhss :: A.Rhs A.Expr -> GRHSs GhcPs (LHsExpr GhcPs)
grhss rhs =
  case rhs of
    A.UnguardedRhs _ body whereDecls ->
      GRHSs emptyComments (lA (GRHS noAnn [] (toGhcLHsExpr body)) :| []) (whereBinds whereDecls)
    A.GuardedRhss _ rhss whereDecls ->
      GRHSs emptyComments (nonEmpty (map toGRHS rhss)) (whereBinds whereDecls)

toGRHS :: A.GuardedRhs A.Expr -> LGRHS GhcPs (LHsExpr GhcPs)
toGRHS (A.GuardedRhs _ guards body) =
  lA (GRHS noAnn (map guardStmt guards) (toGhcLHsExpr body))

guardStmt :: A.GuardQualifier -> GuardLStmt GhcPs
guardStmt guard =
  lA $
    case A.peelGuardQualifierAnn guard of
      A.GuardAnn _ inner -> unLoc (guardStmt inner)
      A.GuardExpr expr -> BodyStmt noExtField (toGhcLHsExpr expr) noSyntaxExpr noSyntaxExpr
      A.GuardPat pat expr -> BindStmt noAnn (toLPat pat) (toGhcLHsExpr expr)
      A.GuardLet decls -> LetStmt noAnn (localBinds decls)

patMult :: A.MultiplicityTag -> HsMultAnn GhcPs
patMult tag =
  case tag of
    A.NoMultiplicityTag -> HsUnannotated EpPatBind
    A.LinearMultiplicityTag -> HsLinearAnn noAnn
    A.ExplicitMultiplicityTag ty -> HsExplicitMult (noAnn, EpPatBind) (toLHsType ty)

toLHsCmd :: A.Cmd -> LHsCmd GhcPs
toLHsCmd = lA . toHsCmd

toHsCmd :: A.Cmd -> HsCmd GhcPs
toHsCmd cmd =
  case A.peelCmdAnn cmd of
    A.CmdAnn _ inner -> toHsCmd inner
    A.CmdArrApp lhs appTy rhs -> HsCmdArrApp (NormalSyntax, EpaSpan noSrcSpan) (toGhcLHsExpr lhs) (toGhcLHsExpr rhs) (arrAppType appTy) True
    A.CmdInfix lhs op rhs -> HsCmdArrForm noAnn (operatorExpr op) Infix [cmdTop lhs, cmdTop rhs]
    A.CmdDo stmts -> HsCmdDo noAnn (lA (cmdDoStmts stmts))
    A.CmdIf cond yes no -> HsCmdIf noAnn noSyntaxExpr (toGhcLHsExpr cond) (toLHsCmd yes) (toLHsCmd no)
    A.CmdCase scrut alts -> HsCmdCase noAnn (toGhcLHsExpr scrut) (cmdMatchGroup CaseAlt (map cmdCaseAltMatch alts))
    A.CmdLet decls body -> HsCmdLet noAnn (localBinds decls) (toLHsCmd body)
    A.CmdLam pats body -> HsCmdLam noAnn LamSingle (cmdMatchGroup (LamAlt LamSingle) [cmdMatch (LamAlt LamSingle) pats (A.UnguardedRhs [] body Nothing)])
    A.CmdApp fun arg -> HsCmdApp noExtField (toLHsCmd fun) (toGhcLHsExpr arg)
    A.CmdPar inner -> HsCmdPar parAnn (toLHsCmd inner)

cmdTop :: A.Cmd -> LHsCmdTop GhcPs
cmdTop cmd = lA (HsCmdTop noExtField (toLHsCmd cmd))

arrAppType :: A.ArrAppType -> HsArrAppType
arrAppType appTy =
  case appTy of
    A.HsFirstOrderApp -> HsFirstOrderApp
    A.HsHigherOrderApp -> HsHigherOrderApp

cmdDoStmts :: [A.DoStmt A.Cmd] -> [CmdLStmt GhcPs]
cmdDoStmts [] = []
cmdDoStmts [A.DoExpr cmd] = [lA (LastStmt noExtField (toLHsCmd cmd) Nothing noSyntaxExpr)]
cmdDoStmts (stmt : rest) = toCmdDoStmt stmt : cmdDoStmts rest

toCmdDoStmt :: A.DoStmt A.Cmd -> CmdLStmt GhcPs
toCmdDoStmt stmt =
  lA $
    case A.peelDoStmtAnn stmt of
      A.DoAnn _ inner -> unLoc (toCmdDoStmt inner)
      A.DoBind pat body -> BindStmt noAnn (toLPat pat) (toLHsCmd body)
      A.DoLetDecls decls -> LetStmt noAnn (localBinds decls)
      A.DoExpr body -> BodyStmt noExtField (toLHsCmd body) noSyntaxExpr noSyntaxExpr
      A.DoRecStmt stmts ->
        RecStmt
          noAnn
          (lA (map toCmdDoStmt stmts))
          []
          []
          noSyntaxExpr
          noSyntaxExpr
          noSyntaxExpr

cmdCaseAltMatch :: A.CaseAlt A.Cmd -> Match GhcPs (LHsCmd GhcPs)
cmdCaseAltMatch (A.CaseAlt _ pat rhs) = cmdMatch CaseAlt [pat] rhs

cmdMatch :: HsMatchContext (LIdP GhcPs) -> [A.Pattern] -> A.Rhs A.Cmd -> Match GhcPs (LHsCmd GhcPs)
cmdMatch context pats rhs =
  Match noExtField context (lA (map toLPat pats)) (cmdGrhss rhs)

cmdMatchGroup :: HsMatchContext (LIdP GhcPs) -> [Match GhcPs (LHsCmd GhcPs)] -> MatchGroup GhcPs (LHsCmd GhcPs)
cmdMatchGroup _ matches =
  MG FromSource (lA (map lA matches))

cmdGrhss :: A.Rhs A.Cmd -> GRHSs GhcPs (LHsCmd GhcPs)
cmdGrhss rhs =
  case rhs of
    A.UnguardedRhs _ body whereDecls ->
      GRHSs emptyComments (lA (GRHS noAnn [] (toLHsCmd body)) :| []) (whereBinds whereDecls)
    A.GuardedRhss _ rhss whereDecls ->
      GRHSs emptyComments (nonEmpty (map cmdGRHS rhss)) (whereBinds whereDecls)

cmdGRHS :: A.GuardedRhs A.Cmd -> LGRHS GhcPs (LHsCmd GhcPs)
cmdGRHS (A.GuardedRhs _ guards body) =
  lA (GRHS noAnn (map guardStmt guards) (toLHsCmd body))

quotedExprName :: A.Expr -> LIdP GhcPs
quotedExprName expr =
  case A.peelExprAnn expr of
    A.EVar name -> lA (toRdrName name)
    A.EList [] -> lA (getRdrName nilDataCon)
    A.ETuple flavor elems -> lA (getRdrName (tupleDataCon (boxity flavor) (length elems)))
    _ -> lA (mkRdrUnqual (mkVarOcc "unsupportedQuote"))

quotedTypeName :: A.Type -> LIdP GhcPs
quotedTypeName ty =
  case A.peelTypeAnn ty of
    A.TCon name _ -> lA (toRdrName name)
    A.TBuiltinCon A.TBuiltinList -> lA (getRdrName nilDataCon)
    A.TTuple flavor _ elems -> lA (getRdrName (tupleTyCon (boxity flavor) (length elems)))
    _ -> lA (mkRdrUnqual (mkDataOcc "UnsupportedQuote"))

toRdrName :: A.Name -> RdrName
toRdrName name =
  let occ = case A.nameType name of
        A.NameVarId -> mkVarOcc
        A.NameVarSym -> mkVarOcc
        A.NameConId -> mkDataOcc
        A.NameConSym -> mkDataOcc
      local = occ (T.unpack (A.nameText name))
   in case A.nameQualifier name of
        Nothing -> mkRdrUnqual local
        Just qualifier -> mkRdrQual (mkModuleName (T.unpack qualifier)) local

qualifiedSymbolTextRdrName :: T.Text -> RdrName
qualifiedSymbolTextRdrName text =
  case qualifiedSymbolTextParts text of
    Just (qualifier, symbol) -> mkRdrQual (mkModuleName (T.unpack qualifier)) (symbolicOcc symbol)
    Nothing -> mkRdrUnqual (symbolicOcc text)

qualifiedSymbolTextParts :: T.Text -> Maybe (T.Text, T.Text)
qualifiedSymbolTextParts text =
  listToMaybe $
    reverse
      [ (qualifier, symbol)
      | (qualifier, rest) <- T.breakOnAll "." text,
        let symbol = T.drop 1 rest,
        isModulePathText qualifier,
        isSymbolicNameText symbol
      ]

isModulePathText :: T.Text -> Bool
isModulePathText text =
  case T.splitOn "." text of
    [] -> False
    segments -> all isModuleSegmentText segments

isModuleSegmentText :: T.Text -> Bool
isModuleSegmentText text =
  case T.uncons text of
    Just (char, rest) -> isConIdentifierStartChar char && T.all isIdentChar rest
    Nothing -> False

isIdentChar :: Char -> Bool
isIdentChar char = isIdentifierStartChar char || isIdentifierNumberChar char || char == '\''

isIdentifierStartChar :: Char -> Bool
isIdentifierStartChar char = char == '_' || generalCategory char == LowercaseLetter || isConIdentifierStartChar char

isConIdentifierStartChar :: Char -> Bool
isConIdentifierStartChar char = generalCategory char `elem` [UppercaseLetter, TitlecaseLetter]

isIdentifierNumberChar :: Char -> Bool
isIdentifierNumberChar char =
  generalCategory char
    `elem` [ DecimalNumber,
             LetterNumber,
             OtherNumber,
             NonSpacingMark,
             SpacingCombiningMark
           ]

symbolicOcc :: T.Text -> OccName
symbolicOcc text
  | Just (':', _) <- T.uncons text = mkDataOcc (T.unpack text)
  | otherwise = mkVarOcc (T.unpack text)

isSymbolicNameText :: T.Text -> Bool
isSymbolicNameText text =
  case T.uncons text of
    Just (':', _) -> True
    Just (char, _) -> not (isIdentifierStartChar char || char == '\'')
    Nothing -> False

decimalTypeName :: T.Text -> Maybe Integer
decimalTypeName text =
  case TR.decimal text of
    Right (value, "") -> Just value
    _ -> Nothing

stringTypeName :: T.Text -> Maybe T.Text
stringTypeName text =
  T.stripPrefix "\"" text >>= T.stripSuffix "\""

boxity :: A.TupleFlavor -> Boxity
boxity flavor =
  case flavor of
    A.Boxed -> Boxed
    A.Unboxed -> Unboxed

tupleSort :: A.TupleFlavor -> HsTupleSort
tupleSort flavor =
  case flavor of
    A.Boxed -> HsBoxedOrConstraintTuple
    A.Unboxed -> HsUnboxedTuple

promotionFlag :: A.TypePromotion -> PromotionFlag
promotionFlag promotion =
  case promotion of
    A.Unpromoted -> NotPromoted
    A.Promoted -> IsPromoted

sourceText :: T.Text -> SourceText
sourceText = SourceText . mkFastString . T.unpack

lA :: (HasAnnotation e) => a -> GenLocated e a
lA = noLocA

recordAnn :: (Maybe (EpToken "{"), Maybe (EpToken "}"))
recordAnn = (Just noAnn, Just noAnn)

conPatAnn :: (Maybe (EpToken "{"), Maybe (EpToken "}"))
conPatAnn = (Nothing, Nothing)

parAnn :: (EpToken "(", EpToken ")")
parAnn = (noAnn, noAnn)

tupleAnn :: (EpaLocation, EpaLocation)
tupleAnn = (EpaSpan noSrcSpan, EpaSpan noSrcSpan)

nonEmpty :: [a] -> NonEmpty a
nonEmpty [] = error "aihc-parser-compat: expected a non-empty list"
nonEmpty (x : xs) = x :| xs
