{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- This module reexports a simplified view on "Language.Haskell.Exts.Syntax".
-- The idea is to expose datatypes like
--
-- > data Name l = Ident l String | Symbol l String
--
-- using ghc's pattern synonyms:
--
-- > type Name = H.Name ()
-- > pattern Ident a = H.Ident () a
-- > pattern Symbol a = H.Symbol () a

module Language.Haskell.Exts.Simple.Syntax (
    module Language.Haskell.Exts.Simple.Syntax,
    module Language.Haskell.Exts.Syntax
) where

import qualified Language.Haskell.Exts.Syntax as H
import Language.Haskell.Exts.Syntax (
    Boxed (..),
    Tool (..),
 )

-- * Datatypes and Constructors

-- ** `H.ModuleName`
type ModuleName = H.ModuleName ()
pattern ModuleName a = H.ModuleName () (a :: String) :: ModuleName

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ModuleName #-}
#endif

-- ** `H.SpecialCon`
type SpecialCon = H.SpecialCon ()
pattern UnitCon = H.UnitCon () :: SpecialCon
pattern ListCon = H.ListCon () :: SpecialCon
pattern FunCon = H.FunCon () :: SpecialCon
pattern TupleCon a b = H.TupleCon () (a :: Boxed) (b :: Int) :: SpecialCon
pattern Cons = H.Cons () :: SpecialCon
pattern UnboxedSingleCon = H.UnboxedSingleCon () :: SpecialCon
pattern ExprHole = H.ExprHole () :: SpecialCon

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE UnitCon, ListCon, FunCon, TupleCon, Cons, UnboxedSingleCon, ExprHole #-}
#endif

-- ** `H.QName`
type QName = H.QName ()
pattern Qual a b = H.Qual () (a :: ModuleName) (b :: Name) :: QName
pattern UnQual a = H.UnQual () (a :: Name) :: QName
pattern Special a = H.Special () (a :: SpecialCon) :: QName

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Qual, UnQual, Special #-}
#endif

-- ** `H.Name`
type Name = H.Name ()
pattern Ident a = H.Ident () (a :: String) :: Name
pattern Symbol a = H.Symbol () (a :: String) :: Name

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Ident, Symbol #-}
#endif

-- ** `H.IPName`
type IPName = H.IPName ()
pattern IPDup a = H.IPDup () (a :: String) :: IPName
pattern IPLin a = H.IPLin () (a :: String) :: IPName

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE IPDup, IPLin #-}
#endif

-- ** `H.QOp`
type QOp = H.QOp ()
pattern QVarOp a = H.QVarOp () (a :: QName) :: QOp
pattern QConOp a = H.QConOp () (a :: QName) :: QOp

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE QVarOp, QConOp #-}
#endif

-- ** `H.Op`
type Op = H.Op ()
pattern VarOp a = H.VarOp () (a :: Name) :: Op
pattern ConOp a = H.ConOp () (a :: Name) :: Op

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE VarOp, ConOp #-}
#endif

-- ** `H.CName`
type CName = H.CName ()
pattern VarName a = H.VarName () (a :: Name) :: CName
pattern ConName a = H.ConName () (a :: Name) :: CName

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE VarName, ConName #-}
#endif

-- ** `H.Module`
type Module = H.Module ()
pattern Module a b c d = H.Module () (a :: (Maybe ModuleHead)) (b :: [ModulePragma]) (c :: [ImportDecl]) (d :: [Decl]) :: Module
pattern XmlPage a b c d e f = H.XmlPage () (a :: ModuleName) (b :: [ModulePragma]) (c :: XName) (d :: [XAttr]) (e :: (Maybe Exp)) (f :: [Exp]) :: Module
pattern XmlHybrid a b c d e f g h = H.XmlHybrid () (a :: (Maybe ModuleHead)) (b :: [ModulePragma]) (c :: [ImportDecl]) (d :: [Decl]) (e :: XName) (f :: [XAttr]) (g :: (Maybe Exp)) (h :: [Exp]) :: Module

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Module, XmlPage, XmlHybrid #-}
#endif

-- ** `H.ModuleHead`
type ModuleHead = H.ModuleHead ()
pattern ModuleHead a b c = H.ModuleHead () (a :: ModuleName) (b :: (Maybe WarningText)) (c :: (Maybe ExportSpecList)) :: ModuleHead

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ModuleHead #-}
#endif

-- ** `H.ExportSpecList`
type ExportSpecList = H.ExportSpecList ()
pattern ExportSpecList a = H.ExportSpecList () (a :: [ExportSpec]) :: ExportSpecList

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ExportSpecList #-}
#endif

-- ** `H.ExportSpec`
type ExportSpec = H.ExportSpec ()
pattern EVar a = H.EVar () (a :: QName) :: ExportSpec
pattern EAbs a b = H.EAbs () (a :: Namespace) (b :: QName) :: ExportSpec
pattern EThingWith a b c = H.EThingWith () (a :: EWildcard) (b :: QName) (c :: [CName]) :: ExportSpec
pattern EModuleContents a = H.EModuleContents () (a :: ModuleName) :: ExportSpec

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE EVar, EAbs, EThingWith, EModuleContents #-}
#endif

-- ** `H.EWildcard`
type EWildcard = H.EWildcard ()
pattern NoWildcard = H.NoWildcard () :: EWildcard
pattern EWildcard a = H.EWildcard () (a :: Int) :: EWildcard

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE NoWildcard, EWildcard #-}
#endif

-- ** `H.Namespace`
type Namespace = H.Namespace ()
pattern NoNamespace = H.NoNamespace () :: Namespace
pattern TypeNamespace = H.TypeNamespace () :: Namespace
pattern PatternNamespace = H.PatternNamespace () :: Namespace

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE NoNamespace, TypeNamespace, PatternNamespace #-}
#endif

-- ** `H.ImportDecl`
type ImportDecl = H.ImportDecl ()
pattern ImportDecl { importModule, importQualified, importSrc, importSafe, importPkg, importAs, importSpecs } =
    H.ImportDecl () importModule importQualified importSrc importSafe importPkg importAs importSpecs :: ImportDecl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ImportDecl #-}
#endif

-- ** `H.ImportSpecList`
type ImportSpecList = H.ImportSpecList ()
pattern ImportSpecList a b = H.ImportSpecList () (a :: Bool) (b :: [ImportSpec]) :: ImportSpecList

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ImportSpecList #-}
#endif

-- ** `H.ImportSpec`
type ImportSpec = H.ImportSpec ()
pattern IVar a = H.IVar () (a :: Name) :: ImportSpec
pattern IAbs a b = H.IAbs () (a :: Namespace) (b :: Name) :: ImportSpec
pattern IThingAll a = H.IThingAll () (a :: Name) :: ImportSpec
pattern IThingWith a b = H.IThingWith () (a :: Name) (b :: [CName]) :: ImportSpec

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE IVar, IAbs, IThingAll, IThingWith #-}
#endif

-- ** `H.Assoc`
type Assoc = H.Assoc ()
pattern AssocNone = H.AssocNone () :: Assoc
pattern AssocLeft = H.AssocLeft () :: Assoc
pattern AssocRight = H.AssocRight () :: Assoc

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE AssocNone, AssocLeft, AssocRight #-}
#endif

-- ** `H.Decl`
type Decl = H.Decl ()
pattern TypeDecl a b = H.TypeDecl () (a :: DeclHead) (b :: Type) :: Decl
pattern TypeFamDecl a b c = H.TypeFamDecl () (a :: DeclHead) (b :: (Maybe ResultSig)) (c :: (Maybe InjectivityInfo)) :: Decl
pattern ClosedTypeFamDecl a b c d = H.ClosedTypeFamDecl () (a :: DeclHead) (b :: (Maybe ResultSig)) (c :: (Maybe InjectivityInfo)) (d :: [TypeEqn]) :: Decl
pattern DataDecl a b c d e = H.DataDecl () (a :: DataOrNew) (b :: (Maybe Context)) (c :: DeclHead) (d :: [QualConDecl]) (e :: [Deriving]) :: Decl
pattern GDataDecl a b c d e f = H.GDataDecl () (a :: DataOrNew) (b :: (Maybe Context)) (c :: DeclHead) (d :: (Maybe Kind)) (e :: [GadtDecl]) (f :: [Deriving]) :: Decl
pattern DataFamDecl a b c = H.DataFamDecl () (a :: (Maybe Context)) (b :: DeclHead) (c :: (Maybe ResultSig)) :: Decl
pattern TypeInsDecl a b = H.TypeInsDecl () (a :: Type) (b :: Type) :: Decl
pattern DataInsDecl a b c d = H.DataInsDecl () (a :: DataOrNew) (b :: Type) (c :: [QualConDecl]) (d :: [Deriving]) :: Decl
pattern GDataInsDecl a b c d e = H.GDataInsDecl () (a :: DataOrNew) (b :: Type) (c :: (Maybe Kind)) (d :: [GadtDecl]) (e :: [Deriving]) :: Decl
pattern ClassDecl a b c d = H.ClassDecl () (a :: (Maybe Context)) (b :: DeclHead) (c :: [FunDep]) (d :: (Maybe [ClassDecl])) :: Decl
pattern InstDecl a b c = H.InstDecl () (a :: (Maybe Overlap)) (b :: InstRule) (c :: (Maybe [InstDecl])) :: Decl
pattern DerivDecl a b c = H.DerivDecl () (a :: (Maybe DerivStrategy)) (b :: (Maybe Overlap)) (c :: InstRule) :: Decl
pattern InfixDecl a b c = H.InfixDecl () (a :: Assoc) (b :: (Maybe Int)) (c :: [Op]) :: Decl
pattern DefaultDecl a = H.DefaultDecl () (a :: [Type]) :: Decl
pattern SpliceDecl a = H.SpliceDecl () (a :: Exp) :: Decl
pattern TSpliceDecl a = H.TSpliceDecl () (a :: Exp) :: Decl
pattern TypeSig a b = H.TypeSig () (a :: [Name]) (b :: Type) :: Decl
pattern PatSynSig a b c d e f = H.PatSynSig () (a :: [Name]) (b :: (Maybe [TyVarBind])) (c :: (Maybe Context)) (d :: (Maybe [TyVarBind])) (e :: (Maybe Context)) (f :: Type) :: Decl
pattern FunBind a = H.FunBind () (a :: [Match]) :: Decl
pattern PatBind a b c = H.PatBind () (a :: Pat) (b :: Rhs) (c :: (Maybe Binds)) :: Decl
pattern PatSyn a b c = H.PatSyn () (a :: Pat) (b :: Pat) (c :: PatternSynDirection) :: Decl
pattern ForImp a b c d e = H.ForImp () (a :: CallConv) (b :: (Maybe Safety)) (c :: (Maybe String)) (d :: Name) (e :: Type) :: Decl
pattern ForExp a b c d = H.ForExp () (a :: CallConv) (b :: (Maybe String)) (c :: Name) (d :: Type) :: Decl
pattern RulePragmaDecl a = H.RulePragmaDecl () (a :: [Rule]) :: Decl
pattern DeprPragmaDecl a = H.DeprPragmaDecl () (a :: [([Name], String)]) :: Decl
pattern WarnPragmaDecl a = H.WarnPragmaDecl () (a :: [([Name], String)]) :: Decl
pattern InlineSig a b c = H.InlineSig () (a :: Bool) (b :: (Maybe Activation)) (c :: QName) :: Decl
pattern InlineConlikeSig a b = H.InlineConlikeSig () (a :: (Maybe Activation)) (b :: QName) :: Decl
pattern SpecSig a b c = H.SpecSig () (a :: (Maybe Activation)) (b :: QName) (c :: [Type]) :: Decl
pattern SpecInlineSig a b c d = H.SpecInlineSig () (a :: Bool) (b :: (Maybe Activation)) (c :: QName) (d :: [Type]) :: Decl
pattern InstSig a = H.InstSig () (a :: InstRule) :: Decl
pattern AnnPragma a = H.AnnPragma () (a :: Annotation) :: Decl
pattern MinimalPragma a = H.MinimalPragma () (a :: (Maybe BooleanFormula)) :: Decl
pattern RoleAnnotDecl a b = H.RoleAnnotDecl () (a :: QName) (b :: [Role]) :: Decl
pattern CompletePragma a b = H.CompletePragma () (a :: [Name]) (b :: (Maybe QName)) :: Decl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE TypeDecl, TypeFamDecl, ClosedTypeFamDecl, DataDecl, GDataDecl,
             DataFamDecl, TypeInsDecl, DataInsDecl, GDataInsDecl, ClassDecl, InstDecl,
             DerivDecl, InfixDecl, DefaultDecl, SpliceDecl, TSpliceDecl, TypeSig,
             PatSynSig, FunBind, PatBind, PatSyn, ForImp, ForExp, RulePragmaDecl,
             DeprPragmaDecl, WarnPragmaDecl, InlineSig, InlineConlikeSig, SpecSig,
             SpecInlineSig, InstSig, AnnPragma, MinimalPragma, RoleAnnotDecl,
             CompletePragma #-}
#endif

-- ** `H.PatternSynDirection`
type PatternSynDirection = H.PatternSynDirection ()
pattern Unidirectional = H.Unidirectional :: PatternSynDirection
pattern ImplicitBidirectional = H.ImplicitBidirectional :: PatternSynDirection
pattern ExplicitBidirectional a = H.ExplicitBidirectional () (a :: [Decl]) :: PatternSynDirection

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Unidirectional, ImplicitBidirectional, ExplicitBidirectional #-}
#endif

-- ** `H.TypeEqn`
type TypeEqn = H.TypeEqn ()
pattern TypeEqn a b = H.TypeEqn () (a :: Type) (b :: Type) :: TypeEqn

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE TypeEqn #-}
#endif

-- ** `H.Annotation`
type Annotation = H.Annotation ()
pattern Ann a b = H.Ann () (a :: Name) (b :: Exp) :: Annotation
pattern TypeAnn a b = H.TypeAnn () (a :: Name) (b :: Exp) :: Annotation
pattern ModuleAnn a = H.ModuleAnn () (a :: Exp) :: Annotation

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Ann, TypeAnn, ModuleAnn #-}
#endif

-- ** `H.BooleanFormula`
type BooleanFormula = H.BooleanFormula ()
pattern VarFormula a = H.VarFormula () (a :: Name) :: BooleanFormula
pattern AndFormula a = H.AndFormula () (a :: [BooleanFormula]) :: BooleanFormula
pattern OrFormula a = H.OrFormula () (a :: [BooleanFormula]) :: BooleanFormula
pattern ParenFormula a = H.ParenFormula () (a :: BooleanFormula) :: BooleanFormula

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE VarFormula, AndFormula, OrFormula, ParenFormula #-}
#endif

-- ** `H.Role`
type Role = H.Role ()
pattern Nominal = H.Nominal () :: Role
pattern Representational = H.Representational () :: Role
pattern Phantom = H.Phantom () :: Role
pattern RoleWildcard = H.RoleWildcard () :: Role

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Nominal, Representational, Phantom, RoleWildcard #-}
#endif

-- ** `H.DataOrNew`
type DataOrNew = H.DataOrNew ()
pattern DataType = H.DataType () :: DataOrNew
pattern NewType = H.NewType () :: DataOrNew

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE DataType, NewType #-}
#endif

-- ** `H.InjectivityInfo`
type InjectivityInfo = H.InjectivityInfo ()
pattern InjectivityInfo a b = H.InjectivityInfo () (a :: Name) (b :: [Name]) :: InjectivityInfo

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE InjectivityInfo #-}
#endif

-- ** `H.ResultSig`
type ResultSig = H.ResultSig ()
pattern KindSig a = H.KindSig () (a :: Kind) :: ResultSig
pattern TyVarSig a = H.TyVarSig () (a :: TyVarBind) :: ResultSig

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE KindSig, TyVarSig #-}
#endif

-- ** `H.DeclHead`
type DeclHead = H.DeclHead ()
pattern DHead a = H.DHead () (a :: Name) :: DeclHead
pattern DHInfix a b = H.DHInfix () (a :: TyVarBind) (b :: Name) :: DeclHead
pattern DHParen a = H.DHParen () (a :: DeclHead) :: DeclHead
pattern DHApp a b = H.DHApp () (a :: DeclHead) (b :: TyVarBind) :: DeclHead

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE DHead, DHInfix, DHParen, DHApp #-}
#endif

-- ** `H.InstRule`
type InstRule = H.InstRule ()
pattern IRule a b c = H.IRule () (a :: (Maybe [TyVarBind])) (b :: (Maybe Context)) (c :: InstHead) :: InstRule
pattern IParen a = H.IParen () (a :: InstRule) :: InstRule

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE IRule, IParen #-}
#endif

-- ** `H.InstHead`
type InstHead = H.InstHead ()
pattern IHCon a = H.IHCon () (a :: QName) :: InstHead
pattern IHInfix a b = H.IHInfix () (a :: Type) (b :: QName) :: InstHead
pattern IHParen a = H.IHParen () (a :: InstHead) :: InstHead
pattern IHApp a b = H.IHApp () (a :: InstHead) (b :: Type) :: InstHead

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE IHCon, IHInfix, IHParen, IHApp #-}
#endif

-- ** `H.Deriving`
type Deriving = H.Deriving ()
pattern Deriving a b = H.Deriving () (a :: (Maybe DerivStrategy)) (b :: [InstRule]) :: Deriving

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Deriving #-}
#endif

-- ** `H.DerivStrategy`
type DerivStrategy = H.DerivStrategy ()
pattern DerivStock = H.DerivStock () :: DerivStrategy
pattern DerivAnyclass = H.DerivAnyclass () :: DerivStrategy
pattern DerivNewtype = H.DerivNewtype () :: DerivStrategy
pattern DerivVia a = H.DerivVia () (a :: Type) :: DerivStrategy

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE DerivStock, DerivAnyclass, DerivNewtype, DerivVia #-}
#endif

-- ** `H.Binds`
type Binds = H.Binds ()
pattern BDecls a = H.BDecls () (a :: [Decl]) :: Binds
pattern IPBinds a = H.IPBinds () (a :: [IPBind]) :: Binds

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE BDecls, IPBinds #-}
#endif

-- ** `H.IPBind`
type IPBind = H.IPBind ()
pattern IPBind a b = H.IPBind () (a :: IPName) (b :: Exp) :: IPBind

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE IPBind #-}
#endif

-- ** `H.Match`
type Match = H.Match ()
pattern Match a b c d = H.Match () (a :: Name) (b :: [Pat]) (c :: Rhs) (d :: (Maybe Binds)) :: Match
pattern InfixMatch a b c d e = H.InfixMatch () (a :: Pat) (b :: Name) (c :: [Pat]) (d :: Rhs) (e :: (Maybe Binds)) :: Match

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Match, InfixMatch #-}
#endif

-- ** `H.QualConDecl`
type QualConDecl = H.QualConDecl ()
pattern QualConDecl a b c = H.QualConDecl () (a :: (Maybe [TyVarBind])) (b :: (Maybe Context)) (c :: ConDecl) :: QualConDecl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE QualConDecl #-}
#endif

-- ** `H.ConDecl`
type ConDecl = H.ConDecl ()
pattern ConDecl a b = H.ConDecl () (a :: Name) (b :: [Type]) :: ConDecl
pattern InfixConDecl a b c = H.InfixConDecl () (a :: Type) (b :: Name) (c :: Type) :: ConDecl
pattern RecDecl a b = H.RecDecl () (a :: Name) (b :: [FieldDecl]) :: ConDecl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ConDecl, InfixConDecl, RecDecl #-}
#endif

-- ** `H.FieldDecl`
type FieldDecl = H.FieldDecl ()
pattern FieldDecl a b = H.FieldDecl () (a :: [Name]) (b :: Type) :: FieldDecl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE FieldDecl #-}
#endif

-- ** `H.GadtDecl`
type GadtDecl = H.GadtDecl ()
pattern GadtDecl a b c d e = H.GadtDecl () (a :: Name) (b :: (Maybe [TyVarBind])) (c :: (Maybe Context)) (d :: (Maybe [FieldDecl])) (e :: Type) :: GadtDecl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE GadtDecl #-}
#endif

-- ** `H.ClassDecl`
type ClassDecl = H.ClassDecl ()
pattern ClsDecl a = H.ClsDecl () (a :: Decl) :: ClassDecl
pattern ClsDataFam a b c = H.ClsDataFam () (a :: (Maybe Context)) (b :: DeclHead) (c :: (Maybe ResultSig)) :: ClassDecl
pattern ClsTyFam a b c = H.ClsTyFam () (a :: DeclHead) (b :: (Maybe ResultSig)) (c :: (Maybe InjectivityInfo)) :: ClassDecl
pattern ClsTyDef a = H.ClsTyDef () (a :: TypeEqn) :: ClassDecl
pattern ClsDefSig a b = H.ClsDefSig () (a :: Name) (b :: Type) :: ClassDecl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ClsDecl, ClsDataFam, ClsTyFam, ClsTyDef, ClsDefSig #-}
#endif

-- ** `H.InstDecl`
type InstDecl = H.InstDecl ()
pattern InsDecl a = H.InsDecl () (a :: Decl) :: InstDecl
pattern InsType a b = H.InsType () (a :: Type) (b :: Type) :: InstDecl
pattern InsData a b c d = H.InsData () (a :: DataOrNew) (b :: Type) (c :: [QualConDecl]) (d :: [Deriving]) :: InstDecl
pattern InsGData a b c d e = H.InsGData () (a :: DataOrNew) (b :: Type) (c :: (Maybe Kind)) (d :: [GadtDecl]) (e :: [Deriving]) :: InstDecl

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE InsDecl, InsType, InsData, InsGData #-}
#endif

-- ** `H.BangType`
type BangType = H.BangType ()
pattern BangedTy = H.BangedTy () :: BangType
pattern LazyTy = H.LazyTy () :: BangType
pattern NoStrictAnnot = H.NoStrictAnnot () :: BangType

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE BangedTy, LazyTy, NoStrictAnnot #-}
#endif

-- ** `H.Unpackedness`
type Unpackedness = H.Unpackedness ()
pattern Unpack = H.Unpack () :: Unpackedness
pattern NoUnpack = H.NoUnpack () :: Unpackedness
pattern NoUnpackPragma = H.NoUnpackPragma () :: Unpackedness

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Unpack, NoUnpack, NoUnpackPragma #-}
#endif

-- ** `H.Rhs`
type Rhs = H.Rhs ()
pattern UnGuardedRhs a = H.UnGuardedRhs () (a :: Exp) :: Rhs
pattern GuardedRhss a = H.GuardedRhss () (a :: [GuardedRhs]) :: Rhs

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE UnGuardedRhs, GuardedRhss #-}
#endif

-- ** `H.GuardedRhs`
type GuardedRhs = H.GuardedRhs ()
pattern GuardedRhs a b = H.GuardedRhs () (a :: [Stmt]) (b :: Exp) :: GuardedRhs

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE GuardedRhs #-}
#endif

-- ** `H.Type`
type Type = H.Type ()
pattern TyForall a b c = H.TyForall () (a :: (Maybe [TyVarBind])) (b :: (Maybe Context)) (c :: Type) :: Type
pattern TyStar = H.TyStar () :: Type
pattern TyFun a b = H.TyFun () (a :: Type) (b :: Type) :: Type
pattern TyTuple a b = H.TyTuple () (a :: Boxed) (b :: [Type]) :: Type
pattern TyUnboxedSum a = H.TyUnboxedSum () (a :: [Type]) :: Type
pattern TyList a = H.TyList () (a :: Type) :: Type
pattern TyParArray a = H.TyParArray () (a :: Type) :: Type
pattern TyApp a b = H.TyApp () (a :: Type) (b :: Type) :: Type
pattern TyVar a = H.TyVar () (a :: Name) :: Type
pattern TyCon a = H.TyCon () (a :: QName) :: Type
pattern TyParen a = H.TyParen () (a :: Type) :: Type
pattern TyInfix a b c = H.TyInfix () (a :: Type) (b :: MaybePromotedName) (c :: Type) :: Type
pattern TyKind a b = H.TyKind () (a :: Type) (b :: Kind) :: Type
pattern TyPromoted a = H.TyPromoted () (a :: Promoted) :: Type
pattern TyEquals a b = H.TyEquals () (a :: Type) (b :: Type) :: Type
pattern TySplice a = H.TySplice () (a :: Splice) :: Type
pattern TyBang a b c = H.TyBang () (a :: BangType) (b :: Unpackedness) (c :: Type) :: Type
pattern TyWildCard a = H.TyWildCard () (a :: (Maybe Name)) :: Type
pattern TyQuasiQuote a b = H.TyQuasiQuote () (a :: String) (b :: String) :: Type

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE TyForall, TyStar, TyFun, TyTuple, TyUnboxedSum, TyList, TyParArray, TyApp,
             TyVar, TyCon, TyParen, TyInfix, TyKind, TyPromoted, TyEquals, TySplice,
             TyBang, TyWildCard, TyQuasiQuote #-}
#endif

-- ** `H.MaybePromotedName`
type MaybePromotedName = H.MaybePromotedName ()
pattern PromotedName a = H.PromotedName () (a :: QName) :: MaybePromotedName
pattern UnpromotedName a = H.UnpromotedName () (a :: QName) :: MaybePromotedName

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE PromotedName, UnpromotedName #-}
#endif

-- ** `H.Promoted`
type Promoted = H.Promoted ()
pattern PromotedInteger a b = H.PromotedInteger () (a :: Integer) (b :: String) :: Promoted
pattern PromotedString a b = H.PromotedString () (a :: String) (b :: String) :: Promoted
pattern PromotedCon a b = H.PromotedCon () (a :: Bool) (b :: QName) :: Promoted
pattern PromotedList a b = H.PromotedList () (a :: Bool) (b :: [Type]) :: Promoted
pattern PromotedTuple a = H.PromotedTuple () (a :: [Type]) :: Promoted
pattern PromotedUnit = H.PromotedUnit () :: Promoted

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE PromotedInteger, PromotedString, PromotedCon, PromotedList, PromotedTuple,
             PromotedUnit #-}
#endif

-- skipped: data Boxed

-- ** `H.TyVarBind`
type TyVarBind = H.TyVarBind ()
pattern KindedVar a b = H.KindedVar () (a :: Name) (b :: Kind) :: TyVarBind
pattern UnkindedVar a = H.UnkindedVar () (a :: Name) :: TyVarBind

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE KindedVar, UnkindedVar #-}
#endif

-- ** `H.Kind`

-- | Note that `Kind` is an alias for `Type` since haskell-src-exts-1.21.
type Kind = H.Kind ()

-- ** `H.FunDep`
type FunDep = H.FunDep ()
pattern FunDep a b = H.FunDep () (a :: [Name]) (b :: [Name]) :: FunDep

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE FunDep #-}
#endif

-- ** `H.Context`
type Context = H.Context ()
pattern CxSingle a = H.CxSingle () (a :: Asst) :: Context
pattern CxTuple a = H.CxTuple () (a :: [Asst]) :: Context
pattern CxEmpty = H.CxEmpty () :: Context

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE CxSingle, CxTuple, CxEmpty #-}
#endif

-- ** `H.Asst`
type Asst = H.Asst ()
pattern TypeA a = H.TypeA () (a :: Type) :: Asst
pattern IParam a b = H.IParam () (a :: IPName) (b :: Type) :: Asst
pattern ParenA a = H.ParenA () (a :: Asst) :: Asst

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE TypeA, IParam, ParenA #-}
#endif

-- ** `H.Literal`
type Literal = H.Literal ()

pattern Char a <- H.Char () (a :: Char) _ :: Literal
    where Char a = H.Char () a [a]
charL :: Char -> Literal
charL a = H.Char () a [a]

pattern String a <- H.String () (a :: String) _ :: Literal
    where String a = stringL a
stringL :: String -> Literal
stringL a = H.String () a a

pattern Int a <- H.Int () (a :: Integer) _ :: Literal
    where Int a = intL a
intL :: Integer -> Literal
intL a = H.Int () a (show a)

pattern Frac a <- H.Frac () (a :: Rational) _ :: Literal
    where Frac a = fracL a
fracL :: Rational -> Literal
fracL a = H.Frac () a (show a)

pattern PrimInt a <- H.PrimInt () (a :: Integer) _ :: Literal
    where PrimInt a = primIntL a
primIntL :: Integer -> Literal
primIntL a = H.PrimInt () a (show a)

pattern PrimWord a <- H.PrimWord () (a :: Integer) _ :: Literal
    where PrimWord a = primWordL a
primWordL :: Integer -> Literal
primWordL a = H.PrimWord () a (show a)

pattern PrimFloat a <- H.PrimFloat () (a :: Rational) _ :: Literal
    where PrimFloat a = primFloatL a
primFloatL :: Rational -> Literal
primFloatL a = H.PrimFloat () a (show (fromRational a :: Float))

pattern PrimDouble a <- H.PrimDouble () (a :: Rational) _ :: Literal
    where PrimDouble a = primDoubleL a
primDoubleL :: Rational -> Literal
primDoubleL a = H.PrimDouble () a (show (fromRational a :: Double))

pattern PrimChar a <- H.PrimChar () (a :: Char) _ :: Literal
    where PrimChar a = primCharL a
primCharL :: Char -> Literal
primCharL a = H.PrimChar () a [a]

pattern PrimString a <- H.PrimString () (a :: String) _ :: Literal
    where PrimString a = primStringL a
primStringL :: String -> Literal
primStringL a = H.PrimString () a a

#undef where

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Char, String, Int, Frac, PrimInt, PrimWord, PrimFloat, PrimDouble,
             PrimChar, PrimString #-}
#endif

-- ** `H.Sign`
type Sign = H.Sign ()
pattern Signless = H.Signless () :: Sign
pattern Negative = H.Negative () :: Sign

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Signless, Negative #-}
#endif

-- ** `H.Exp`
type Exp = H.Exp ()
pattern Var a = H.Var () (a :: QName) :: Exp
pattern OverloadedLabel a = H.OverloadedLabel () (a :: String) :: Exp
pattern IPVar a = H.IPVar () (a :: IPName) :: Exp
pattern Con a = H.Con () (a :: QName) :: Exp
pattern Lit a = H.Lit () (a :: Literal) :: Exp
pattern InfixApp a b c = H.InfixApp () (a :: Exp) (b :: QOp) (c :: Exp) :: Exp
pattern App a b = H.App () (a :: Exp) (b :: Exp) :: Exp
pattern NegApp a = H.NegApp () (a :: Exp) :: Exp
pattern Lambda a b = H.Lambda () (a :: [Pat]) (b :: Exp) :: Exp
pattern Let a b = H.Let () (a :: Binds) (b :: Exp) :: Exp
pattern If a b c = H.If () (a :: Exp) (b :: Exp) (c :: Exp) :: Exp
pattern MultiIf a = H.MultiIf () (a :: [GuardedRhs]) :: Exp
pattern Case a b = H.Case () (a :: Exp) (b :: [Alt]) :: Exp
pattern Do a = H.Do () (a :: [Stmt]) :: Exp
pattern MDo a = H.MDo () (a :: [Stmt]) :: Exp
pattern Tuple a b = H.Tuple () (a :: Boxed) (b :: [Exp]) :: Exp
pattern UnboxedSum a b c = H.UnboxedSum () (a :: Int) (b :: Int) (c :: Exp) :: Exp
pattern TupleSection a b = H.TupleSection () (a :: Boxed) (b :: [Maybe Exp]) :: Exp
pattern List a = H.List () (a :: [Exp]) :: Exp
pattern ParArray a = H.ParArray () (a :: [Exp]) :: Exp
pattern Paren a = H.Paren () (a :: Exp) :: Exp
pattern LeftSection a b = H.LeftSection () (a :: Exp) (b :: QOp) :: Exp
pattern RightSection a b = H.RightSection () (a :: QOp) (b :: Exp) :: Exp
pattern RecConstr a b = H.RecConstr () (a :: QName) (b :: [FieldUpdate]) :: Exp
pattern RecUpdate a b = H.RecUpdate () (a :: Exp) (b :: [FieldUpdate]) :: Exp
pattern EnumFrom a = H.EnumFrom () (a :: Exp) :: Exp
pattern EnumFromTo a b = H.EnumFromTo () (a :: Exp) (b :: Exp) :: Exp
pattern EnumFromThen a b = H.EnumFromThen () (a :: Exp) (b :: Exp) :: Exp
pattern EnumFromThenTo a b c = H.EnumFromThenTo () (a :: Exp) (b :: Exp) (c :: Exp) :: Exp
pattern ParArrayFromTo a b = H.ParArrayFromTo () (a :: Exp) (b :: Exp) :: Exp
pattern ParArrayFromThenTo a b c = H.ParArrayFromThenTo () (a :: Exp) (b :: Exp) (c :: Exp) :: Exp
pattern ListComp a b = H.ListComp () (a :: Exp) (b :: [QualStmt]) :: Exp
pattern ParComp a b = H.ParComp () (a :: Exp) (b :: [[QualStmt]]) :: Exp
pattern ParArrayComp a b = H.ParArrayComp () (a :: Exp) (b :: [[QualStmt]]) :: Exp
pattern ExpTypeSig a b = H.ExpTypeSig () (a :: Exp) (b :: Type) :: Exp
pattern VarQuote a = H.VarQuote () (a :: QName) :: Exp
pattern TypQuote a = H.TypQuote () (a :: QName) :: Exp
pattern BracketExp a = H.BracketExp () (a :: Bracket) :: Exp
pattern SpliceExp a = H.SpliceExp () (a :: Splice) :: Exp
pattern QuasiQuote a b = H.QuasiQuote () (a :: String) (b :: String) :: Exp
pattern TypeApp a = H.TypeApp () (a :: Type) :: Exp
pattern XTag a b c d = H.XTag () (a :: XName) (b :: [XAttr]) (c :: (Maybe Exp)) (d :: [Exp]) :: Exp
pattern XETag a b c = H.XETag () (a :: XName) (b :: [XAttr]) (c :: (Maybe Exp)) :: Exp
pattern XPcdata a = H.XPcdata () (a :: String) :: Exp
pattern XExpTag a = H.XExpTag () (a :: Exp) :: Exp
pattern XChildTag a = H.XChildTag () (a :: [Exp]) :: Exp
pattern CorePragma a b = H.CorePragma () (a :: String) (b :: Exp) :: Exp
pattern SCCPragma a b = H.SCCPragma () (a :: String) (b :: Exp) :: Exp
pattern GenPragma a b c d = H.GenPragma () (a :: String) (b :: (Int, Int)) (c :: (Int, Int)) (d :: Exp) :: Exp
pattern Proc a b = H.Proc () (a :: Pat) (b :: Exp) :: Exp
pattern LeftArrApp a b = H.LeftArrApp () (a :: Exp) (b :: Exp) :: Exp
pattern RightArrApp a b = H.RightArrApp () (a :: Exp) (b :: Exp) :: Exp
pattern LeftArrHighApp a b = H.LeftArrHighApp () (a :: Exp) (b :: Exp) :: Exp
pattern RightArrHighApp a b = H.RightArrHighApp () (a :: Exp) (b :: Exp) :: Exp
pattern ArrOp a = H.ArrOp () (a :: Exp) :: Exp
pattern LCase a = H.LCase () (a :: [Alt]) :: Exp

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Var, OverloadedLabel, IPVar, Con, Lit, InfixApp, App, NegApp, Lambda, Let,
             If, MultiIf, Case, Do, MDo, Tuple, UnboxedSum, TupleSection, List,
             ParArray, Paren, LeftSection, RightSection, RecConstr, RecUpdate,
             EnumFrom, EnumFromTo, EnumFromThen, EnumFromThenTo, ParArrayFromTo,
             ParArrayFromThenTo, ListComp, ParComp, ParArrayComp, ExpTypeSig, VarQuote,
             TypQuote, BracketExp, SpliceExp, QuasiQuote, TypeApp, XTag, XETag,
             XPcdata, XExpTag, XChildTag, CorePragma, SCCPragma, GenPragma, Proc,
             LeftArrApp, RightArrApp, LeftArrHighApp, RightArrHighApp, ArrOp, LCase #-}
#endif

-- ** `H.XName`
type XName = H.XName ()
pattern XName a = H.XName () (a :: String) :: XName
pattern XDomName a b = H.XDomName () (a :: String) (b :: String) :: XName

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE XName, XDomName #-}
#endif

-- ** `H.XAttr`
type XAttr = H.XAttr ()
pattern XAttr a b = H.XAttr () (a :: XName) (b :: Exp) :: XAttr

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE XAttr #-}
#endif

-- ** `H.Bracket`
type Bracket = H.Bracket ()
pattern ExpBracket a = H.ExpBracket () (a :: Exp) :: Bracket
pattern TExpBracket a = H.TExpBracket () (a :: Exp) :: Bracket
pattern PatBracket a = H.PatBracket () (a :: Pat) :: Bracket
pattern TypeBracket a = H.TypeBracket () (a :: Type) :: Bracket
pattern DeclBracket a = H.DeclBracket () (a :: [Decl]) :: Bracket

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ExpBracket, TExpBracket, PatBracket, TypeBracket, DeclBracket #-}
#endif

-- ** `H.Splice`
type Splice = H.Splice ()
pattern IdSplice a = H.IdSplice () (a :: String) :: Splice
pattern TIdSplice a = H.TIdSplice () (a :: String) :: Splice
pattern ParenSplice a = H.ParenSplice () (a :: Exp) :: Splice
pattern TParenSplice a = H.TParenSplice () (a :: Exp) :: Splice

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE IdSplice, TIdSplice, ParenSplice, TParenSplice #-}
#endif

-- ** `H.Safety`
type Safety = H.Safety ()
pattern PlayRisky = H.PlayRisky () :: Safety
pattern PlaySafe a = H.PlaySafe () (a :: Bool) :: Safety
pattern PlayInterruptible = H.PlayInterruptible () :: Safety

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE PlayRisky, PlaySafe, PlayInterruptible #-}
#endif

-- ** `H.CallConv`
type CallConv = H.CallConv ()
pattern StdCall = H.StdCall () :: CallConv
pattern CCall = H.CCall () :: CallConv
pattern CPlusPlus = H.CPlusPlus () :: CallConv
pattern DotNet = H.DotNet () :: CallConv
pattern Jvm = H.Jvm () :: CallConv
pattern Js = H.Js () :: CallConv
pattern JavaScript = H.JavaScript () :: CallConv
pattern CApi = H.CApi () :: CallConv

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE StdCall, CCall, CPlusPlus, DotNet, Jvm, Js, JavaScript, CApi #-}
#endif

-- ** `H.ModulePragma`
type ModulePragma = H.ModulePragma ()
pattern LanguagePragma a = H.LanguagePragma () (a :: [Name]) :: ModulePragma
pattern OptionsPragma a b = H.OptionsPragma () (a :: (Maybe Tool)) (b :: String) :: ModulePragma
pattern AnnModulePragma a = H.AnnModulePragma () (a :: Annotation) :: ModulePragma

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE LanguagePragma, OptionsPragma, AnnModulePragma #-}
#endif

-- skipped: data Tool

-- ** `H.Overlap`
type Overlap = H.Overlap ()
pattern NoOverlap = H.NoOverlap () :: Overlap
pattern Overlap = H.Overlap () :: Overlap
pattern Overlapping = H.Overlapping () :: Overlap
pattern Overlaps = H.Overlaps () :: Overlap
pattern Overlappable = H.Overlappable () :: Overlap
pattern Incoherent = H.Incoherent () :: Overlap

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE NoOverlap, Overlap, Overlapping, Overlaps, Overlappable, Incoherent #-}
#endif

-- ** `H.Activation`
type Activation = H.Activation ()
pattern ActiveFrom a = H.ActiveFrom () (a :: Int) :: Activation
pattern ActiveUntil a = H.ActiveUntil () (a :: Int) :: Activation

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE ActiveFrom, ActiveUntil #-}
#endif

-- ** `H.Rule`
type Rule = H.Rule ()
pattern Rule a b c d e = H.Rule () (a :: String) (b :: (Maybe Activation)) (c :: (Maybe [RuleVar])) (d :: Exp) (e :: Exp) :: Rule

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Rule #-}
#endif

-- ** `H.RuleVar`
type RuleVar = H.RuleVar ()
pattern RuleVar a = H.RuleVar () (a :: Name) :: RuleVar
pattern TypedRuleVar a b = H.TypedRuleVar () (a :: Name) (b :: Type) :: RuleVar

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE RuleVar, TypedRuleVar #-}
#endif

-- ** `H.WarningText`
type WarningText = H.WarningText ()
pattern DeprText a = H.DeprText () (a :: String) :: WarningText
pattern WarnText a = H.WarnText () (a :: String) :: WarningText

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE DeprText, WarnText #-}
#endif

-- ** `H.Pat`
type Pat = H.Pat ()
pattern PVar a = H.PVar () (a :: Name) :: Pat
pattern PLit a b = H.PLit () (a :: Sign) (b :: Literal) :: Pat
pattern PNPlusK a b = H.PNPlusK () (a :: Name) (b :: Integer) :: Pat
pattern PInfixApp a b c = H.PInfixApp () (a :: Pat) (b :: QName) (c :: Pat) :: Pat
pattern PApp a b = H.PApp () (a :: QName) (b :: [Pat]) :: Pat
pattern PTuple a b = H.PTuple () (a :: Boxed) (b :: [Pat]) :: Pat
pattern PUnboxedSum a b c = H.PUnboxedSum () (a :: Int) (b :: Int) (c :: Pat) :: Pat
pattern PList a = H.PList () (a :: [Pat]) :: Pat
pattern PParen a = H.PParen () (a :: Pat) :: Pat
pattern PRec a b = H.PRec () (a :: QName) (b :: [PatField]) :: Pat
pattern PAsPat a b = H.PAsPat () (a :: Name) (b :: Pat) :: Pat
pattern PWildCard = H.PWildCard () :: Pat
pattern PIrrPat a = H.PIrrPat () (a :: Pat) :: Pat
pattern PatTypeSig a b = H.PatTypeSig () (a :: Pat) (b :: Type) :: Pat
pattern PViewPat a b = H.PViewPat () (a :: Exp) (b :: Pat) :: Pat
pattern PRPat a = H.PRPat () (a :: [RPat]) :: Pat
pattern PXTag a b c d = H.PXTag () (a :: XName) (b :: [PXAttr]) (c :: (Maybe Pat)) (d :: [Pat]) :: Pat
pattern PXETag a b c = H.PXETag () (a :: XName) (b :: [PXAttr]) (c :: (Maybe Pat)) :: Pat
pattern PXPcdata a = H.PXPcdata () (a :: String) :: Pat
pattern PXPatTag a = H.PXPatTag () (a :: Pat) :: Pat
pattern PXRPats a = H.PXRPats () (a :: [RPat]) :: Pat
pattern PSplice a = H.PSplice () (a :: Splice) :: Pat
pattern PQuasiQuote a b = H.PQuasiQuote () (a :: String) (b :: String) :: Pat
pattern PBangPat a = H.PBangPat () (a :: Pat) :: Pat

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE PVar, PLit, PNPlusK, PInfixApp, PApp, PTuple, PUnboxedSum, PList, PParen,
             PRec, PAsPat, PWildCard, PIrrPat, PatTypeSig, PViewPat, PRPat, PXTag,
             PXETag, PXPcdata, PXPatTag, PXRPats, PSplice, PQuasiQuote, PBangPat #-}
#endif

-- ** `H.PXAttr`
type PXAttr = H.PXAttr ()
pattern PXAttr a b = H.PXAttr () (a :: XName) (b :: Pat) :: PXAttr

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE PXAttr #-}
#endif

-- ** `H.RPatOp`
type RPatOp = H.RPatOp ()
pattern RPStar = H.RPStar () :: RPatOp
pattern RPStarG = H.RPStarG () :: RPatOp
pattern RPPlus = H.RPPlus () :: RPatOp
pattern RPPlusG = H.RPPlusG () :: RPatOp
pattern RPOpt = H.RPOpt () :: RPatOp
pattern RPOptG = H.RPOptG () :: RPatOp

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE RPStar, RPStarG, RPPlus, RPPlusG, RPOpt, RPOptG #-}
#endif

-- ** `H.RPat`
type RPat = H.RPat ()
pattern RPOp a b = H.RPOp () (a :: RPat) (b :: RPatOp) :: RPat
pattern RPEither a b = H.RPEither () (a :: RPat) (b :: RPat) :: RPat
pattern RPSeq a = H.RPSeq () (a :: [RPat]) :: RPat
pattern RPGuard a b = H.RPGuard () (a :: Pat) (b :: [Stmt]) :: RPat
pattern RPCAs a b = H.RPCAs () (a :: Name) (b :: RPat) :: RPat
pattern RPAs a b = H.RPAs () (a :: Name) (b :: RPat) :: RPat
pattern RPParen a = H.RPParen () (a :: RPat) :: RPat
pattern RPPat a = H.RPPat () (a :: Pat) :: RPat

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE RPOp, RPEither, RPSeq, RPGuard, RPCAs, RPAs, RPParen, RPPat #-}
#endif

-- ** `H.PatField`
type PatField = H.PatField ()
pattern PFieldPat a b = H.PFieldPat () (a :: QName) (b :: Pat) :: PatField
pattern PFieldPun a = H.PFieldPun () (a :: QName) :: PatField
pattern PFieldWildcard = H.PFieldWildcard () :: PatField

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE PFieldPat, PFieldPun, PFieldWildcard #-}
#endif

-- ** `H.Stmt`
type Stmt = H.Stmt ()
pattern Generator a b = H.Generator () (a :: Pat) (b :: Exp) :: Stmt
pattern Qualifier a = H.Qualifier () (a :: Exp) :: Stmt
pattern LetStmt a = H.LetStmt () (a :: Binds) :: Stmt
pattern RecStmt a = H.RecStmt () (a :: [Stmt]) :: Stmt

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Generator, Qualifier, LetStmt, RecStmt #-}
#endif

-- ** `H.QualStmt`
type QualStmt = H.QualStmt ()
pattern QualStmt a = H.QualStmt () (a :: Stmt) :: QualStmt
pattern ThenTrans a = H.ThenTrans () (a :: Exp) :: QualStmt
pattern ThenBy a b = H.ThenBy () (a :: Exp) (b :: Exp) :: QualStmt
pattern GroupBy a = H.GroupBy () (a :: Exp) :: QualStmt
pattern GroupUsing a = H.GroupUsing () (a :: Exp) :: QualStmt
pattern GroupByUsing a b = H.GroupByUsing () (a :: Exp) (b :: Exp) :: QualStmt

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE QualStmt, ThenTrans, ThenBy, GroupBy, GroupUsing, GroupByUsing #-}
#endif

-- ** `H.FieldUpdate`
type FieldUpdate = H.FieldUpdate ()
pattern FieldUpdate a b = H.FieldUpdate () (a :: QName) (b :: Exp) :: FieldUpdate
pattern FieldPun a = H.FieldPun () (a :: QName) :: FieldUpdate
pattern FieldWildcard = H.FieldWildcard () :: FieldUpdate

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE FieldUpdate, FieldPun, FieldWildcard #-}
#endif

-- ** `H.Alt`
type Alt = H.Alt ()
pattern Alt a b c = H.Alt () (a :: Pat) (b :: Rhs) (c :: (Maybe Binds)) :: Alt

#if MIN_VERSION_GLASGOW_HASKELL(8,2,1,0)
{-# COMPLETE Alt #-}
#endif

-- * Functions

prelude_mod, main_mod :: ModuleName
prelude_mod = H.prelude_mod ()
main_mod = H.main_mod ()

main_name :: Name
main_name = H.main_name ()

unit_con_name :: QName
unit_con_name = H.unit_con_name ()

tuple_con_name :: Boxed -> Int -> QName
tuple_con_name = H.tuple_con_name ()

list_con_name :: QName
list_con_name = H.list_con_name ()

list_cons_name :: QName
list_cons_name = H.list_cons_name ()

unboxed_singleton_con_name :: QName
unboxed_singleton_con_name = H.unboxed_singleton_con_name ()

unit_con :: Exp
unit_con = H.unit_con ()

tuple_con :: Boxed -> Int -> Exp
tuple_con = H.tuple_con ()

unboxed_singleton_con :: Exp
unboxed_singleton_con = H.unboxed_singleton_con ()

as_name, qualified_name, hiding_name, minus_name, bang_name, dot_name, star_name :: Name
as_name = H.as_name ()
qualified_name = H.qualified_name ()
hiding_name = H.hiding_name ()
minus_name = H.minus_name ()
bang_name = H.bang_name ()
dot_name = H.dot_name ()
star_name = H.star_name ()

hole_name :: QName
hole_name = H.hole_name ()

export_name, safe_name, unsafe_name, interruptible_name, threadsafe_name, stdcall_name, ccall_name, cplusplus_name, dotnet_name, jvm_name, js_name, javascript_name, capi_name, forall_name, family_name, role_name, stock_name, anyclass_name, via_name :: Name
export_name = H.export_name ()
safe_name = H.safe_name ()
unsafe_name = H.unsafe_name ()
interruptible_name = H.interruptible_name ()
threadsafe_name = H.threadsafe_name ()
stdcall_name = H.stdcall_name ()
ccall_name = H.ccall_name ()
cplusplus_name = H.cplusplus_name ()
dotnet_name = H.dotnet_name ()
jvm_name = H.jvm_name ()
js_name = H.js_name ()
javascript_name = H.javascript_name ()
capi_name = H.capi_name ()
forall_name = H.forall_name ()
family_name = H.family_name ()
role_name = H.role_name ()
stock_name = H.stock_name ()
anyclass_name = H.anyclass_name ()
via_name = H.via_name ()

unit_tycon_name, fun_tycon_name, list_tycon_name, unboxed_singleton_tycon_name :: QName
unit_tycon_name = H.unit_tycon_name ()
fun_tycon_name = H.fun_tycon_name ()
list_tycon_name = H.list_tycon_name ()
unboxed_singleton_tycon_name = H.unboxed_singleton_tycon_name ()

tuple_tycon_name :: Boxed -> Int -> QName
tuple_tycon_name = H.tuple_tycon_name ()

unit_tycon, fun_tycon, list_tycon, unboxed_singleton_tycon :: Type
unit_tycon = H.unit_tycon ()
fun_tycon = H.fun_tycon ()
list_tycon = H.list_tycon ()
unboxed_singleton_tycon = H.unboxed_singleton_tycon ()

tuple_tycon :: Boxed -> Int -> Type
tuple_tycon = H.tuple_tycon ()

-- skipped: (=~=) :: (Annotated a, Eq (a ())) => a l1 -> a l2 -> Bool

-- skipped: class Annotated ast
