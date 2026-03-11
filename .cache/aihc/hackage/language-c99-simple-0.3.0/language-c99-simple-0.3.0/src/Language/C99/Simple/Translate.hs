module Language.C99.Simple.Translate where

import Prelude hiding (LT, GT)

import GHC.Exts             (fromList)
import Control.Monad.State  (State, execState, get, put)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import           Language.C99.Simple.AST
import qualified Language.C99.AST         as C

import Language.C99.Util
import Language.C99.Simple.Util

translate = transtransunit

transtransunit :: TransUnit -> C.TransUnit
transtransunit (TransUnit declns fundefs) = fromList (declns' ++ fundefs') where
  declns'  = map (C.ExtDecln . transdecln) declns
  fundefs' = map (C.ExtFun   . transfundef) fundefs

transfundef :: FunDef -> C.FunDef
transfundef (FunDef storespec ty name params decln ss) =
  C.FunDef dspecs declr Nothing body where
    dspecs   = getdeclnspecs storespec ty
    body     = compound decln ss
    declr    = execState (getdeclr ty) fundeclr
    fundeclr = C.Declr Nothing (fundirectdeclr name params)

transdecln :: Decln -> C.Decln
transdecln decln = case decln of
  FunDecln storespec ty name params -> C.Decln dspecs dlist where
    dspecs     = getdeclnspecs storespec ty
    dlist      = Just $ C.InitDeclrBase $ C.InitDeclr declr
    declr      = execState (getdeclr ty) fundeclr
    fundeclr   = C.Declr Nothing (fundirectdeclr name params)

  VarDecln storespec ty name init -> C.Decln dspecs dlist where
    dspecs = getdeclnspecs storespec ty
    dlist  = Just $ case init of
      Nothing  -> C.InitDeclrBase $ C.InitDeclr      declr
      Just val -> C.InitDeclrBase $ C.InitDeclrInitr declr (transinit val)
    declr  = execState (getdeclr ty) (identdeclr name)

  TypeDecln ty -> C.Decln dspecs Nothing where
    dspecs = getdeclnspecs Nothing ty

transparamdecln :: Param -> C.ParamDecln
transparamdecln (Param ty name) = C.ParamDecln dspecs declr where
  dspecs = getdeclnspecs Nothing ty
  declr  = execState (getdeclr ty) (identdeclr name)

transparam :: Param -> C.Decln
transparam (Param ty name) = C.Decln dspecs dlist where
  dspecs = getdeclnspecs Nothing ty
  dlist  = Just $ C.InitDeclrBase $ C.InitDeclr declr
  declr  = execState (getdeclr ty) (identdeclr name)

getdeclr :: Type -> State C.Declr ()
getdeclr ty = case ty of
  Type      ty'     -> do
    getdeclr ty'
    declr <- get
    put $ C.Declr Nothing (C.DirectDeclrDeclr declr)

  TypeSpec  ty' -> return ()

  Ptr       ty' -> do
    let (quals, ty'') = gettypequals ty'
    declr <- get
    put $ insertptr (C.PtrBase quals) declr
    getdeclr ty''

  Array ty' len -> do
    let lenexpr = (wrap.transexpr) <$> len
    C.Declr ptr declr <- get
    let ddeclr = case ptr of
          Nothing -> declr
          Just _  -> C.DirectDeclrDeclr $ C.Declr ptr declr
    put $ C.Declr Nothing (C.DirectDeclrArray1 ddeclr Nothing lenexpr)
    getdeclr ty'

  Const    ty' -> getdeclr ty'
  Restrict ty' -> getdeclr ty'
  Volatile ty' -> getdeclr ty'


getdeclnspecs :: Maybe StorageSpec -> Type -> C.DeclnSpecs
getdeclnspecs storespec ty = dspecs where
  dspecs = case storespec of
    Nothing   -> tyspec
    Just spec -> C.DeclnSpecsStorage (transstorespec spec) (Just tyspec)

  tyspec = case ty of
    Type     ty'   -> rec ty'
    TypeSpec ty'   -> foldtypespecs $ spec2spec ty'
    Ptr      ty'   -> rec (snd $ gettypequals ty')
    Array    ty' _ -> rec ty'
    Const    ty'   -> C.DeclnSpecsQual C.QConst    (Just $ rec ty')
    Restrict ty'   -> C.DeclnSpecsQual C.QRestrict (Just $ rec ty')
    Volatile ty'   -> C.DeclnSpecsQual C.QVolatile (Just $ rec ty')

  rec = getdeclnspecs Nothing

transstorespec :: StorageSpec -> C.StorageClassSpec
transstorespec spec = case spec of
  Typedef  -> C.STypedef
  Extern   -> C.SExtern
  Static   -> C.SStatic
  Auto     -> C.SAuto
  Register -> C.SRegister

spec2spec :: TypeSpec -> [C.TypeSpec]
spec2spec ts = case ts of
  Void                -> [C.TVoid]
  Char                -> [C.TChar]
  Signed_Char         -> [C.TSigned, C.TChar]
  Unsigned_Char       -> [C.TUnsigned, C.TChar]

  Short               -> [C.TShort]
  Signed_Short        -> [C.TSigned, C.TShort]
  Short_Int           -> [C.TShort, C.TInt]
  Signed_Short_Int    -> [C.TSigned, C.TShort, C.TInt]

  Unsigned_Short      -> [C.TUnsigned, C.TShort]
  Unsigned_Short_Int  -> [C.TUnsigned, C.TShort, C.TInt]

  Int                 -> [C.TInt]
  Signed              -> [C.TSigned]
  Signed_Int          -> [C.TSigned, C.TInt]

  Unsigned            -> [C.TUnsigned]
  Unsigned_Int        -> [C.TUnsigned, C.TInt]

  Long                -> [C.TLong]
  Signed_Long         -> [C.TSigned, C.TLong]
  Long_Int            -> [C.TLong, C.TInt]
  Signed_Long_Int     -> [C.TSigned, C.TLong, C.TInt]

  Unsigned_Long       -> [C.TUnsigned, C.TLong]
  Unsgined_Long_Int   -> [C.TUnsigned, C.TLong, C.TInt]

  Long_Long           -> [C.TLong, C.TLong]
  Signed_Long_Long    -> [C.TSigned, C.TLong, C.TLong]
  Long_Long_Int       -> [C.TLong, C.TLong, C.TInt]
  Signed_Long_Long_Int-> [C.TSigned, C.TLong, C.TLong, C.TInt]

  Unsigned_Long_Long      -> [C.TUnsigned, C.TLong, C.TLong]
  Unsigned_Long_Long_Int  -> [C.TUnsigned, C.TLong, C.TLong, C.TInt]

  Float               -> [C.TFloat]
  Double              -> [C.TDouble]
  Long_Double         -> [C.TLong, C.TDouble]
  Bool                -> [C.TBool]
  Float_Complex       -> [C.TComplex, C.TFloat]
  Double_Complex      -> [C.TComplex, C.TDouble]
  Long_Double_Complex -> [C.TLong, C.TDouble, C.TComplex]
  TypedefName name -> [C.TTypedef $ C.TypedefName $ ident name]
  Struct      name -> [C.TStructOrUnion $ C.StructOrUnionForwDecln C.Struct (ident name)]
  StructDecln name declns -> [C.TStructOrUnion $ C.StructOrUnionDecln C.Struct (ident <$> name) declns'] where
    declns' = transfielddeclns declns
  Union      name -> [C.TStructOrUnion $ C.StructOrUnionForwDecln C.Union (ident name)]
  UnionDecln name declns -> [C.TStructOrUnion $ C.StructOrUnionDecln C.Union (ident <$> name) declns'] where
    declns' = transfielddeclns declns
  Enum      name -> [C.TEnum $ C.EnumSpecForw (ident name)]
  EnumDecln name declns -> [C.TEnum $ C.EnumSpec (ident <$> name) declns'] where
    declns' = transvariantdeclns declns

transfielddeclns :: NonEmpty FieldDecln -> C.StructDeclnList
transfielddeclns (decln NE.:| declns) = foldl step (base decln) declns
  where
    base d    = C.StructDeclnBase (transfielddecln d)
    step ds d = C.StructDeclnCons ds (transfielddecln d)

transfielddecln :: FieldDecln -> C.StructDecln
transfielddecln (FieldDecln ty name) = C.StructDecln quals declrlist where
  declrlist = C.StructDeclrBase $ C.StructDeclr declr
  declr = execState (getdeclr ty) (identdeclr name)
  quals = getspecquals ty

transvariantdeclns :: NonEmpty Ident -> C.EnumrList
transvariantdeclns (decln NE.:| declns) = foldl step (base decln) declns
  where
    base d    = C.EnumrBase (transvariantdecln d)
    step ds d = C.EnumrCons ds (transvariantdecln d)

transvariantdecln :: Ident -> C.Enumr
transvariantdecln name = C.Enumr (C.Enum (ident name))

getspecquals :: Type -> C.SpecQualList
getspecquals ty = case ty of
  Type     ty'     -> getspecquals ty'
  TypeSpec ts      -> foldtypequals $ spec2spec ts
  Ptr      ty'     -> getspecquals ty'
  Array    ty' len -> getspecquals ty'
  Const    ty'     -> C.SpecQualQual C.QConst    (Just $ getspecquals ty')
  Restrict ty'     -> C.SpecQualQual C.QRestrict (Just $ getspecquals ty')
  Volatile ty'     -> C.SpecQualQual C.QVolatile (Just $ getspecquals ty')


transexpr :: Expr -> C.Expr
transexpr e = case e of
  Ident      i         -> wrap $ C.PrimIdent $ ident i
  LitBool    b         -> wrap $ litbool   b
  LitInt     i         -> wrap $ litint    i
  LitFloat   f         -> wrap $ litfloat  f
  LitDouble  d         -> wrap $ litdouble d
  LitString  s         -> wrap $ litstring s
  Index      arr idx   -> wrap $ indexexpr arr idx
  Funcall    fun args  -> wrap $ funcall   fun args
  Dot        e   field -> wrap $ dotexpr   e field
  Arrow      e   field -> wrap $ arrowexpr e field
  InitVal    ty  init  -> wrap $ initexpr  ty init
  UnaryOp    op e      -> wrap $ unaryop op e
  Cast       ty e      -> wrap $ castexpr ty e
  BinaryOp   op e1 e2  -> binaryop op e1 e2
  AssignOp   op e1 e2  -> wrap $ assignop op e1 e2
  Cond       c e1 e2   -> wrap $ condexpr c e1 e2
  SizeOf     e         -> wrap $ C.UnarySizeExpr (wrap $ transexpr e)
  SizeOfType ty        -> wrap $ C.UnarySizeType (transtypename ty)


unaryop :: UnaryOp -> Expr -> C.UnaryExpr
unaryop op e = case op of
    Inc     -> C.UnaryInc          (wrap e')
    Dec     -> C.UnaryDec          (wrap e')
    Ref     -> C.UnaryOp C.UORef   (wrap e')
    DeRef   -> C.UnaryOp C.UODeref (wrap e')
    Plus    -> C.UnaryOp C.UOPlus  (wrap e')
    Min     -> C.UnaryOp C.UOMin   (wrap e')
    BoolNot -> C.UnaryOp C.UOBNot  (wrap e')
    Not     -> C.UnaryOp C.UONot   (wrap e')
  where
    e' = transexpr e

binaryop :: BinaryOp -> Expr -> Expr -> C.Expr
binaryop op e1 e2 = case op of
    Mult   -> wrap $ C.MultMult   (wrap e1') (wrap e2')
    Div    -> wrap $ C.MultDiv    (wrap e1') (wrap e2')
    Mod    -> wrap $ C.MultMod    (wrap e1') (wrap e2')
    Add    -> wrap $ C.AddPlus    (wrap e1') (wrap e2')
    Sub    -> wrap $ C.AddMin     (wrap e1') (wrap e2')
    ShiftL -> wrap $ C.ShiftLeft  (wrap e1') (wrap e2')
    ShiftR -> wrap $ C.ShiftRight (wrap e1') (wrap e2')
    LT     -> wrap $ C.RelLT      (wrap e1') (wrap e2')
    GT     -> wrap $ C.RelGT      (wrap e1') (wrap e2')
    LE     -> wrap $ C.RelLE      (wrap e1') (wrap e2')
    GE     -> wrap $ C.RelGE      (wrap e1') (wrap e2')
    Eq     -> wrap $ C.EqEq       (wrap e1') (wrap e2')
    NEq    -> wrap $ C.EqNEq      (wrap e1') (wrap e2')
    And    -> wrap $ C.And        (wrap e1') (wrap e2')
    XOr    -> wrap $ C.XOr        (wrap e1') (wrap e2')
    Or     -> wrap $ C.Or         (wrap e1') (wrap e2')
    LAnd   -> wrap $ C.LAnd       (wrap e1') (wrap e2')
    LOr    -> wrap $ C.LOr        (wrap e1') (wrap e2')
  where
    e1' = transexpr e1
    e2' = transexpr e2

assignop :: AssignOp -> Expr -> Expr -> C.AssignExpr
assignop op e1 e2 = C.Assign e1' op' e2' where
  e1' = wrap $ transexpr e1
  e2' = wrap $ transexpr e2
  op' = case op of
    Assign       -> C.AEq
    AssignMult   -> C.ATimes
    AssignDiv    -> C.ADiv
    AssignMod    -> C.AMod
    AssignAdd    -> C.AAdd
    AssignSub    -> C.ASub
    AssignShiftL -> C.AShiftL
    AssignShiftR -> C.AShiftR
    AssignAnd    -> C.AAnd
    AssignXOr    -> C.AXOr
    AssignOr     -> C.AOr

transinit :: Init -> C.Init
transinit (InitExpr e)  = C.InitExpr (wrap $ transexpr e)
transinit (InitList es) = C.InitList (transinitlist es)

transinitlist :: NonEmpty InitItem -> C.InitList
transinitlist (x NE.:| xs) = foldl step (base x) xs
  where
    base (InitItem mident y)    = C.InitBase    (transdesigr <$> mident) (transinit y)
    step ys (InitItem mident y) = C.InitCons ys (transdesigr <$> mident) (transinit y)

transdesigr :: Ident -> C.Design
transdesigr = C.Design . C.DesigrBase . C.DesigrIdent . ident

initexpr :: TypeName -> NonEmpty InitItem -> C.PostfixExpr
initexpr ty inits = C.PostfixInits ty' inits' where
  ty'    = transtypename ty
  inits' = transinititems inits

transinititems :: NonEmpty InitItem -> C.InitList
transinititems = transinitlist

indexexpr arr idx = C.PostfixIndex arr' idx' where
  arr' = wrap $ transexpr arr
  idx' = wrap $ transexpr idx

dotexpr e field = C.PostfixDot e' field' where
  e'     = wrap $ transexpr e
  field' = ident field

arrowexpr e field = C.PostfixArrow e' field' where
  e'     = wrap $ transexpr e
  field' = ident field

castexpr ty e = C.Cast ty' e' where
  ty' = transtypename ty
  e'  = wrap $ transexpr e

funcall fun args = C.PostfixFunction fun' args' where
  fun'  = wrap $ transexpr fun
  args' = case argses of
    [] -> Nothing
    _  -> Just $ fromList argses

  argses :: [C.AssignExpr]
  argses = map wrap exprs

  exprs :: [C.Expr]
  exprs = map transexpr args

condexpr c e1 e2 = C.Cond c' e1' e2' where
  c'  = wrap $ transexpr c
  e1' = wrap $ transexpr e1
  e2' = wrap $ transexpr e2

transtypename :: TypeName -> C.TypeName
transtypename (TypeName ty) = C.TypeName specquals adeclr where
  specquals = getspecquals ty
  adeclr    = execState (getabstractdeclr ty) Nothing

getabstractdeclr :: Type -> State (Maybe C.AbstractDeclr) ()
getabstractdeclr ty = case ty of
  Type ty' -> do
    getabstractdeclr ty'
    adeclr <- get
    case adeclr of
      Nothing      -> return ()
      Just adeclr' -> put $ Just $ C.AbstractDeclrDirect Nothing dadeclr where
        dadeclr = C.DirectAbstractDeclr adeclr'

  TypeSpec ts -> return ()

  Ptr ty' -> do
    let (quals, ty'') = gettypequals ty'
        ptr           = C.PtrBase quals
    adeclr <- get
    case adeclr of
      Nothing      -> put $ Just $ C.AbstractDeclr ptr
      Just adeclr' -> put $ Just $ C.AbstractDeclrDirect (Just ptr) dadeclr where
        dadeclr = C.DirectAbstractDeclr adeclr'
    getabstractdeclr ty''

  Array ty' len -> do
    let lenexpr       = (wrap.transexpr) <$> len
        emptyarrdeclr = C.DirectAbstractDeclrArray1 Nothing Nothing lenexpr
    adeclr <- get
    let declr = case adeclr of
          Nothing -> C.AbstractDeclrDirect Nothing emptyarrdeclr
          Just adeclr -> case adeclr of
            C.AbstractDeclrDirect mptr adeclr' -> C.AbstractDeclrDirect mptr arrdeclr where
              arrdeclr = C.DirectAbstractDeclrArray1 (Just adeclr') Nothing lenexpr
            C.AbstractDeclr ptr -> C.AbstractDeclrDirect (Just ptr) emptyarrdeclr
    put $ Just declr
    getabstractdeclr ty'

  Const    ty' -> getabstractdeclr ty'
  Restrict ty' -> getabstractdeclr ty'
  Volatile ty' -> getabstractdeclr ty'

transstmt :: Stmt -> C.Stmt
transstmt stmt = case stmt of
  Expr    e                  -> exprstmt e
  If      cond ss            -> ifstmt cond ss
  IfElse  cond ssthen sselse -> ifelsestmt cond ssthen sselse
  Switch  cond cases         -> switchstmt cond cases
  While   cond ss            -> whilestmt cond ss
  For     start end step ss  -> forstmt (Just start) (Just end) (Just step) ss
  ForInf                 ss  -> forstmt Nothing      Nothing    Nothing     ss
  Continue                   -> C.StmtJump $ C.JumpContinue
  Break                      -> C.StmtJump $ C.JumpBreak
  Label   name   s           -> labelstmt name s
  Return  e                  -> returnstmt e

exprstmt :: Expr -> C.Stmt
exprstmt e = C.StmtExpr   $ C.ExprStmt (Just $ wrap $ transexpr e)

ifstmt :: Expr -> [Stmt] -> C.Stmt
ifstmt cond ss = C.StmtSelect $ C.SelectIf cond' body where
  cond' = wrap $ transexpr cond
  body  = compoundstmt [] ss

ifelsestmt :: Expr -> [Stmt] -> [Stmt] -> C.Stmt
ifelsestmt cond ssthen sselse =
  C.StmtSelect $ C.SelectIfElse cond' ssthen' sselse' where
    cond'  = wrap $ transexpr cond
    ssthen' = compoundstmt [] ssthen
    sselse' = compoundstmt [] sselse

switchstmt :: Expr -> [Case] -> C.Stmt
switchstmt cond cs = C.StmtSelect $ C.SelectSwitch cond' cs' where
  cond' = wrap $ transexpr cond
  cs'   = casestmt cs

whilestmt :: Expr -> [Stmt] -> C.Stmt
whilestmt cond ss = C.StmtIter $ C.IterWhile cond' ss' where
  cond' = wrap $ transexpr cond
  ss'   = compoundstmt [] ss

forstmt :: Maybe Expr -> Maybe Expr -> Maybe Expr -> [Stmt] -> C.Stmt
forstmt start end step ss =
  C.StmtIter $ C.IterForUpdate start' end' step' ss' where
    start' = (wrap.transexpr) <$> start
    end'   = (wrap.transexpr) <$> end
    step'  = (wrap.transexpr) <$> step
    ss'    = compoundstmt [] ss

labelstmt :: String -> Stmt -> C.Stmt
labelstmt name s = C.StmtLabeled $ C.LabeledIdent (ident name) (transstmt s)

returnstmt :: Maybe Expr -> C.Stmt
returnstmt e = C.StmtJump $ C.JumpReturn ((wrap.transexpr) <$> e)

casestmt :: [Case] -> C.Stmt
casestmt cs =
  C.StmtCompound $ C.Compound (Just $ fromList $ map casestmt' cs) where
    casestmt' cs = C.BlockItemStmt $ C.StmtLabeled $ case cs of
      Case  e s -> C.LabeledCase (C.Const $ wrap $ transexpr e) (transstmt s)
      Default s -> C.LabeledDefault (transstmt s)

compound :: [Decln] -> [Stmt] -> C.CompoundStmt
compound ds ss = C.Compound (Just $ fromList items) where
  items = ds' ++ ss'
  ss' = map (C.BlockItemStmt . transstmt) ss
  ds' = map (C.BlockItemDecln . transdecln) ds

compoundstmt :: [Decln] -> [Stmt] -> C.Stmt
compoundstmt ds ss = C.StmtCompound $ compound ds ss

fundirectdeclr :: Ident -> [Param] -> C.DirectDeclr
fundirectdeclr name params = C.DirectDeclrFun1 namedeclr params' where
  namedeclr = C.DirectDeclrIdent $ ident name
  params'   = C.ParamTypeList $ voidparamlist $ map transparamdecln params

