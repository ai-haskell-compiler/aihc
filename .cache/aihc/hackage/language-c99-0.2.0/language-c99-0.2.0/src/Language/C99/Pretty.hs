module Language.C99.Pretty where

import Language.C99.AST

import Text.PrettyPrint
import Prelude hiding ((<>))


-- Binary operator
bin :: (Pretty a, Pretty b) => a -> String -> b -> Doc
bin x op y = pretty x <+> text op <+> pretty y

class Pretty a where
  pretty :: a -> Doc

instance Pretty a => Pretty (Maybe a) where
  pretty (Just x) = pretty x
  pretty Nothing  = empty

{- IDENTIFIERS -}
{- 6.4.2.1 -}
instance Pretty Ident where
  pretty (IdentBase           idn) = pretty idn
  pretty (IdentConsNonDigit i idn) = pretty i <> pretty idn
  pretty (IdentCons         i d  ) = pretty i <> pretty d

instance Pretty IdentNonDigit where
  pretty (IdentNonDigit     nd ) = pretty nd
  pretty (IdentNonDigitUniv ucn) = pretty ucn

instance Pretty NonDigit where
  pretty c = case c of
    NDUnderscore -> char '_'
    NDa -> char 'a' ;      NDA -> char 'A'
    NDb -> char 'b' ;      NDB -> char 'B'
    NDc -> char 'c' ;      NDC -> char 'C'
    NDd -> char 'd' ;      NDD -> char 'D'
    NDe -> char 'e' ;      NDE -> char 'E'
    NDf -> char 'f' ;      NDF -> char 'F'
    NDg -> char 'g' ;      NDG -> char 'G'
    NDh -> char 'h' ;      NDH -> char 'H'
    NDi -> char 'i' ;      NDI -> char 'I'
    NDj -> char 'j' ;      NDJ -> char 'J'
    NDk -> char 'k' ;      NDK -> char 'K'
    NDl -> char 'l' ;      NDL -> char 'L'
    NDm -> char 'm' ;      NDM -> char 'M'
    NDn -> char 'n' ;      NDN -> char 'N'
    NDo -> char 'o' ;      NDO -> char 'O'
    NDp -> char 'p' ;      NDP -> char 'P'
    NDq -> char 'q' ;      NDQ -> char 'Q'
    NDr -> char 'r' ;      NDR -> char 'R'
    NDs -> char 's' ;      NDS -> char 'S'
    NDt -> char 't' ;      NDT -> char 'T'
    NDu -> char 'u' ;      NDU -> char 'U'
    NDv -> char 'v' ;      NDV -> char 'V'
    NDw -> char 'w' ;      NDW -> char 'W'
    NDx -> char 'x' ;      NDX -> char 'X'
    NDy -> char 'y' ;      NDY -> char 'Y'
    NDz -> char 'z' ;      NDZ -> char 'Z'

instance Pretty Digit where
  pretty c = case c of
    DZero  -> int 0
    DOne   -> int 1
    DTwo   -> int 2
    DThree -> int 3
    DFour  -> int 4
    DFive  -> int 5
    DSix   -> int 6
    DSeven -> int 7
    DEight -> int 8
    DNine  -> int 9


{- UNIVERSAL CHARACTER NAMES -}
{- 6.4.3 -}
instance Pretty UnivCharName where
  pretty (UnivCharName1 hq     ) = text "\\u" <> pretty hq
  pretty (UnivCharName2 hq1 hq2) = text "\\U" <> pretty hq1 <> pretty hq2

instance Pretty HexQuad where
  pretty (HexQuad hd1 hd2 hd3 hd4) =  pretty hd1 <> pretty hd2
                                   <> pretty hd3 <> pretty hd4


{- CONSTANTS -}
{- 6.4.4 -}
instance Pretty Const where
  pretty (ConstInt   ic) = pretty ic
  pretty (ConstFloat fc) = pretty fc
  pretty (ConstEnum  ec) = pretty ec
  pretty (ConstChar  cc) = pretty cc

{- 6.4.4.1 -}
instance Pretty IntConst where
  pretty (IntDec dc mis) = pretty dc <> pretty mis
  pretty (IntOc  oc mis) = pretty oc <> pretty mis
  pretty (IntHex hc mis) = pretty hc <> pretty mis

instance Pretty DecConst where
  pretty (DecBase    nzd) = pretty nzd
  pretty (DecCons dc d  ) = pretty dc <> pretty d

instance Pretty OcConst where
  pretty Oc0            = int 0
  pretty (OcCons oc od) = pretty oc <> pretty od

instance Pretty HexConst where
  pretty (HexBase prefix digit) = pretty prefix <> pretty digit
  pretty (HexCons hexes digit)  = pretty hexes <> pretty digit

instance Pretty HexPrefix where
  pretty OX = text "0x"

instance Pretty NonZeroDigit where
  pretty d = case d of
    NZOne   -> int 1
    NZTwo   -> int 2
    NZThree -> int 3
    NZFour  -> int 4
    NZFive  -> int 5
    NZSix   -> int 6
    NZSeven -> int 7
    NZEight -> int 8
    NZNine  -> int 9

instance Pretty OcDigit where
  pretty d = case d of
    OcZero  -> text "0"
    OcOne   -> text "1"
    OcTwo   -> text "2"
    OcThree -> text "3"
    OcFour  -> text "4"
    OcFive  -> text "5"
    OcSix   -> text "6"
    OcSeven -> text "7"

instance Pretty HexDigit where
  pretty HexZero  = text "0"
  pretty HexOne   = text "1"
  pretty HexTwo   = text "2"
  pretty HexThree = text "3"
  pretty HexFour  = text "4"
  pretty HexFive  = text "5"
  pretty HexSix   = text "6"
  pretty HexSeven = text "7"
  pretty HexEight = text "8"
  pretty HexNine  = text "9"
  pretty HexA     = text "A"
  pretty HexB     = text "B"
  pretty HexC     = text "C"
  pretty HexD     = text "D"
  pretty HexE     = text "E"
  pretty HexF     = text "F"

instance Pretty IntSuffix where
  pretty (IntSuffixUnsignedLong     u  ml) = pretty u  <> pretty ml
  pretty (IntSuffixUnsignedLongLong u  ll) = pretty u  <> pretty ll
  pretty (IntSuffixLong             l  mu) = pretty l  <> pretty mu
  pretty (IntSuffixLongLong         ll mu) = pretty ll <> pretty mu

instance Pretty UnsignedSuffix where
  pretty U = char 'U'

instance Pretty LongSuffix     where
  pretty L = char 'L'

instance Pretty LongLongSuffix where
  pretty rL = text "LL"

{- 6.4.4.2 -}
instance Pretty FloatConst where
  pretty (FloatDec dfc) = pretty dfc
  pretty (FloatHex hfc) = pretty hfc

instance Pretty DecFloatConst where
  pretty (DecFloatFrac   fc me mfs) = pretty fc <> pretty me <> pretty mfs
  pretty (DecFloatDigits ds ep mfs) = pretty ds <> pretty ep <> pretty mfs

instance Pretty HexFloatConst where
  pretty (HexFloatFrac   hp hfc bep mfs) =
    pretty hp <> pretty hfc <> pretty bep <> pretty mfs
  pretty (HexFloatDigits hp hds bep mfs) =
    pretty hp <> pretty hds <> pretty bep <> pretty mfs

instance Pretty FracConst where
  pretty (FracZero mds ds) = pretty mds <> char '.' <> pretty ds
  pretty (Frac         ds) = pretty ds <> char '.'

instance Pretty ExpPart where
  pretty (E ms ds) = char 'e' <> pretty ms <> pretty ds

instance Pretty Sign where
  pretty SPlus  = char '+'
  pretty SMinus = char '-'

instance Pretty DigitSeq where
  pretty (DigitBase    d) = pretty d
  pretty (DigitCons ds d) = pretty ds <> pretty d

instance Pretty HexFracConst where
  pretty (HexFracZero mhds hds) = pretty mhds <> char '.' <> pretty hds
  pretty (HexFrac          hds) = pretty hds <> char '.'

instance Pretty BinExpPart where
  pretty (P ms ds) = char 'p' <> pretty ms <> pretty ds

instance Pretty HexDigitSeq where
  pretty (HexDigitBase     hd) = pretty hd
  pretty (HexDigitCons hds hd) = pretty hds <> pretty hd

instance Pretty FloatSuffix where
  pretty FF = char 'f'
  pretty FL = char 'l'

{- 6.4.4.3 -}
instance Pretty EnumConst where
  pretty (Enum i) = pretty i

{- 6.4.4.4 -}
instance Pretty CharConst where
  pretty (Char charSeq)  = quotes (pretty charSeq)
  pretty (CharL charSeq) = char 'L' <> quotes (pretty charSeq)

instance Pretty CCharSeq where
  pretty (CCharBase cchar)      = pretty cchar
  pretty (CCharCons cseq cchar) = pretty cseq <> pretty cchar

instance Pretty CChar where
  pretty (CChar ch)        = char ch
  pretty (CCharEsc escSeq) = pretty escSeq

instance Pretty EscSeq where
  pretty (EscSimple se) = pretty se

instance Pretty SimpleEscSeq where
  pretty esc = case esc of
    SEQuote     -> text "\\\'"
    SEDQuote    -> text "\\\""
    SEQuestion  -> text "\\?"
    SEBackSlash -> text "\\\\"
    SEa         -> text "\\a"
    SEb         -> text "\\b"
    SEf         -> text "\\f"
    SEn         -> text "\\n"
    SEr         -> text "\\r"
    SEt         -> text "\\t"
    SEv         -> text "\\v"

instance Pretty OcEscSeq where
  pretty (OcEsc1 od) = char '\\' <> pretty od
  pretty (OcEsc2 od1 od2) = char '\\' <> pretty od1 <> pretty od2
  pretty (OcEsc3 od1 od2 od3) = char '\\' <> pretty od1 <> pretty od2 <> pretty od3

instance Pretty HexEscSeq where
  pretty (HexEscBase hd) = text "\\x" <> pretty hd
  pretty (HexEscCons hs hd) = pretty hs <> pretty hd

{- STRING LITERALS -}
{- 6.4.5 -}
instance Pretty StringLit where
  pretty (StringLit  mcs) =             doubleQuotes (pretty mcs)
  pretty (StringLitL mcs) = char 'L' <> doubleQuotes (pretty mcs)

instance Pretty SCharSeq where
  pretty (SCharBase sc    ) = pretty sc
  pretty (SCharCons scs sc) = pretty scs <> pretty sc

instance Pretty SChar where
  pretty (SChar    c ) = char c
  pretty (SCharEsc es) = pretty es


{- EXPRESSIONS -}
{- 6.5.1 -}
instance Pretty PrimExpr where
  pretty (PrimIdent  i ) = pretty i
  pretty (PrimConst  c ) = pretty c
  pretty (PrimString sl) = pretty sl
  pretty (PrimExpr   e ) = parens (pretty e)

{- 6.5.2 -}
instance Pretty PostfixExpr where
  pretty (PostfixPrim     pe     ) = pretty pe
  pretty (PostfixIndex    pe e   ) = pretty pe <> brackets (pretty e)
  pretty (PostfixFunction pe mael) = pretty pe <> parens (pretty mael)
  pretty (PostfixDot      pe i   ) = pretty pe <> char '.' <> pretty i
  pretty (PostfixArrow    pe i   ) = pretty pe <> text "->" <> pretty i
  pretty (PostfixInc      pe     ) = pretty pe <> text "++"
  pretty (PostfixDec      pe     ) = pretty pe <> text "--"
  pretty (PostfixInits    tn il  ) = parens (pretty tn) <> braces (pretty il)

instance Pretty ArgExprList where
  pretty (ArgExprListBase     ae) = pretty ae
  pretty (ArgExprListCons ael ae) = pretty ael <> comma <+> pretty ae

{- 6.5.3 -}
instance Pretty UnaryExpr where
  pretty (UnaryPostfix  pe   ) = pretty pe
  pretty (UnaryInc      ue   ) = text "++" <> pretty ue
  pretty (UnaryDec      ue   ) = text "--" <> pretty ue
  pretty (UnaryOp       op ce) = pretty op <> pretty ce
  pretty (UnarySizeExpr ue   ) = text "sizeof" <+> pretty ue
  pretty (UnarySizeType tn   ) = text "sizeof" <> parens (pretty tn)

instance Pretty UnaryOp where
  pretty op = case op of
    UORef   -> char '&'
    UODeref -> char '*'
    UOPlus  -> char '+'
    UOMin   -> char '-'
    UOBNot  -> char '~'
    UONot   -> char '!'

{- 6.5.4 -}
instance Pretty CastExpr where
  pretty (CastUnary ue) = pretty ue
  pretty (Cast tn ce)   = parens (pretty tn) <> pretty ce

{- 6.5.5 -}
instance Pretty MultExpr where
  pretty (MultCast    ce) = pretty ce
  pretty (MultMult me ce) = bin me "*" ce
  pretty (MultDiv  me ce) = bin me "/" ce
  pretty (MultMod  me ce) = bin me "%" ce

{- 6.5.6 -}
instance Pretty AddExpr where
  pretty (AddMult    me) = pretty me
  pretty (AddPlus ae me) = bin ae "+" me
  pretty (AddMin  ae me) = bin ae "-" me

{- 6.5.7 -}
instance Pretty ShiftExpr where
  pretty (ShiftAdd         add) = pretty add
  pretty (ShiftLeft  shift add) = bin shift "<<" add
  pretty (ShiftRight shift add) = bin shift ">>" add

{- 6.5.8 -}
instance Pretty RelExpr where
  pretty (RelShift     shift) = pretty shift
  pretty (RelLT    rel shift) = bin rel "<"  shift
  pretty (RelGT    rel shift) = bin rel ">"  shift
  pretty (RelLE    rel shift) = bin rel "<=" shift
  pretty (RelGE    rel shift) = bin rel ">=" shift

{- 6.5.9 -}
instance Pretty EqExpr where
  pretty (EqRel    rel) = pretty rel
  pretty (EqEq  eq rel) = bin eq "==" rel
  pretty (EqNEq eq rel) = bin eq "!=" rel

{- 6.5.10 -}
instance Pretty AndExpr where
  pretty (AndEq     eq) = pretty eq
  pretty (And   and eq) = bin and "&" eq

{- 6.5.11 -}
instance Pretty XOrExpr where
  pretty (XOrAnd     and) = pretty and
  pretty (XOr    xor and) = bin xor "^" and

{- 6.5.12 -}
instance Pretty OrExpr where
  pretty (OrXOr    xor) = pretty xor
  pretty (Or    or xor) = bin or "|" xor

{- 6.5.13 -}
instance Pretty LAndExpr where
  pretty (LAndOr     or) = pretty or
  pretty (LAnd   and or) = bin and "&&" or

{- 6.5.14 -}
instance Pretty LOrExpr where
  pretty (LOrAnd    and) = pretty and
  pretty (LOr    or and) = bin or "||" and

{- 6.5.15 -}
instance Pretty CondExpr where
  pretty (CondLOr le     ) = pretty le
  pretty (Cond    le e ce) = pretty le <+> char '?' <+> pretty e <+> colon <+> pretty ce

{- 6.5.16 -}
instance Pretty AssignExpr where
  pretty (AssignCond ce)   = pretty ce
  pretty (Assign ue op ae) = pretty ue <+> pretty op <+> pretty ae

instance Pretty AssignOp where
  pretty op = case op of
    AEq     -> text "="
    ATimes  -> text "*="
    ADiv    -> text "/="
    AMod    -> text "%="
    AAdd    -> text "+="
    ASub    -> text "-="
    AShiftL -> text "<<="
    AShiftR -> text ">>="
    AAnd    -> text "&="
    AXOr    -> text "^="
    AOr     -> text "|="

{- 6.5.17 -}
instance Pretty Expr where
  pretty (ExprAssign   ae) = pretty ae
  pretty (Expr       e ae) = pretty e <> comma <+> pretty ae

{- 6.6 -}
instance Pretty ConstExpr where
  pretty (Const ce) = pretty ce


{- DECLARATIONS -}
{- 6.7 -}
instance Pretty Decln where
  pretty (Decln ds midl) = pretty ds <+> pretty midl

instance Pretty DeclnSpecs where
  pretty (DeclnSpecsStorage scs mds) = pretty scs <+> pretty mds
  pretty (DeclnSpecsType    ts  mds) = pretty ts  <+> pretty mds
  pretty (DeclnSpecsQual    tq  mds) = pretty tq  <+> pretty mds
  pretty (DeclnSpecsFun     fs  mds) = pretty fs  <+> pretty mds

instance Pretty InitDeclrList where
  pretty (InitDeclrBase     id) = pretty id
  pretty (InitDeclrCons idl id) = pretty idl <> comma <+> pretty id

instance Pretty InitDeclr where
  pretty (InitDeclr      d  ) = pretty d
  pretty (InitDeclrInitr d i) = pretty d <+> equals <+> pretty i

{- 6.7.1 -}
instance Pretty StorageClassSpec where
  pretty c = case c of
    STypedef  -> text "typedef"
    SExtern   -> text "extern"
    SStatic   -> text "static"
    SAuto     -> text "auto"
    SRegister -> text "register"

{- 6.7.2 -}
instance Pretty TypeSpec where
  pretty ty = case ty of
    TVoid               -> text "void"
    TChar               -> text "char"
    TShort              -> text "short"
    TInt                -> text "int"
    TLong               -> text "long"
    TFloat              -> text "float"
    TDouble             -> text "double"
    TSigned             -> text "signed"
    TUnsigned           -> text "unsigned"
    TBool               -> text "_Bool"
    TComplex            -> text "_Complex"
    TStructOrUnion sous -> pretty sous
    TEnum          es   -> pretty es
    TTypedef       tn   -> pretty tn

{- 6.7.2.1 -}
instance Pretty StructOrUnionSpec where
  pretty (StructOrUnionDecln     sou mi sdl) =
    vcat [pretty sou <+> pretty mi, lbrace, nest 2 $ pretty sdl, rbrace]
  pretty (StructOrUnionForwDecln sou i     ) =
    pretty sou <+> pretty i

instance Pretty StructOrUnion where
  pretty Struct = text "struct"
  pretty Union  = text "union"

instance Pretty StructDeclnList where
  pretty (StructDeclnBase sd    ) = pretty sd
  pretty (StructDeclnCons sdl sd) = pretty sdl $+$ pretty sd

instance Pretty StructDecln where
  pretty (StructDecln sql sdl) = pretty sql <+> pretty sdl <> char ';'

instance Pretty SpecQualList where
  pretty (SpecQualType ts msql) = pretty ts <+> pretty msql
  pretty (SpecQualQual tq msql) = pretty tq <+> pretty msql

instance Pretty StructDeclrList where
  pretty (StructDeclrBase     sd) = pretty sd
  pretty (StructDeclrCons sdl sd) = pretty sdl <+> char ',' <+> pretty sd

instance Pretty StructDeclr where
  pretty (StructDeclr    d    ) = pretty d
  pretty (StructDeclrBit md ce) = pretty md <+> char ':' <+> pretty ce

{- 6.7.2.2 -}
instance Pretty EnumSpec where
  pretty (EnumSpec mident enumrlist) = text "enum" <+> pretty mident <+> braces (pretty enumrlist)
  pretty (EnumSpecForw ident)        = text "enum" <+> pretty ident

instance Pretty EnumrList where
  pretty (EnumrBase enumr)    = pretty enumr
  pretty (EnumrCons el enumr) = pretty el <+> char ',' <+> pretty enumr

instance Pretty Enumr where
  pretty (Enumr (Enum ident))          = pretty ident
  pretty (EnumrInit (Enum ident) expr) = pretty ident <+> char '=' <+> pretty expr

{- 6.7.3 -}
instance Pretty TypeQual where
  pretty q = case q of
    QConst    -> text "const"
    QRestrict -> text "restrict"
    QVolatile -> text "volatile"

{- 6.7.4 -}
instance Pretty FunSpec where
  pretty SpecInline = text "inline"

{- 6.7.5 -}
instance Pretty Declr where
  pretty (Declr mptr dd) = pretty mptr <+> pretty dd

instance Pretty DirectDeclr where
  pretty (DirectDeclrIdent  i        ) = pretty i
  pretty (DirectDeclrDeclr  d        ) = parens $ pretty d
  pretty (DirectDeclrArray1 d mtl mae) = pretty d <> brackets (pretty mtl <+> pretty mae)
  pretty (DirectDeclrArray2 d mtl ae ) = pretty d <> brackets (text "static" <+> pretty mtl <+> pretty ae)
  pretty (DirectDeclrArray3 d tl  ae ) = pretty d <> brackets (pretty tl <+> text "static" <+> pretty ae)
  pretty (DirectDeclrArray4 d mtl    ) = pretty d <> brackets (pretty mtl <+> char '*')
  pretty (DirectDeclrFun1   d ptl    ) = pretty d <> parens (pretty ptl)
  pretty (DirectDeclrFun2   d mil    ) = pretty d <> parens (pretty mil)

instance Pretty Ptr where
  pretty (PtrBase mtql  ) = char '*' <> pretty mtql
  pretty (PtrCons mtql p) = char '*' <> pretty mtql <> pretty p

instance Pretty TypeQualList where
  pretty (TypeQualBase tq)     = pretty tq
  pretty (TypeQualCons tql tq) = pretty tql <+> pretty tq

instance Pretty ParamTypeList where
  pretty (ParamTypeList    tq) = pretty tq
  pretty (ParamTypeListVar tq) = pretty tq <> comma <+> text "..."

instance Pretty ParamList where
  pretty (ParamBase    pd) = pretty pd
  pretty (ParamCons pl pd) = pretty pl <> comma <+> pretty pd

instance Pretty ParamDecln where
  pretty (ParamDecln         ds d   ) = pretty ds <+> pretty d
  pretty (ParamDeclnAbstract ds mdad) = pretty ds <+> pretty mdad

instance Pretty IdentList where
  pretty (IdentListBase ident) = pretty ident
  pretty (IdentListCons idl ident) = pretty idl <> comma <+> pretty ident

{- 6.7.6 -}
instance Pretty TypeName where
  pretty (TypeName sql mdar) = pretty sql <+> pretty mdar

instance Pretty AbstractDeclr where
  pretty (AbstractDeclr       ptr    ) = pretty ptr
  pretty (AbstractDeclrDirect mptr ad) = pretty mptr <> pretty ad

instance Pretty DirectAbstractDeclr where
  pretty (DirectAbstractDeclr dad) = parens $ pretty dad
  pretty (DirectAbstractDeclrArray1 mdad mtql mae)
    = pretty mdad <> brackets (pretty mtql <> pretty mae)
  pretty (DirectAbstractDeclrArray2 mdad mtql ae)
    = pretty mdad <> brackets (text "static" <+> pretty mtql <> pretty ae)
  pretty (DirectAbstractDeclrArray3 mdad tql ae)
    = pretty mdad <> brackets (pretty tql <+> text "static" <+> pretty ae)
  pretty (DirectAbstractDeclrArray4 mdad) = pretty mdad <> brackets (char '*')
  pretty (DirectAbstractDeclrFun mdad mptl) = pretty mdad <> parens (pretty mptl)

{- 6.7.7 -}
instance Pretty TypedefName where
  pretty (TypedefName i) = pretty i

{- 6.7.8 -}
instance Pretty Init where
  pretty (InitExpr ae) = pretty ae
  pretty (InitList il) = braces (pretty il)

instance Pretty InitList where
  pretty (InitBase    md i) =                        pretty md <+> pretty i
  pretty (InitCons il md i) = pretty il <> comma <+> pretty md <+> pretty i

instance Pretty Design where
  pretty (Design dl) = pretty dl <+> char '='

instance Pretty DesigrList where
  pretty (DesigrBase    d) = pretty d
  pretty (DesigrCons dl d) = pretty dl <+> pretty d

instance Pretty Desigr where
  pretty (DesigrConst ce) = brackets (pretty ce)
  pretty (DesigrIdent i ) = char '.' <> pretty i

{- STATEMENTS -}
{- 6.8 -}
instance Pretty Stmt where
  pretty (StmtLabeled  ls) = pretty ls
  pretty (StmtCompound cs) = nest 2 $ braces (pretty cs)
  pretty (StmtExpr     es) = pretty es
  pretty (StmtSelect   ss) = pretty ss
  pretty (StmtIter     is) = pretty is
  pretty (StmtJump     js) = pretty js

{- 6.8.1 -}
instance Pretty LabeledStmt where
  pretty (LabeledIdent i  s) =                 pretty i  <> colon <+> pretty s
  pretty (LabeledCase  ce s) = text "case" <+> pretty ce <> colon <+> pretty s
  pretty (LabeledDefault  s) = text "default"            <> colon <+> pretty s

{- 6.8.2 -}
instance Pretty CompoundStmt where
  pretty (Compound Nothing) = empty
  pretty (Compound mbil   ) = pretty mbil

instance Pretty BlockItemList where
  pretty (BlockItemBase     bi) = pretty bi
  pretty (BlockItemCons bil bi) = pretty bil $$ pretty bi

instance Pretty BlockItem where
  pretty (BlockItemDecln d) = pretty d <> semi
  pretty (BlockItemStmt  s) = pretty s <> semi

{- 6.8.3 -}
instance Pretty ExprStmt where
  pretty (ExprStmt Nothing) = empty
  pretty (ExprStmt me)      = pretty me

{- 6.8.4 -}
instance Pretty SelectStmt where
  pretty (SelectIf c s) = vcat [ text "if" <+> parens (pretty c) <+> lbrace
                               , pretty s
                               , rbrace
                               ]
  pretty (SelectIfElse c s1 s2) =
    vcat [ text "if" <+> parens (pretty c) <+> lbrace
         , pretty s1
         , rbrace <+> text "else" <+> lbrace
         , pretty s2
         , rbrace
         ]
  pretty (SelectSwitch c s) =
    vcat [ text "switch" <+> parens (pretty c) <+> lbrace
         , pretty s
         , rbrace
         ]

{- 6.8.5 -}
instance Pretty IterStmt where
  pretty (IterWhile c s) = text "while" <+> parens (pretty c) <+> pretty s
  pretty (IterDo    s c) =
    vcat [ text "do" <+> lbrace
         , pretty s
         , rbrace <+> text "while" <+> parens (pretty c)
         ]
  pretty (IterForUpdate me1 me2 me3 s) =
    vcat [ text "for" <+> parens ( pretty me1 <> semi <+>
                                   pretty me2 <> semi <+>
                                   pretty me3 ) <+> lbrace
         , pretty s
         , rbrace
         ]
  pretty (IterFor d me1 me2 s) =
    vcat [ text "for" <+> parens ( pretty d <+> pretty me1 <> semi
                            <+> pretty me2 ) <+> lbrace
         , pretty s
         , rbrace
         ]

{- 6.8.6 -}
instance Pretty JumpStmt where
  pretty (JumpGoto i)    = text "goto" <+> pretty i
  pretty JumpContinue    = text "continue"
  pretty JumpBreak       = text "break"
  pretty (JumpReturn me) = text "return" <+> pretty me


{- EXTERNAL DEFINITIONS -}
{- 6.9 -}
instance Pretty TransUnit where
  pretty tu = vcat [pretty' tu, text ""]
    where
      pretty' (TransUnitBase    ed) = pretty ed
      pretty' (TransUnitCons tu ed) = case ed of
        ExtFun _ -> vcat [pretty' tu, text "", pretty ed]
        _        -> vcat [pretty' tu, pretty ed]

instance Pretty ExtDecln where
  pretty (ExtFun fd)  = pretty fd
  pretty (ExtDecln d) = pretty d <> semi

{- 6.9.1 -}
instance Pretty FunDef where
  pretty (FunDef ds d mdl (Compound Nothing)) = fheader ds d mdl <> semi
  pretty (FunDef ds d mdl cs) =
    vcat [ fheader ds d mdl <+> lbrace
         , nest 2 $ pretty cs
         , rbrace
         ]

instance Pretty DeclnList where
  pretty (DeclnBase    d) = pretty d
  pretty (DeclnCons dl d) = pretty dl <> comma <+> pretty d


fheader :: DeclnSpecs -> Declr -> Maybe DeclnList -> Doc
fheader ds d mdl = pretty ds <+> pretty d <+> pretty mdl
