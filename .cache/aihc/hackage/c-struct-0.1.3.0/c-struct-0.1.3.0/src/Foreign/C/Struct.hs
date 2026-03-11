{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.Struct (
	-- * STRUCT
	struct, StrName, StrSize, StrAlgn,
	MemName, MemType, MemPeek, MemPoke, DerivClass,
	-- * STRUCT WITH PRIMITIVE MONAD
	structPrim, FunCopy, FunFree ) where

import Language.Haskell.TH (
	DecsQ, DecQ, Dec(PragmaD), Pragma(CompleteP), sigD, valD, funD, tySynD,
	newtypeD, plainTV, normalC, derivClause,
		bangType, bang, noSourceUnpackedness, noSourceStrictness,
	instanceD, cxt,
	patSynSigD, patSynD, recordPatSyn, explBidir,
	ExpQ, varE, conE, appE, infixE, lamE, tupE, listE, litE, integerL,
	forallT, varT, conT, appT, varP, wildP, conP, tupP, viewP,
	Name, mkName, newName,
	ClauseQ, clause, normalB, StmtQ, doE, compE, bindS, noBindS,
	lookupTypeName, lookupValueName )
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal (malloc, mallocBytes, free, copyBytes)
import Foreign.Storable
import Control.Arrow ((&&&))
import Control.Monad (replicateM)
import Control.Monad.Primitive (PrimMonad(..), RealWorld, unsafeIOToPrim)
import Data.Bool (bool)
import Data.Maybe (mapMaybe, isJust, fromJust)
import Data.List (unzip4, intersperse, intercalate)
import Data.Array (Ix(..))
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Lexeme(..), readPrec, step, lexP, parens, prec)

import Foreign.C.Struct.Parts (
	(.->), (.$), (...), (.<$>), (.<*>), (.>>=),
	(.&&), (.||), (.==), (.<), (.+), (.*),
	intE, strP, pt, zp, ss, (..+), toLabel, lcfirst,
	bigTupleData, sbTupP, sbTupT, sbTupleE )

import Foreign.C.Struct.Ord

---------------------------------------------------------------------------

-- * STRUCT
--	+ FUNCTION STRUCT
--	+ NEWTYPE
--	+ PATTERN
--		- Function Mk Pattern
--		- Function Mk Pattern Fun
--	+ DERIVING
--		- Function Mk Deriving
--		- Show
--		- Read
--		- Eq
--		- Ord
--		- Bounded
--		- Ix
-- * STRUCT WITH PRIMITIVE MONAD
-- 	+ FUNCTION STRUCT PRIM
-- 	+ NEWTYPE
-- 	+ FREEZE
-- 	+ THAW
-- 	+ COPY

---------------------------------------------------------------------------
-- STRUCT
---------------------------------------------------------------------------

-- FUNCTION STRUCT

struct :: StrName -> StrSize -> StrAlgn ->
	[(MemName, MemType, MemPeek, MemPoke)] -> [DerivClass] -> DecsQ
struct sn sz algn (unzip4 -> (mns, mts, mpes, mpos)) dcs_ = do
	mtpl <- if ln > 62
		then do	nm <- lookupTypeName tplnm
			nm' <- lookupValueName tplnm
			if isJust nm && isJust nm'
			then pure $ Just (fromJust nm, fromJust nm')
			else Just <$> ((,) <$> newName tplnm <*> newName tplnm)
		else pure Nothing
	(\mtd dt ist -> maybe id (:) mtd $ dt ++ ist)
		<$> do	b <- isJust <$> lookupTypeName tplnm
			if b
			then pure Nothing
			else maybe (pure Nothing)
				(\(tpl, tpl') -> Just <$> bigTupleData tpl tpl' ln) mtpl
		<*> sequence [
			mkNewtype sn,
			pure . PragmaD $ CompleteP [mkName sn] Nothing,
			mkPatternSig sn mts,
			mkPatternBody mtpl sn sz mns mpos,
			mkPatternFunSig mtpl sn mts,
			mkPatternFunBody mtpl sn mpes ]
		<*> mkInstances sn sz algn mns dcs
	where
	dcs = case toDerivCollection dcs_ of
		(d, []) -> d; (_, os) -> error $ "Can't derive: " ++ show os
	ln = length mns
	tplnm = "Tuple" ++ show ln

-- ^
-- Example
--
-- @
-- struct \"Foo\" ${size Foo} #{alignment Foo}
--	[	(\"x\", ''CInt, [| \#{peek Foo, x} |], [| \#{poke Foo, x} |]),
--		(\"y\", ''CInt, [| \#{peek Foo, y} |], [| \#{poke Foo, y} |]) ]
--	[''Show, ''Read, ''Eq, ''Ord, ''Bounded, ''Storable]
-- @

type StrName = String; type StrSize = Integer; type StrAlgn = Integer
type MemName = String; type MemType = Name
type MemPeek = ExpQ; type MemPoke = ExpQ
type DerivClass = Name

-- NEWTYPE

mkNewtype :: StrName -> DecQ
mkNewtype sn =
	newtypeD (cxt []) (mkName sn) [] Nothing (normalC (mkName $ sn ++ "_") [
		bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''ForeignPtr `appT` conT (mkName sn)) ]) []

-- PATTERN

-- Function Mk Pattern

mkPatternSig :: StrName -> [MemType] -> DecQ
mkPatternSig (mkName -> sn) = patSynSigD sn . foldr (.->) (conT sn) . (conT <$>)

mkPatternBody :: Maybe (Name, Name) -> StrName -> StrSize -> [MemName] -> [MemPoke] -> DecQ
mkPatternBody mtpl sn sz ms_ pos = patSynD (mkName sn) (recordPatSyn ms)
	(explBidir [mkPatternBodyClause sn sz pos])
	(viewP (varE . mkName $ lcfirst sn) (sbTupP mtpl $ varP <$> ms))
	where ms = mkName . toLabel sn <$> ms_

mkPatternBodyClause :: StrName -> StrSize -> [MemPoke] -> ClauseQ
mkPatternBodyClause (mkName . (++ "_") -> sn) sz pos = do
	(vs, p) <- (,) <$> length pos `replicateM` newName "v" <*> newName "p"
	let	vps = varP <$> vs; pe = varE p; fr = varE 'free `appE` pe
	clause vps (normalB $ varE 'unsafePerformIO .$ conE sn .<$> doE (
		(varP p `bindS` (varE 'mallocBytes `appE` intE sz)) :
		((<$> zip pos vs) \(po, v) ->
			noBindS $ po `appE` pe `appE` varE v) ++
		[noBindS $ varE 'newForeignPtr `appE` pe `appE` fr] )) []

-- Function Mk Pattern Fun

mkPatternFunSig :: Maybe (Name, Name) -> StrName -> [MemType] -> DecQ
mkPatternFunSig mtpl (mkName . lcfirst &&& conT . mkName -> (fn, st)) =
	sigD fn . (st .->) . sbTupT mtpl . (conT <$>)

mkPatternFunBody :: Maybe (Name, Name) -> StrName -> [MemPeek] -> DecQ
mkPatternFunBody mtpl (mkName . lcfirst &&& mkName . (++ "_") -> (fn, cn)) pes =
	funD fn . (: []) $ (,) <$> newName "f" <*> newName "p" >>= \(f, p) ->
		clause [conP cn [varP f]] (normalB $ varE 'unsafePerformIO
			.$ varE 'withForeignPtr `appE` varE f
				`appE` lamE [bool (varP p) wildP $ null pes]
					(mkPatternFunPeeks mtpl p pes)) []

mkPatternFunPeeks :: Maybe (Name, Name) -> Name -> [MemPeek] -> ExpQ
mkPatternFunPeeks mtpl (varE -> p) (length &&& id -> (n, pes)) =
	foldl (.<*>) (varE 'pure .$ sbTupleE mtpl n) $ (`appE` p) <$> pes

-- DERIVING

-- Function Mk Deriving

mkInstances :: StrName -> StrSize -> StrAlgn -> [MemName] -> DerivCollection -> DecsQ
mkInstances sn sz algn ms dc =
	sequence $ (\(t, b) -> bool Nothing (Just t) b) `mapMaybe` zip [
		mkInstanceShow sn ms, mkInstanceRead sn ms, mkInstanceEq sn ms,
		mkInstanceOrd sn ms, mkInstanceBounded sn ms, mkInstanceIx sn ms,
		deriveStorable sn sz algn
		] [	derivingShow dc, derivingRead dc, derivingEq dc,
			derivingOrd dc, derivingBounded dc, derivingIx dc,
			derivingStorable dc ]

data DerivCollection = DerivCollection {
	derivingShow :: Bool, derivingRead :: Bool,
	derivingEq :: Bool, derivingOrd :: Bool,
	derivingBounded :: Bool, derivingIx :: Bool,
	derivingStorable :: Bool } deriving Show

toDerivCollection :: [DerivClass] -> (DerivCollection, [DerivClass])
toDerivCollection [] = (DerivCollection False False False False False False False, [])
toDerivCollection (d : ds) = case d of
	NameShow -> (dc { derivingShow = True }, ds')
	NameRead -> (dc { derivingRead = True }, ds')
	NameEq -> (dc { derivingEq = True }, ds')
	NameOrd -> (dc { derivingOrd = True }, ds')
	NameBounded -> (dc { derivingBounded = True }, ds')
	NameIx -> (dc { derivingIx = True }, ds')
	NameStorable -> (dc { derivingStorable = True }, ds')
	_ -> (dc, d : ds')
	where (dc, ds') = toDerivCollection ds

pattern NameShow, NameRead, NameEq, NameOrd, NameBounded, NameIx,
	NameStorable :: Name
pattern NameShow <- ((== ''Show) -> True)
pattern NameRead <- ((== ''Read) -> True)
pattern NameEq <- ((== ''Eq) -> True)
pattern NameOrd <- ((== ''Ord) -> True)
pattern NameBounded <- ((== ''Bounded) -> True)
pattern NameIx <- ((== ''Ix) -> True)
pattern NameStorable <- ((== ''Storable) -> True)

-- Show

mkInstanceShow :: StrName -> [MemName] -> DecQ
mkInstanceShow (mkName &&& id -> (sn, ssn)) ms = do
	(s, vs) <- (,) <$> newName "s" <*> length ms `replicateM` newName "v"
	instanceD (cxt []) (conT ''Show `appT` conT sn) . (: [])
		$ funD 'showsPrec [clause [wildP, varP s]
			(normalB $ ss (ssn ++ " {") ...
				mkShowMems ssn ms vs ... ss "}")
			[valD (conP sn $ varP <$> vs) (normalB $ varE s) []]]

mkShowMems :: StrName -> [MemName] -> [Name] -> ExpQ
mkShowMems (toLabel -> l) ms vs = foldr (...) (varE 'id) . intersperse (ss ", ")
	$ (<$> zip ms vs) \(m, v) ->
		l m ..+ " = " ... varE 'showsPrec `appE` intE 0 `appE` varE v

-- Read

mkInstanceRead :: StrName -> [MemName] -> DecQ
mkInstanceRead sn ms = length ms `replicateM` newName "v" >>= \vs ->
	instanceD (cxt []) (conT ''Read `appT` t) . (: [])
		$ valD (varP 'readPrec) (normalB $ varE 'parens
			.$ varE 'prec `appE` intE 10 `appE` doE ([
				conP 'Ident [strP sn] `bindS` varE 'lexP,
				conP 'Punc [strP "{"] `bindS` varE 'lexP ] ++
				mkReadMems sn ms vs ++ [
				conP 'Punc [strP "}"] `bindS` varE 'lexP,
				noBindS $ varE 'pure
					.$ foldl appE c (varE <$> vs) ])) []
	where t = conT $ mkName sn; c = conE $ mkName sn

mkReadMems :: StrName -> [MemName] -> [Name] -> [StmtQ]
mkReadMems sn ms vs =
	intercalate [conP 'Punc [strP ","] `bindS` varE 'lexP]
		$ (<$> zip ms vs) \(m, v) -> [
			conP 'Ident [strP $ toLabel sn m] `bindS` varE 'lexP,
			conP 'Punc [strP "="] `bindS` varE 'lexP,
			varP v `bindS` (varE 'step `appE` varE 'readPrec) ]

-- Eq

mkInstanceEq :: StrName -> [MemName] -> DecQ
mkInstanceEq sn ms = (,) <$> newName "s" <*> newName "t" >>= \(s, t) ->
	instanceD (cxt []) (conT ''Eq `appT` conT (mkName sn)) . (: [])
		. funD '(==) . (: []) $ clause [varP s, varP t] (normalB
			$ foldl (.&&) (conE 'True) $ mkMemEq sn s t <$> ms) []

mkMemEq :: StrName -> Name -> Name -> MemName -> ExpQ
mkMemEq sn (varE -> s) (varE -> t) m = let l = varE . mkName $ toLabel sn m in
	l `appE` s .== l `appE` t

-- Ord

mkInstanceOrd :: StrName -> [MemName] -> DecQ
mkInstanceOrd sn ms = (,) <$> newName "s" <*> newName "t" >>= \(s, t) ->
	instanceD (cxt []) (conT ''Ord `appT` conT (mkName sn)) . (: [])
		. funD '(<=) . (: []) $ clause [varP s, varP t]
			(normalB $ compareAllMember
				(varE . mkName . toLabel sn <$> ms)
				(varE s) (varE t)) []

{-
		(
			normalB $ varE 'foldr `appE` lamOrd s t `appE`
				conE 'True `appE` listE ln ) []
	where ln = varE . mkName . toLabel sn <$> ms

lamOrd :: Name -> Name -> ExpQ
lamOrd (varE -> s) (varE -> t) =
	(,) <$> newName "x" <*> newName "v" >>= \(x, v) -> let xe = varE x in
		lamE [varP x, varP v] $ xe `appE` s .< xe `appE` t .||
			xe `appE` s .== xe `appE` t .&& varE v
-}

-- Bounded

mkInstanceBounded :: StrName -> [MemName] -> DecQ
mkInstanceBounded (mkName -> sn) (length -> n) =
	instanceD (cxt []) (conT ''Bounded `appT` conT sn) [
		valD (varP 'minBound) (normalB $ foldl appE (conE sn)
			(replicate n $ varE 'minBound)) [],
		valD (varP 'maxBound) (normalB $ foldl appE (conE sn)
			(replicate n $ varE 'maxBound)) [] ]

-- Ix

mkInstanceIx :: StrName -> [MemName] -> DecQ
mkInstanceIx (mkName -> sn) ms = instanceD (cxt []) (conT ''Ix `appT` conT sn) [
	mkRange 'range sn ms, mkIndex 'index sn ms, mkInRange 'inRange sn ms ]

mkRange :: Name -> Name -> [MemName] -> DecQ
mkRange fn sn (length -> n) = do
	(vs, ws, is) <- unzip3 <$> n `replicateM`
		((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn . (: []) $ clause
		[tupP [conP sn $ varP <$> vs, conP sn $ varP <$> ws]]
		(normalB . compE . (++ [noBindS . foldl appE sne $ varE <$> is])
			$ (<$> is `zip` (vs `zip` ws)) \(i, (v, w)) ->
				bindS (varP i) $ rg `appE` tupE [varE v, varE w]
			) []
	where rg = varE 'range; sne = conE sn

mkIndex :: Name -> Name -> [MemName] -> DecQ
mkIndex fn (conP -> sn) (length -> n) = do
	(vs, ws, is) <- unzip3 <$> n `replicateM`
		((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn . (: []) $ clause
		[tupP [sn $ varP <$> vs, sn $ varP <$> ws], sn $ varP <$> is]
		(normalB $ varE 'foldl `appE` mkIndexLam `appE` intE 0
			.$ listE (varE <$> vs) `zp` listE (varE <$> ws) `zp`
				listE (varE <$> is)) []

mkIndexLam :: ExpQ
mkIndexLam =
	(,,) <$> newName "v" <*> newName "z" <*> newName "k" >>= \(v, z, k) ->
		lamE [varP v, tupP [varP z, varP k]]
			$ (varE 'index `appE` varE z `appE` varE k) .+
				(varE 'rangeSize `appE` varE z .* varE v)

mkInRange :: Name -> Name -> [MemName] -> DecQ
mkInRange fn (conP -> sn) (length -> n) = do
	(vs, ws, is) <- unzip3 <$> n `replicateM`
		((,,) <$> newName "v" <*> newName "w" <*> newName "i")
	funD fn . (: []) $ clause
		[tupP [sn $ varP <$> vs, sn $ varP <$> ws], sn $ varP <$> is]
		(normalB . foldr (.&&) (conE 'True) $
			(<$> vs `zip` ws `zip` is) \((v, w), i) ->
				ir `appE` tupE [varE v, varE w] `appE` varE i
			) []
	where ir = varE 'inRange

---------------------------------------------------------------------------
-- STRUCT WITH PRIMITIVE MONAD
---------------------------------------------------------------------------

-- FUNCTION STRUCT PRIM

structPrim :: StrName -> FunCopy -> FunFree -> [DerivClass] -> DecsQ
structPrim nt cp fr ds = sequence [
	mkNewtypePrim nt ds, mkTypeST nt, mkTypeIO nt,
	mkFreezeSig nt, mkFreezeFun nt cp fr, mkThawSig nt, mkThawFun nt cp fr,
	mkCopySig nt, mkCopyFun nt cp fr ]

-- ^
-- Example
--
-- @
-- foreign import ccall "foo_copy" c_foo_copy :: Ptr Foo -> IO (Ptr Foo)
-- foreign import ccall "foo_free" c_foo_free :: Ptr Foo -> IO ()
--
-- structPrim "Foo" 'c_foo_copy 'c_foo_free [''Show]
-- @

type FunCopy = Name; type FunFree = Name

-- NEWTYPE AND TYPE SYNONYM

mkNewtypePrim :: StrName -> [DerivClass] -> DecQ
mkNewtypePrim sn ds = newName "s" >>= \s ->
	newtypeD (cxt []) snp [plainTV s] Nothing
		(normalC snp . (: []) $ bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT ''ForeignPtr `appT` conT (mkName sn)))
		[derivClause Nothing $ conT <$> ds]
	where snp = mkName $ sn ++ "Prim"

mkTypeIO :: StrName -> DecQ
mkTypeIO sn = tySynD (mkName $ sn ++ "IO") []
	$ conT (mkName $ sn ++ "Prim") `appT` conT ''RealWorld

mkTypeST :: StrName -> DecQ
mkTypeST sn = tySynD (mkName $ sn ++ "ST") [] . conT . mkName $ sn ++ "Prim"

-- FREEZE

mkFreezeSig :: StrName -> DecQ
mkFreezeSig sn = newName "m" >>= \m ->
	sigD fn . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT snp `appT` (conT ''PrimState `appT` varT m) .->
			varT m `appT` conT (mkName sn)
	where fn = mkName $ lcfirst sn ++ "Freeze"; snp = mkName $ sn ++ "Prim"

mkFreezeFun :: StrName -> FunCopy -> FunFree -> DecQ
mkFreezeFun sn cp fr = newName "fp" >>= \fp ->
	funD (mkName $ lcfirst sn ++ "Freeze") . (: []) $
		clause [conP (mkName $ sn ++ "Prim") [varP fp]] (normalB
			$ mkFreezeBody sn cp fr fp) []

mkFreezeBody :: StrName -> FunCopy -> FunFree -> Name -> ExpQ
mkFreezeBody sn cp fr fp =
	varE 'unsafeIOToPrim ... conE (mkName $ sn ++ "_") `pt` varE '(<$>)
		.$ varE 'withForeignPtr `appE` varE fp `appE` varE cp
			.>>= varE 'newForeignPtr .<$> varE 'id .<*> varE fr

-- THAW

mkThawSig :: StrName -> DecQ
mkThawSig sn = newName "m" >>= \m ->
	sigD fn . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT (mkName sn) .-> varT m `appT`
			(conT snp `appT` (conT ''PrimState `appT` varT m))
	where fn = mkName $ lcfirst sn ++ "Thaw"; snp = mkName $ sn ++ "Prim"

mkThawFun :: StrName -> FunCopy -> FunFree -> DecQ
mkThawFun sn cp fr = newName "fp" >>= \fp ->
	funD (mkName $ lcfirst sn ++ "Thaw") . (: [])
		$ clause [conP (mkName $ sn ++ "_") [varP fp]] (
			normalB $ mkThawBody sn cp fr fp) []

mkThawBody :: StrName -> FunCopy -> FunFree -> Name -> ExpQ
mkThawBody sn cp fr fp =
	varE 'unsafeIOToPrim ... conE (mkName $ sn ++ "Prim") `pt` varE '(<$>)
		.$ varE 'withForeignPtr `appE` varE fp `appE` varE cp
			.>>= varE 'newForeignPtr .<$> varE 'id .<*> varE fr

-- COPY

mkCopySig :: StrName -> DecQ
mkCopySig sn = newName "m" >>= \m ->
	sigD fn . forallT [] (cxt [conT ''PrimMonad `appT` varT m])
		$ conT snp `appT` (conT ''PrimState `appT` varT m) .->
			varT m `appT` (conT snp `appT`
				(conT ''PrimState `appT` varT m))
	where fn = mkName $ lcfirst sn ++ "Copy"; snp = mkName $ sn ++ "Prim"

mkCopyFun :: StrName -> FunCopy -> FunFree -> DecQ
mkCopyFun sn cp fr = newName "fp" >>= \fp ->
	funD (mkName $ lcfirst sn ++ "Copy") . (: [])
		$ clause [conP (mkName $ sn ++ "Prim") [varP fp]] (normalB
			$ mkCopyBody sn cp fr fp) []

mkCopyBody :: StrName -> FunCopy -> FunFree -> Name -> ExpQ
mkCopyBody sn cp fr fp =
	varE 'unsafeIOToPrim ... conE (mkName $ sn ++ "Prim") `pt` varE '(<$>)
		.$ varE 'withForeignPtr `appE` varE fp `appE` varE cp
			.>>= varE 'newForeignPtr .<$> varE 'id .<*> varE fr

-- DERIVE STORABLE

deriveStorable :: String -> Integer -> Integer -> DecQ
deriveStorable n sz algn = do
	[ps, pd, pd', fps', ps'] <-
		newName `mapM` ["ps", "pd", "pd", "fps", "ps"]
	instanceD (cxt []) (appT (conT ''Storable) (conT tp)) [
		funD 'sizeOf [clause [wildP] (normalB . litE $ integerL sz) []],
		funD 'alignment
			[clause [wildP] (normalB . litE $ integerL algn) []],
		funD 'peek [clause [varP ps] (normalB $ doE [
			bindS (varP pd) $ varE 'malloc,
			noBindS $ varE 'copyBytes `appE` varE pd `appE`
				varE ps `appE` litE (integerL sz),
			noBindS . infixE (Just $ conE dc) (varE '(<$>))
				. Just $ varE 'newForeignPtr
					`appE` varE pd
					`appE` (varE 'free `appE` varE pd)
			]) [] ],
		funD 'poke [clause [varP pd', conP dc [varP fps']] (normalB
			$ varE 'withForeignPtr
				`appE` varE fps' `appE` (lamE [varP ps']
					$ varE 'copyBytes `appE` varE pd'
						`appE` varE ps'
						`appE` litE (integerL sz))) []]
		]
	where tp = mkName n; dc = mkName $ n ++ "_"
