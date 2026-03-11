{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foreign.C.Enum (enum, enumMems) where

import Language.Haskell.TH (
	Name, mkName, newName, Lit(..), clause, cxt, normalB,
	DecsQ, DecQ, valD, funD, instanceD,
	patSynSigD, patSynD, prefixPatSyn, explBidir,
	newtypeD, normalC, derivClause,
	ExpQ, varE, conE, litE, sigE, appE, infixE, listE, lamCaseE,
	conT, appT, varP, conP, litP, wildP, match,
	doE, bindS, noBindS,
	bangType, bang, noSourceUnpackedness, noSourceStrictness,
	TypeQ, sigD, arrowT )
import Foreign.Ptr
import Foreign.Storable
import Control.Arrow (first)
import Data.Bool (bool)
import Data.Maybe (isJust, listToMaybe)
import Data.List (partition)
import Text.Read (readPrec, Lexeme(..), step, choice, prec, parens, lexP)

enum :: String -> Name -> [Name] -> [(String, Integer)] -> DecsQ
enum nt t ds nvs = (\n s r st ms unsf -> n : s (r (st ms)) ++ unsf)
	<$> mkNewtype nt t ds'
	<*> bool (pure id) ((:) <$> mkShow nt ns) bs
	<*> bool (pure id) ((:) <$> mkRead nt ns) br
	<*> bool (pure id)
		((:) <$> deriveStorable (mkName nt) t) bst
	<*> enumMems nt nvs
	<*> unSigFun nt t
	where ShowReadClasses bs br bst ds' = showReadClasses ds; ns = fst <$> nvs

{- ^

Write like the following.

@
enum \"Foo\" ''Int [''Show, ''Read, ''Eq] [
	(\"FooError\", - 1),
	(\"FooZero\", 0),
	(\"FooOne\", 1),
	(\"FooTwo\", 2) ]
@

Then you get like the following.

@
newtype Foo = Foo Int deriving Eq

pattern FooError :: Int -> Foo
pattern FooError <- Foo (- 1) where
	FooError = Foo (- 1)

pattern FooZero :: Int -> Foo
...


instance Show Foo where
	showsPrec = ...

instance Read Foo where
	readPrec = ...
@

And you can read and show like the following.

@
> Foo $ - 1
FooError
> FooTwo
FooTwo
> Foo 3
Foo 3
> read "Foo (- 1)" :: Foo
FooError
> read \"FooOne\" :: Foo
FooOne
@

-}

data ShowReadClasses = ShowReadClasses {
	showReadClassesShow :: Bool,
	showReadClassesRead :: Bool,
	showReadClassesStorable :: Bool,
	showReadClassesClasses :: [Name] } deriving Show

showReadClasses :: [Name] -> ShowReadClasses
showReadClasses ns = ShowReadClasses (isJust s) (isJust r) (isJust st) ns'''
	where
	(s, ns') = popIt ''Show ns
	(r, ns'') = popIt ''Read ns'
	(st, ns''') = popIt ''Storable ns''

popIt :: Eq a => a -> [a] -> (Maybe a, [a])
popIt x = (listToMaybe `first`) . partition (== x)

mkNewtype :: String -> Name -> [Name] -> DecQ
mkNewtype nt t ds = newtypeD (cxt []) (mkName nt) [] Nothing
	(normalC (mkName nt)
		[bangType
			(bang noSourceUnpackedness noSourceStrictness)
			(conT t)])
	[derivClause Nothing $ conT <$> ds]

enumMems :: String -> [(String, Integer)] -> DecsQ
enumMems t nvs = concat <$> uncurry (mkMember (mkName t)) `mapM` nvs

{- ^

You can define enum members separately.

@
enumMems \"Foo\" [
	(\"FooThree\", 3),
	(\"FooFour\", 4) ]
@

-}

mkMember :: Name -> String -> Integer -> DecsQ
mkMember t n v = sequence [
	patSynSigD (mkName n) (conT t),
	patSynD (mkName n) (prefixPatSyn [])
		(explBidir [flip (clause []) []
			. normalB $ conE t `appE` litE (IntegerL v)])
		(conP t [litP (IntegerL v)]) ]

mkShow :: String -> [String] -> DecQ
mkShow t ns = instanceD (cxt [])
	(conT ''Show `appT` conT (mkName t)) [defineShowsPrec t ns]

defineShowsPrec :: String -> [String] -> DecQ
defineShowsPrec t ns = newName `mapM` ["d", "n"] >>= \[d, n] ->
	funD 'showsPrec [clause [varP d] (normalB (lamCaseE (
		(named <$> ns) ++
		[match (conP (mkName t) [varP n]) (normalB $ sw d n) []] ))) []]
	where
	named f = flip (match $ conP (mkName f) []) [] 
		. normalB $ litE (StringL f) `p` varE '(++)
	sw d n = varE 'showParen `appE` (varE d .> litE (IntegerL 10))
		.$ ((litE (StringL $ t ++ " ") `p` varE '(++)) ...
			(varE 'showsPrec `appE` litE (IntegerL 11) `appE` varE n))

mkRead :: String -> [String] -> DecQ
mkRead t ns = instanceD (cxt []) (conT ''Read `appT` conT (mkName t)) . (: [])
	$ valD (varP 'readPrec) (normalB $ varE 'parens .$ (varE 'choice `appE` listE (
		(named <$> ns) ++
		[varE 'prec `appE` litE (IntegerL 10) `appE` doE [
			bindS (conP 'Ident [litP $ StringL t]) $ varE 'lexP,
			noBindS $ conE (mkName t) .<$> (varE 'step `appE` varE 'readPrec) ]]
		))) []
	where
	named n = doE [
		bindS (conP 'Ident [litP $ StringL n]) $ varE 'lexP,
		noBindS $ varE 'pure `appE` conE (mkName n) ]

(...), (.$), (.<$>), (.>), p :: ExpQ -> ExpQ -> ExpQ
e1 ... e2 = infixE (Just e1) (varE '(.)) (Just e2)
e1 .$ e2 = infixE (Just e1) (varE '($)) (Just e2)
e1 .<$> e2 = infixE (Just e1) (varE '(<$>)) (Just e2)
e1 .> e2 = infixE (Just e1) (varE '(>)) (Just e2)
ex `p` op = infixE (Just ex) op Nothing

deriveStorable :: Name -> Name -> DecQ
deriveStorable drv org = newName `mapM` ["p", "p", "x"] >>= \[pnt, pnt', x] ->
	instanceD (cxt []) (appT (conT ''Storable) (conT drv)) [
		funD 'sizeOf [clause [wildP]
			(normalB $ varE 'sizeOf `appE`
				(varE 'undefined `sigE` conT org))
			[]],
		funD 'alignment [clause [wildP]
			(normalB $ varE 'alignment `appE`
				(varE 'undefined `sigE` conT org))
			[]],
		funD 'peek [clause [varP pnt]
			(normalB $ infixE (Just $ conE drv) (varE '(<$>)) . Just
				$ varE 'peek `appE`
					(varE 'castPtr `appE` varE pnt))
			[]],
		funD 'poke [clause [varP pnt', conP drv [varP x]]
			(normalB $ varE 'poke `appE`
				(varE 'castPtr `appE` varE pnt') `appE` varE x)
			[]] ]

unSigFun :: String -> Name -> DecsQ
unSigFun en tp = (\s f -> [s, f]) <$> unSig en tp <*> unFun en

unSig :: String -> Name -> DecQ
unSig en tp = sigD (mkName $ "un" ++ en) $ conT (mkName en) `arrT` conT tp

unFun :: String -> DecQ
unFun en = do
	x <- newName "x"
	funD (mkName $ "un" ++ en) [
		clause [conP (mkName en) [varP x]] (normalB (varE x)) []
		]

arrT :: TypeQ -> TypeQ -> TypeQ
t1 `arrT` t2 = arrowT `appT` t1 `appT` t2
