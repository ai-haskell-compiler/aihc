{-# LANGUAGE CPP, FlexibleInstances, UndecidableInstances, OverlappingInstances, OverloadedStrings, TypeFamilies, RankNTypes, DeriveDataTypeable, StandaloneDeriving, FlexibleContexts, TypeSynonymInstances, ScopedTypeVariables, GADTs
           , GeneralizedNewtypeDeriving, LambdaCase #-}

-----------------------------------------------------------------------------
{- |
Module      :  Language.Javascript.JMacro.Base
Copyright   :  (c) Gershom Bazerman, 2009
License     :  BSD 3 Clause
Maintainer  :  gershomb@gmail.com
Stability   :  experimental

Simple DSL for lightweight (untyped) programmatic generation of Javascript.
-}
-----------------------------------------------------------------------------

module Language.Javascript.JMacro.Base (
  -- * ADT
  JStat(..), JExpr(..), JVal(..), Ident(..), IdentSupply(..), JsLabel,
  -- * Generic traversal (via compos)
  JMacro(..), JMGadt(..), Compos(..),
  composOp, composOpM, composOpM_, composOpFold,
  -- * Hygienic transformation
  withHygiene, scopify,
  -- * Display/Output
  renderJs, renderPrefixJs, JsToDoc(..),
  -- * Ad-hoc data marshalling
  ToJExpr(..),
  -- * Literals
  jsv,
  -- * Occasionally helpful combinators
  jLam, jVar, jVarTy, jFor, jForIn, jForEachIn, jTryCatchFinally,
  expr2stat, ToStat(..), nullStat,
  -- * Hash combinators
  jhEmpty, jhSingle, jhAdd, jhFromList,
  -- * Utility
  jsSaturate, jtFromList, SaneDouble(..)
  ) where
import Prelude hiding (tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read)
import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad (ap, return, liftM2)
import Control.Monad.State.Strict
import Control.Monad.Identity

import Data.Function
import Data.Char (toLower,isControl)
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text as TS
import Data.Generics
import Data.Monoid(Monoid, mappend, mempty)
import Data.Semigroup(Semigroup(..))

import Numeric(showHex)
import Safe
import Data.Aeson
import qualified Data.Vector as V
#if MIN_VERSION_aeson (2,0,0)
import qualified Data.Aeson.Key    as KM
import qualified Data.Aeson.KeyMap as KM
#else
import qualified Data.HashMap.Strict as HM
#endif
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

import qualified Text.PrettyPrint.Leijen.Text as PP

import Language.Javascript.JMacro.Types

-- wl-pprint-text compatibility with pretty
infixl 5 $$, $+$
($+$), ($$), ($$$) :: Doc -> Doc -> Doc
x $+$ y = x PP.<$> y
x $$ y  = align (x $+$ y)
x $$$ y = align (nest 2 $ x $+$ y)

{--------------------------------------------------------------------
  ADTs
--------------------------------------------------------------------}

newtype IdentSupply a = IS {runIdentSupply :: State [Ident] a} deriving Typeable

inIdentSupply :: (State [Ident] a -> State [Ident] b) -> IdentSupply a -> IdentSupply b
inIdentSupply f x = IS $ f (runIdentSupply x)

instance Data a => Data (IdentSupply a) where
    gunfold _ _ _ = error "gunfold IdentSupply"
    toConstr _ = error "toConstr IdentSupply"
    dataTypeOf _ = mkNoRepType "IdentSupply"

instance Functor IdentSupply where
    fmap f x = inIdentSupply (fmap f) x

takeOne :: State [Ident] Ident
takeOne = do
  get >>= \case
            (x:xs) -> do
               put xs
               return x
            _ -> error "not enough elements"

newIdentSupply :: Maybe String -> [Ident]
newIdentSupply Nothing     = newIdentSupply (Just "jmId")
newIdentSupply (Just pfx') = [StrI (pfx ++ show x) | x <- [(0::Integer)..]]
    where pfx = pfx'++['_']

sat_ :: IdentSupply a -> a
sat_ x = evalState (runIdentSupply x) $ newIdentSupply (Just "<<unsatId>>")

instance Eq a => Eq (IdentSupply a) where
    (==) = (==) `on` sat_
instance Ord a => Ord (IdentSupply a) where
    compare = compare `on` sat_
instance Show a => Show (IdentSupply a) where
    show x = "(" ++ show (sat_ x) ++ ")"


--switch
--Yield statement?
--destructuring/pattern matching functions --pattern matching in lambdas.
--array comprehensions/generators?
--add postfix stat

-- | Statements
data JStat = DeclStat   Ident (Maybe JLocalType)
           | ReturnStat JExpr
           | IfStat     JExpr JStat JStat
           | WhileStat  Bool JExpr JStat -- bool is "do"
           | ForInStat  Bool Ident JExpr JStat -- bool is "each"
           | SwitchStat JExpr [(JExpr, JStat)] JStat
           | TryStat    JStat Ident JStat JStat
           | BlockStat  [JStat]
           | ApplStat   JExpr [JExpr]
           | PPostStat  Bool String JExpr
           | AssignStat JExpr JExpr
           | UnsatBlock (IdentSupply JStat)
           | AntiStat   String
           | ForeignStat Ident JLocalType
           | LabelStat JsLabel JStat
           | BreakStat (Maybe JsLabel)
           | ContinueStat (Maybe JsLabel)
             deriving (Eq, Ord, Show, Data, Typeable)

type JsLabel = String


instance Semigroup JStat where
    (<>) (BlockStat xs) (BlockStat ys) = BlockStat $ xs ++ ys
    (<>) (BlockStat xs) ys = BlockStat $ xs ++ [ys]
    (<>) xs (BlockStat ys) = BlockStat $ xs : ys
    (<>) xs ys = BlockStat [xs,ys]


instance Monoid JStat where
    mempty = BlockStat []
    mappend x y = x <> y


-- TODO: annotate expressions with type
-- | Expressions
data JExpr = ValExpr    JVal
           | SelExpr    JExpr Ident
           | IdxExpr    JExpr JExpr
           | InfixExpr  String JExpr JExpr
           | PPostExpr  Bool String JExpr
           | IfExpr     JExpr JExpr JExpr
           | NewExpr    JExpr
           | ApplExpr   JExpr [JExpr]
           | UnsatExpr  (IdentSupply JExpr)
           | AntiExpr   String
           | TypeExpr   Bool JExpr JLocalType
             deriving (Eq, Ord, Show, Data, Typeable)

-- | Values
data JVal = JVar     Ident
          | JList    [JExpr]
          | JDouble  SaneDouble
          | JInt     Integer
          | JStr     String
          | JRegEx   String
          | JHash    (M.Map String JExpr)
          | JFunc    [Ident] JStat
          | UnsatVal (IdentSupply JVal)
            deriving (Eq, Ord, Show, Data, Typeable)

newtype SaneDouble = SaneDouble Double deriving (Data, Typeable, Fractional, Num)

instance Eq SaneDouble where
    (SaneDouble x) == (SaneDouble y) = x == y || (isNaN x && isNaN y)

instance Ord SaneDouble where
    compare (SaneDouble x) (SaneDouble y) = compare (fromNaN x) (fromNaN y)
        where fromNaN z | isNaN z = Nothing
                        | otherwise = Just z

instance Show SaneDouble where
    show (SaneDouble x) = show x

-- | Identifiers
newtype Ident = StrI String deriving (Eq, Ord, Show, Data, Typeable)


--deriving instance Typeable2 (StateT [Ident] Identity)
--deriving instance Data (State [Ident] JVal)
--deriving instance Data (State [Ident] JExpr)
--deriving instance Data (State [Ident] JStat)



expr2stat :: JExpr -> JStat
expr2stat (ApplExpr x y) = (ApplStat x y)
expr2stat (IfExpr x y z) = IfStat x (expr2stat y) (expr2stat z)
expr2stat (PPostExpr b s x) = PPostStat b s x
expr2stat (AntiExpr x) = AntiStat x
expr2stat _ = nullStat


{--------------------------------------------------------------------
  Compos
--------------------------------------------------------------------}
-- | Compos and ops for generic traversal as defined over
-- the JMacro ADT.

-- | Utility class to coerce the ADT into a regular structure.
class JMacro a where
    jtoGADT :: a -> JMGadt a
    jfromGADT :: JMGadt a -> a

instance JMacro Ident where
    jtoGADT = JMGId
    jfromGADT (JMGId x) = x
    jfromGADT _ = error "impossible"

instance JMacro JStat where
    jtoGADT = JMGStat
    jfromGADT (JMGStat x) = x
    jfromGADT _ = error "impossible"

instance JMacro JExpr where
    jtoGADT = JMGExpr
    jfromGADT (JMGExpr x) = x
    jfromGADT _ = error "impossible"

instance JMacro JVal where
    jtoGADT = JMGVal
    jfromGADT (JMGVal x) = x
    jfromGADT _ = error "impossible"

-- | Union type to allow regular traversal by compos.
data JMGadt a where
    JMGId   :: Ident -> JMGadt Ident
    JMGStat :: JStat -> JMGadt JStat
    JMGExpr :: JExpr -> JMGadt JExpr
    JMGVal  :: JVal  -> JMGadt JVal


composOp :: Compos t => (forall a. t a -> t a) -> t b -> t b
composOp f = runIdentity . composOpM (Identity . f)
composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t b -> m (t b)
composOpM = compos return ap
composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t b -> m ()
composOpM_ = composOpFold (return ()) (>>)
composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)
newtype C b a = C { unC :: b }

class Compos t where
    compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
           -> (forall a. t a -> m (t a)) -> t c -> m (t c)

instance Compos JMGadt where
    compos = jmcompos

jmcompos :: forall m c. (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b) -> (forall a. JMGadt a -> m (JMGadt a)) -> JMGadt c -> m (JMGadt c)
jmcompos ret app f' v =
    case v of
     JMGId _ -> ret v
     JMGStat v' -> ret JMGStat `app` case v' of
           DeclStat i t -> ret DeclStat `app` f i `app` ret t
           ReturnStat i -> ret ReturnStat `app` f i
           IfStat e s s' -> ret IfStat `app` f e `app` f s `app` f s'
           WhileStat b e s -> ret (WhileStat b) `app` f e `app` f s
           ForInStat b i e s -> ret (ForInStat b) `app` f i `app` f e `app` f s
           SwitchStat e l d -> ret SwitchStat `app` f e `app` l' `app` f d
               where l' = mapM' (\(c,s) -> ret (,) `app` f c `app` f s) l
           BlockStat xs -> ret BlockStat `app` mapM' f xs
           ApplStat  e xs -> ret ApplStat `app` f e `app` mapM' f xs
           TryStat s i s1 s2 -> ret TryStat `app` f s `app` f i `app` f s1 `app` f s2
           PPostStat b o e -> ret (PPostStat b o) `app` f e
           AssignStat e e' -> ret AssignStat `app` f e `app` f e'
           UnsatBlock _ -> ret v'
           AntiStat _ -> ret v'
           ForeignStat i t -> ret ForeignStat `app` f i `app` ret t
           ContinueStat l -> ret (ContinueStat l)
           BreakStat l -> ret (BreakStat l)
           LabelStat l s -> ret (LabelStat l) `app` f s
     JMGExpr v' -> ret JMGExpr `app` case v' of
           ValExpr e -> ret ValExpr `app` f e
           SelExpr e e' -> ret SelExpr `app` f e `app` f e'
           IdxExpr e e' -> ret IdxExpr `app` f e `app` f e'
           InfixExpr o e e' -> ret (InfixExpr o) `app` f e `app` f e'
           PPostExpr b o e -> ret (PPostExpr b o) `app` f e
           IfExpr e e' e'' -> ret IfExpr `app` f e `app` f e' `app` f e''
           NewExpr e -> ret NewExpr `app` f e
           ApplExpr e xs -> ret ApplExpr `app` f e `app` mapM' f xs
           AntiExpr _ -> ret v'
           TypeExpr b e t -> ret (TypeExpr b) `app` f e `app` ret t
           UnsatExpr _ -> ret v'
     JMGVal v' -> ret JMGVal `app` case v' of
           JVar i -> ret JVar `app` f i
           JList xs -> ret JList `app` mapM' f xs
           JDouble _ -> ret v'
           JInt    _ -> ret v'
           JStr    _ -> ret v'
           JRegEx  _ -> ret v'
           JHash   m -> ret JHash `app` m'
               where (ls, vs) = unzip (M.toList m)
                     m' = ret (M.fromAscList . zip ls) `app` mapM' f vs
           JFunc xs s -> ret JFunc `app` mapM' f xs `app` f s
           UnsatVal _ -> ret v'

  where
    mapM' :: forall a. (a -> m a) -> [a] -> m [a]
    mapM' g = foldr (app . app (ret (:)) . g) (ret [])
    f :: forall b. JMacro b => b -> m b
    f x = ret jfromGADT `app` f' (jtoGADT x)

{--------------------------------------------------------------------
  New Identifiers
--------------------------------------------------------------------}

class ToSat a where
    toSat_ :: a -> [Ident] -> IdentSupply (JStat, [Ident])

instance ToSat [JStat] where
    toSat_ f vs = IS $ return $ (BlockStat f, reverse vs)

instance ToSat JStat where
    toSat_ f vs = IS $ return $ (f, reverse vs)

instance ToSat JExpr where
    toSat_ f vs = IS $ return $ (expr2stat f, reverse vs)

instance ToSat [JExpr] where
    toSat_ f vs = IS $ return $ (BlockStat $ map expr2stat f, reverse vs)

instance (ToSat a, b ~ JExpr) => ToSat (b -> a) where
    toSat_ f vs = IS $ do
      x <- takeOne
      runIdentSupply $ toSat_ (f (ValExpr $ JVar x)) (x:vs)

{-
splitIdentSupply :: ([Ident] -> ([Ident], [Ident]))
splitIdentSupply is = (takeAlt is, takeAlt (drop 1 is))
    where takeAlt (x:_:xs) = x : takeAlt xs
          takeAlt _ = error "splitIdentSupply: stream is not infinite"
-}

{--------------------------------------------------------------------
  Saturation
--------------------------------------------------------------------}

-- | Given an optional prefix, fills in all free variable names with a supply
-- of names generated by the prefix.
jsSaturate :: (JMacro a) => Maybe String -> a -> a
jsSaturate str x = evalState (runIdentSupply $ jsSaturate_ x) (newIdentSupply str)

jsSaturate_ :: (JMacro a) => a -> IdentSupply a
jsSaturate_ e = IS $ jfromGADT <$> go (jtoGADT e)
    where
      go :: forall a. JMGadt a -> State [Ident] (JMGadt a)
      go v = case v of
               JMGStat (UnsatBlock us) -> go =<< (JMGStat <$> runIdentSupply us)
               JMGExpr (UnsatExpr  us) -> go =<< (JMGExpr <$> runIdentSupply us)
               JMGVal  (UnsatVal   us) -> go =<< (JMGVal  <$> runIdentSupply us)
               _ -> composOpM go v

{--------------------------------------------------------------------
  Transformation
--------------------------------------------------------------------}

--doesn't apply to unsaturated bits
jsReplace_ :: JMacro a => [(Ident, Ident)] -> a -> a
jsReplace_ xs e = jfromGADT $ go (jtoGADT e)
    where
      go :: forall a. JMGadt a -> JMGadt a
      go v = case v of
                   JMGId i -> maybe v JMGId (M.lookup i mp)
                   _ -> composOp go v
      mp = M.fromList xs

--only works on fully saturated things
jsUnsat_ :: JMacro a => [Ident] -> a -> IdentSupply a
jsUnsat_ xs e = IS $ do
  (idents,is') <- splitAt (length xs) <$> get
  put is'
  return $ jsReplace_ (zip xs idents) e

-- | Apply a transformation to a fully saturated syntax tree,
-- taking care to return any free variables back to their free state
-- following the transformation. As the transformation preserves
-- free variables, it is hygienic.
withHygiene ::  JMacro a => (a -> a) -> a -> a
withHygiene f x = jfromGADT $ case jtoGADT x of
    JMGExpr z -> JMGExpr $ UnsatExpr $ inScope z
    JMGStat z -> JMGStat $ UnsatBlock $ inScope z
    JMGVal  z -> JMGVal $ UnsatVal $ inScope z
    JMGId _ -> jtoGADT $ f x
    where
        inScope z = IS $ do
            splitAt 1 `fmap` get >>= \case
              ([StrI a], b) -> do
                put b
                return $ withHygiene_ a f z
              _ -> error "Not as string"

withHygiene_ :: JMacro a => String -> (a -> a) -> a -> a
withHygiene_ un f x = jfromGADT $ case jtoGADT x of
    JMGStat _ -> jtoGADT $ UnsatBlock (jsUnsat_ is' x'')
    JMGExpr _ -> jtoGADT $ UnsatExpr (jsUnsat_ is' x'')
    JMGVal  _ -> jtoGADT $ UnsatVal (jsUnsat_ is' x'')
    JMGId _ -> jtoGADT $ f x
    where
        (x', (StrI l : _)) = runState (runIdentSupply $ jsSaturate_ x) is
        is' = take lastVal is
        x'' = f x'
        lastVal = readNote ("inSat" ++ un) (reverse . takeWhile (/= '_') . reverse $ l) :: Int
        is = newIdentSupply $ Just ("inSat" ++ un)

-- | Takes a fully saturated expression and transforms it to use unique variables that respect scope.
scopify :: JStat -> JStat
scopify x = evalState (jfromGADT <$> go (jtoGADT x)) (newIdentSupply Nothing)
    where go :: forall a. JMGadt a -> State [Ident] (JMGadt a)
          go v = case v of
                   (JMGStat (BlockStat ss)) -> JMGStat . BlockStat <$>
                                             blocks ss
                       where blocks [] = return []
                             blocks (DeclStat (StrI i) t : xs) =  case i of
                                ('!':'!':i') -> (DeclStat (StrI i') t:) <$> blocks xs
                                ('!':i') -> (DeclStat (StrI i') t:) <$> blocks xs
                                _ -> get >>= \case
                                     (newI:st) -> do
                                       put st
                                       rest <- blocks xs
                                       return $ [DeclStat newI t `mappend` jsReplace_ [(StrI i, newI)] (BlockStat rest)]
                                     _ -> error "scopify"
                             blocks (x':xs) = (jfromGADT <$> go (jtoGADT x')) <:> blocks xs
                             (<:>) = liftM2 (:)
                   (JMGStat (ForInStat b (StrI i) e s)) -> get >>= \case
                          (newI:st) -> do
                             put st
                             rest <- jfromGADT <$> go (jtoGADT s)
                             return $ JMGStat . ForInStat b newI e $ jsReplace_ [(StrI i, newI)] rest
                          _ -> error "scopify2"
                   (JMGStat (TryStat s (StrI i) s1 s2)) -> get >>= \case
                          (newI:st) -> do
                            put st
                            t <- jfromGADT <$> go (jtoGADT s)
                            c <- jfromGADT <$> go (jtoGADT s1)
                            f <- jfromGADT <$> go (jtoGADT s2)
                            return . JMGStat . TryStat t newI (jsReplace_ [(StrI i, newI)] c) $ f
                          _ -> error "scopify3"
                   (JMGExpr (ValExpr (JFunc is s))) -> do
                            st <- get
                            let (newIs,newSt) = splitAt (length is) st
                            put (newSt)
                            rest <- jfromGADT <$> go (jtoGADT s)
                            return . JMGExpr . ValExpr $ JFunc newIs $ (jsReplace_ $ zip is newIs) rest
                   _ -> composOpM go v

{--------------------------------------------------------------------
  Pretty Printing
--------------------------------------------------------------------}

-- | Render a syntax tree as a pretty-printable document
-- (simply showing the resultant doc produces a nice,
-- well formatted String).
renderJs :: (JsToDoc a, JMacro a) => a -> Doc
renderJs = jsToDoc . jsSaturate Nothing

-- | Render a syntax tree as a pretty-printable document, using a given prefix to all generated names. Use this with distinct prefixes to ensure distinct generated names between independent calls to render(Prefix)Js.
renderPrefixJs :: (JsToDoc a, JMacro a) => String -> a -> Doc
renderPrefixJs pfx = jsToDoc . jsSaturate (Just $ "jmId_"++pfx)

braceNest :: Doc -> Doc
braceNest x = char '{' <+> nest 2 x $$ char '}'

braceNest' :: Doc -> Doc
braceNest' x = nest 2 (char '{' $+$ x) $$ char '}'

class JsToDoc a
    where jsToDoc :: a -> Doc

instance JsToDoc JStat where
    jsToDoc (IfStat cond x y) = text "if" <> parens (jsToDoc cond) $$ braceNest' (jsToDoc x) $$ mbElse
        where mbElse | y == BlockStat []  = PP.empty
                     | otherwise = text "else" $$ braceNest' (jsToDoc y)
    jsToDoc (DeclStat x t) = text "var" <+> jsToDoc x <> rest
        where rest = case t of
                       Nothing -> text ""
                       Just tp -> text " /* ::" <+> jsToDoc tp <+> text "*/"
    jsToDoc (WhileStat False p b)  = text "while" <> parens (jsToDoc p) $$ braceNest' (jsToDoc b)
    jsToDoc (WhileStat True  p b)  = (text "do" $$ braceNest' (jsToDoc b)) $+$ text "while" <+> parens (jsToDoc p)
    jsToDoc (UnsatBlock e) = jsToDoc $ sat_ e

    jsToDoc (BreakStat l) = maybe (text "break") (((<+>) `on` text) "break" . T.pack) l
    jsToDoc (ContinueStat l) = maybe (text "continue") (((<+>) `on` text) "continue" . T.pack) l
    jsToDoc (LabelStat l s) = text (T.pack l) <> char ':' $$ printBS s
        where
          printBS (BlockStat ss) = vcat $ interSemi $ flattenBlocks ss
          printBS x = jsToDoc x
          interSemi [x] = [jsToDoc x]
          interSemi [] = []
          interSemi (x:xs) = (jsToDoc x <> semi) : interSemi xs

    jsToDoc (ForInStat each i e b) = text txt <> parens (text "var" <+> jsToDoc i <+> text "in" <+> jsToDoc e) $$ braceNest' (jsToDoc b)
        where txt | each = "for each"
                  | otherwise = "for"
    jsToDoc (SwitchStat e l d) = text "switch" <+> parens (jsToDoc e) $$ braceNest' cases
        where l' = map (\(c,s) -> (text "case" <+> parens (jsToDoc c) <> char ':') $$$ (jsToDoc s)) l ++ [text "default:" $$$ (jsToDoc d)]
              cases = vcat l'
    jsToDoc (ReturnStat e) = text "return" <+> jsToDoc e
    jsToDoc (ApplStat e es) = jsToDoc e <> (parens . fillSep . punctuate comma $ map jsToDoc es)
    jsToDoc (TryStat s i s1 s2) = text "try" $$ braceNest' (jsToDoc s) $$ mbCatch $$ mbFinally
        where mbCatch | s1 == BlockStat [] = PP.empty
                      | otherwise = text "catch" <> parens (jsToDoc i) $$ braceNest' (jsToDoc s1)
              mbFinally | s2 == BlockStat [] = PP.empty
                        | otherwise = text "finally" $$ braceNest' (jsToDoc s2)
    jsToDoc (AssignStat i x) = jsToDoc i <+> char '=' <+> jsToDoc x
    jsToDoc (PPostStat isPre op x)
        | isPre = text (T.pack op) <> optParens x
        | otherwise = optParens x <> text (T.pack op)
    jsToDoc (AntiStat s) = text . T.pack $ "`(" ++ s ++ ")`"
    jsToDoc (ForeignStat i t) = text "//foriegn" <+> jsToDoc i <+> text "::" <+> jsToDoc t
    jsToDoc (BlockStat xs) = jsToDoc (flattenBlocks xs)

flattenBlocks :: [JStat] -> [JStat]
flattenBlocks (BlockStat y:ys) = flattenBlocks y ++ flattenBlocks ys
flattenBlocks (y:ys) = y : flattenBlocks ys
flattenBlocks [] = []

optParens :: JExpr -> Doc
optParens x = case x of
                (PPostExpr _ _ _) -> parens (jsToDoc x)
                _ -> jsToDoc x

instance JsToDoc JExpr where
    jsToDoc (ValExpr x) = jsToDoc x
    jsToDoc (SelExpr x y) = cat [jsToDoc x <> char '.', jsToDoc y]
    jsToDoc (IdxExpr x y) = jsToDoc x <> brackets (jsToDoc y)
    jsToDoc (IfExpr x y z) = parens (jsToDoc x <+> char '?' <+> jsToDoc y <+> char ':' <+> jsToDoc z)
    jsToDoc (InfixExpr op x y) = parens $ sep [jsToDoc x, text (T.pack op'), jsToDoc y]
        where op' | op == "++" = "+"
                  | otherwise = op

    jsToDoc (PPostExpr isPre op x)
        | isPre = text (T.pack op) <> optParens x
        | otherwise = optParens x <> text (T.pack op)

    jsToDoc (ApplExpr je xs) = jsToDoc je <> (parens . fillSep . punctuate comma $ map jsToDoc xs)
    jsToDoc (NewExpr e) = text "new" <+> jsToDoc e
    jsToDoc (AntiExpr s) = text . T.pack $ "`(" ++ s ++ ")`"
    jsToDoc (TypeExpr b e t)  = parens $ jsToDoc e <+> text (if b then "/* ::!" else "/* ::") <+> jsToDoc t <+> text "*/"
    jsToDoc (UnsatExpr e) = jsToDoc $ sat_ e

instance JsToDoc JVal where
    jsToDoc (JVar i) = jsToDoc i
    jsToDoc (JList xs) = brackets . fillSep . punctuate comma $ map jsToDoc xs
    jsToDoc (JDouble (SaneDouble d)) = double d
    jsToDoc (JInt i) = integer i
    jsToDoc (JStr s) = text . T.pack $ "\""++encodeJson s++"\""
    jsToDoc (JRegEx s) = text . T.pack $ "/"++s++"/"
    jsToDoc (JHash m)
            | M.null m = text "{}"
            | otherwise = braceNest . fillSep . punctuate comma . map (\(x,y) -> squotes (text (T.pack x)) <> colon <+> jsToDoc y) $ M.toList m
    jsToDoc (JFunc is b) = parens $ text "function" <> parens (fillSep . punctuate comma . map jsToDoc $ is) $$ braceNest' (jsToDoc b)
    jsToDoc (UnsatVal f) = jsToDoc $ sat_ f

instance JsToDoc Ident where
    jsToDoc (StrI s) = text (T.pack s)

instance JsToDoc [JExpr] where
    jsToDoc = vcat . map ((<> semi) . jsToDoc)

instance JsToDoc [JStat] where
    jsToDoc = vcat . map ((<> semi) . jsToDoc)

instance JsToDoc JType where
    jsToDoc JTNum = text "Num"
    jsToDoc JTString = text "String"
    jsToDoc JTBool = text "Bool"
    jsToDoc JTStat = text "()"
    jsToDoc JTImpossible = text "_|_" -- "⊥"
    jsToDoc (JTForall vars t) = text "forall" <+> fillSep  (punctuate comma (map ppRef vars)) <> text "." <+> jsToDoc t
    jsToDoc (JTFunc args ret) = fillSep . punctuate (text " ->") . map ppType $ args' ++ [ret]
        where args'
               | null args = [JTStat]
               | otherwise = args
    jsToDoc (JTList t) = brackets $ jsToDoc t
    jsToDoc (JTMap t) = text "Map" <+> ppType t
    jsToDoc (JTRecord t mp) = braces (fillSep . punctuate comma . map (\(x,y) -> text (T.pack x) <+> text "::" <+> jsToDoc y) $ M.toList mp) <+> text "[" <> jsToDoc t <> text "]"
    jsToDoc (JTFree ref) = ppRef ref
    jsToDoc (JTRigid ref cs) = text "[" <> ppRef ref <> text "]"
{-
        maybe (text "") (text " / " <>)
                  (ppConstraintList . map (\x -> (ref,x)) $ S.toList cs) <>
        text "]"
-}

instance JsToDoc JLocalType where
    jsToDoc (cs,t) = maybe (text "") (<+> text "=> ") (ppConstraintList cs) <> jsToDoc t

ppConstraintList :: Show a => [((Maybe String, a), Constraint)] -> Maybe Doc
ppConstraintList cs
    | null cs = Nothing
    | otherwise = Just . parens . fillSep . punctuate comma $ map go cs
    where
      go (vr,Sub   t') = ppRef vr   <+> text "<:" <+> jsToDoc t'
      go (vr,Super t') = jsToDoc t' <+> text "<:" <+> ppRef vr

ppRef :: Show a => (Maybe String, a) -> Doc
ppRef (Just n,_) = text . T.pack $ n
ppRef (_,i) = text . T.pack $ "t_"++show i

ppType :: JType -> Doc
ppType x@(JTFunc _ _) = parens $ jsToDoc x
ppType x@(JTMap _) = parens $ jsToDoc x
ppType x = jsToDoc x

{--------------------------------------------------------------------
  ToJExpr Class
--------------------------------------------------------------------}


-- | Things that can be marshalled into javascript values.
-- Instantiate for any necessary data structures.
class ToJExpr a where
    toJExpr :: a -> JExpr
    toJExprFromList :: [a] -> JExpr
    toJExprFromList = ValExpr . JList . map toJExpr

instance ToJExpr a => ToJExpr [a] where
    toJExpr = toJExprFromList

instance ToJExpr JExpr where
    toJExpr = id

instance ToJExpr () where
    toJExpr _ = ValExpr $ JList []

instance ToJExpr Bool where
    toJExpr True  = jsv "true"
    toJExpr False = jsv "false"

instance ToJExpr JVal where
    toJExpr = ValExpr

instance ToJExpr a => ToJExpr (M.Map String a) where
    toJExpr = ValExpr . JHash . M.map toJExpr

instance ToJExpr Double where
    toJExpr = ValExpr . JDouble . SaneDouble

instance ToJExpr Int where
    toJExpr = ValExpr . JInt . fromIntegral

instance ToJExpr Integer where
    toJExpr = ValExpr . JInt

instance ToJExpr Char where
    toJExpr = ValExpr . JStr . (:[])
    toJExprFromList = ValExpr . JStr
--        where escQuotes = tailDef "" . initDef "" . show

instance ToJExpr TS.Text where
    toJExpr t = toJExpr (TS.unpack t)

instance ToJExpr T.Text where
    toJExpr t = toJExpr (T.unpack t)


instance (ToJExpr a, ToJExpr b) => ToJExpr (a,b) where
    toJExpr (a,b) = ValExpr . JList $ [toJExpr a, toJExpr b]

instance (ToJExpr a, ToJExpr b, ToJExpr c) => ToJExpr (a,b,c) where
    toJExpr (a,b,c) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c]

instance (ToJExpr a, ToJExpr b, ToJExpr c, ToJExpr d) => ToJExpr (a,b,c,d) where
    toJExpr (a,b,c,d) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c, toJExpr d]
instance (ToJExpr a, ToJExpr b, ToJExpr c, ToJExpr d, ToJExpr e) => ToJExpr (a,b,c,d,e) where
    toJExpr (a,b,c,d,e) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c, toJExpr d, toJExpr e]
instance (ToJExpr a, ToJExpr b, ToJExpr c, ToJExpr d, ToJExpr e, ToJExpr f) => ToJExpr (a,b,c,d,e,f) where
    toJExpr (a,b,c,d,e,f) = ValExpr . JList $ [toJExpr a, toJExpr b, toJExpr c, toJExpr d, toJExpr e, toJExpr f]

instance Num JExpr where
    fromInteger = ValExpr . JInt . fromIntegral
    x + y = InfixExpr "+" x y
    x - y = InfixExpr "-" x y
    x * y = InfixExpr "*" x y
    abs x = ApplExpr (jsv "Math.abs") [x]
    signum x = IfExpr (InfixExpr ">" x 0) 1 (IfExpr (InfixExpr "==" x 0) 0 (-1))

{--------------------------------------------------------------------
  Block Sugar
--------------------------------------------------------------------}

class ToStat a where
    toStat :: a -> JStat

instance ToStat JStat where
    toStat = id

instance ToStat [JStat] where
    toStat = BlockStat

instance ToStat JExpr where
    toStat = expr2stat

instance ToStat [JExpr] where
    toStat = BlockStat . map expr2stat

{--------------------------------------------------------------------
  Combinators
--------------------------------------------------------------------}

-- | Create a new anonymous function. The result is an expression.
-- Usage:
-- @jLam $ \ x y -> {JExpr involving x and y}@
jLam :: (ToSat a) => a -> JExpr
jLam f = ValExpr . UnsatVal . IS $ do
           (block,is) <- runIdentSupply $ toSat_ f []
           return $ JFunc is block

-- | Introduce a new variable into scope for the duration
-- of the enclosed expression. The result is a block statement.
-- Usage:
-- @jVar $ \ x y -> {JExpr involving x and y}@
jVar :: (ToSat a) => a -> JStat
jVar f = UnsatBlock . IS $ do
           (block, is) <- runIdentSupply $ toSat_ f []
           let addDecls (BlockStat ss) =
                  BlockStat $ map (\x -> DeclStat x Nothing) is ++ ss
               addDecls x = x
           return $ addDecls block


-- | Introduce a new variable with optional type into scope for the duration
-- of the enclosed expression. The result is a block statement.
-- Usage:
-- @jVar $ \ x y -> {JExpr involving x and y}@
jVarTy :: (ToSat a) => a -> (Maybe JLocalType) -> JStat
jVarTy f t = UnsatBlock . IS $ do
           (block, is) <- runIdentSupply $ toSat_ f []
           let addDecls (BlockStat ss) =
                  BlockStat $ map (\x -> DeclStat x t) is ++ ss
               addDecls x = x
           return $ addDecls block


-- | Create a for in statement.
-- Usage:
-- @jForIn {expression} $ \x -> {block involving x}@
jForIn :: ToSat a => JExpr -> (JExpr -> a)  -> JStat
jForIn e f = UnsatBlock . IS $ do
               (block, is) <- runIdentSupply $ toSat_ f []
               return $ ForInStat False (headNote "jForIn" is) e block

-- | As with "jForIn" but creating a \"for each in\" statement.
jForEachIn :: ToSat a => JExpr -> (JExpr -> a) -> JStat
jForEachIn e f = UnsatBlock . IS $ do
               (block, is) <- runIdentSupply $ toSat_ f []
               return $ ForInStat True (headNote "jForIn" is) e block

jTryCatchFinally :: (ToSat a) => JStat -> a -> JStat -> JStat
jTryCatchFinally s f s2 = UnsatBlock . IS $ do
                     (block, is) <- runIdentSupply $ toSat_ f []
                     return $ TryStat s (headNote "jTryCatch" is) block s2

jsv :: String -> JExpr
jsv = ValExpr . JVar . StrI

jFor :: (ToJExpr a, ToStat b) => JStat -> a -> JStat -> b -> JStat
jFor before p after b = BlockStat [before, WhileStat False (toJExpr p) b']
    where b' = case toStat b of
                 BlockStat xs -> BlockStat $ xs ++ [after]
                 x -> BlockStat [x,after]

jhEmpty :: M.Map String JExpr
jhEmpty = M.empty

jhSingle :: ToJExpr a => String -> a -> M.Map String JExpr
jhSingle k v = jhAdd k v $ jhEmpty

jhAdd :: ToJExpr a => String -> a -> M.Map String JExpr -> M.Map String JExpr
jhAdd  k v m = M.insert k (toJExpr v) m

jhFromList :: [(String, JExpr)] -> JVal
jhFromList = JHash . M.fromList

jtFromList :: JType -> [(String, JType)] -> JType
jtFromList t y = JTRecord t $ M.fromList y

nullStat :: JStat
nullStat = BlockStat []

-- Aeson instance
instance ToJExpr Value where
    toJExpr Null             = ValExpr $ JVar $ StrI "null"
    toJExpr (Bool b)         = ValExpr $ JVar $ StrI $ map toLower (show b)
    toJExpr (Number n)       = ValExpr $ JDouble $ realToFrac n
    toJExpr (String s)       = ValExpr $ JStr $ TS.unpack s
    toJExpr (Array vs)       = ValExpr $ JList $ map toJExpr $ V.toList vs
#if MIN_VERSION_aeson (2,0,0)
    toJExpr (Object obj)     = ValExpr $ JHash $ M.fromList $ map (KM.toString *** toJExpr) $ KM.toList obj
#else
    toJExpr (Object obj)     = ValExpr $ JHash $ M.fromList $ map (TS.unpack *** toJExpr) $ HM.toList obj
#endif

-------------------------

-- Taken from json package by Sigbjorn Finne.

encodeJson :: String -> String
encodeJson = concatMap encodeJsonChar

encodeJsonChar :: Char -> String
encodeJsonChar '/'  = "\\/"
encodeJsonChar '\b' = "\\b"
encodeJsonChar '\f' = "\\f"
encodeJsonChar '\n' = "\\n"
encodeJsonChar '\r' = "\\r"
encodeJsonChar '\t' = "\\t"
encodeJsonChar '"' = "\\\""
encodeJsonChar '\\' = "\\\\"
encodeJsonChar c
    | not $ isControl c = [c]
    | c < '\x10'   = '\\' : 'u' : '0' : '0' : '0' : hexxs
    | c < '\x100'  = '\\' : 'u' : '0' : '0' : hexxs
    | c < '\x1000' = '\\' : 'u' : '0' : hexxs
    where hexxs = showHex (fromEnum c) "" -- FIXME
encodeJsonChar c = [c]
