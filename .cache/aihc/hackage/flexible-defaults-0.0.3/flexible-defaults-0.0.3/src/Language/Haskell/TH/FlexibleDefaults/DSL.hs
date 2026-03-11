{-# LANGUAGE GeneralizedNewtypeDeriving, CPP #-}

module Language.Haskell.TH.FlexibleDefaults.DSL where

#if !(MIN_VERSION_base(4,8,0))
-- starting with base-4.8, Applicative is rexported from Prelude
import Control.Applicative
#endif
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.List
import Data.Semigroup as Semigroup
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.FlexibleDefaults.Solve

-- newtype wrapper for Problem, because the default implementation of Monoid
-- (@mappend = union@) is not the one we want here; we want
-- @mappend = unionWith mappend@
newtype Impls s = Impls { unImpls :: M.Map String [ImplSpec s] }

instance Functor Impls where
    fmap f (Impls m) = Impls (M.map (map (fmap f)) m)

instance Semigroup.Semigroup (Impls s) where
     (<>) (Impls x) (Impls y) = Impls (M.unionWith mappend x y)

instance Monoid (Impls s) where
    mempty = Impls mempty
    mappend = (Semigroup.<>)

-- |A description of a system of 'Function's and default 'Implementation's 
-- which can be used to complete a partial implementation of some type class.
newtype Defaults s a = Defaults { unDefaults :: Writer (Impls s) a }
    deriving (Functor, Applicative, Monad)


addImplSpecs :: String -> [ImplSpec s] -> Defaults s ()
addImplSpecs f = Defaults . tell . Impls . M.singleton f

addImplSpec :: String -> ImplSpec s -> Defaults s ()
addImplSpec f = addImplSpecs f . (:[])

toProblem :: (Ord s, Monoid s) => Defaults s () -> Problem s
toProblem
    = fmap (sortBy (flip (comparing scoreImplSpec)))
    . unImpls 
    . snd 
    . runWriter 
    . unDefaults

-- |Map a function over all scores.  This function's name comes from the
-- following idiom (where 'Sum' is replaced by whatever monoid-constructor
-- you want to use to combine scores):
-- 
-- > foo = scoreBy Sum $ do
-- >    ...
scoreBy :: (a -> b) -> Defaults a t -> Defaults b t
scoreBy f = Defaults . mapWriterT (fmap (fmap (fmap f))) . unDefaults

-- |A representation of a function for which one or more default
-- 'Implementation's exist.  Defined using the 'function' function.
newtype Function s a = Function (ReaderT String (Defaults s) a)
    deriving (Functor, Applicative, Monad)

-- |Declare a function that must be implemented, and provide a description
-- of any default implementations which can be used.
function :: String -> Function s a -> Defaults s a
function f (Function x) = do
    requireFunction f
    runReaderT x f

-- |State that a function must be implemented but has no default implementation.
requireFunction :: String -> Defaults s ()
requireFunction f = addImplSpecs f []

#if !MIN_VERSION_template_haskell(2,8,0)
data Inline = NoInline | Inline | Inlinable
    deriving (Eq, Show)
#endif

-- |A representation of a single possible implementation of a 'Function'.  Defined
-- using the 'implementation' function.
newtype Implementation s a = Implementation (State (Maybe s, S.Set String, Maybe Inline) a)
    deriving (Functor, Applicative, Monad)

-- |Describe a default implementation of the current function
implementation :: Implementation s (Q [Dec]) -> Function s ()
implementation (Implementation x) = case runState x (Nothing, S.empty, Nothing) of
    (dec, (s, deps, inl)) -> Function $ do
        fName <- ask
        ReaderT (const (addImplSpec fName (ImplSpec s deps (applyInline fName inl dec))))

applyInline :: String -> Maybe Inline -> Q [Dec] -> Q [Dec]
#if MIN_VERSION_template_haskell(2,8,0)
applyInline n (Just inl) = fmap (PragmaD (InlineP (mkName n) inl FunLike AllPhases) :)
#elif MIN_VERSION_template_haskell(2,4,0)
applyInline n (Just inl)
    | inl /= Inlinable  = fmap (PragmaD (InlineP (mkName n) (InlineSpec (inl == Inline) False Nothing)) :)
#endif
applyInline _ _ = id


-- |Specify the score associated with the current implementation.  Only one 
-- invocation of either 'score' or 'cost' may be used per implementation.
score :: s -> Implementation s ()
score s = Implementation $ do
    (oldS, deps, inl) <- get
    case oldS of
        Nothing -> put (Just s, deps, inl)
        Just _  -> error "score: score was already set"

-- |Specify the cost (negated score) associated with the current implementation.
-- Only one invocation of either 'score' or 'cost' may be used per implementation.
cost :: Num s => s -> Implementation s ()
cost = score . negate

-- |Specify that the current implementation must not be used unless the given
-- function is already defined.  If this implementation can be used
-- mutually-recursively with _ALL_ potential implementations of some other
-- function, then a dependency need not be declared on that function.
dependsOn :: String -> Implementation s ()
dependsOn dep = Implementation $ do
    (s, deps, inl) <- get
    put (s, S.insert dep deps, inl)

setInline :: Inline -> Implementation s ()
setInline inl = Implementation $ do
    (s, deps, _) <- get
    put (s, deps, Just inl)

-- |Specify that an 'Implementation' should be annotated with an INLINE pragma.
-- Under GHC versions earlier than 6.12 this is a no-op, because those Template
-- Haskell implementations do not support pragmas.
inline :: Implementation s ()

-- |Specify that an 'Implementation' should be annotated with an INLINEABLE pragma.
-- Under GHC versions earlier than 7.6 this is a no-op, because those Template
-- Haskell implementations do not support this pragma.
inlinable :: Implementation s ()

-- |Specify that an 'Implementation' should be annotated with a NOINLINE pragma.
-- Under GHC versions earlier than 6.12 this is a no-op, because those Template
-- Haskell implementations do not support pragmas.
noinline :: Implementation s ()

inline = setInline Inline
inlinable = setInline Inlinable
noinline = setInline NoInline
