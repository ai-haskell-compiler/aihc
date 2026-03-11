{-# OPTIONS -fno-warn-name-shadowing #-}

module Data.Type.OpenWitness
    ( OpenWitness
    , RealWorld
    , IOWitness
    , newIOWitness
    , OW
    , newOpenWitnessOW
    , runOW
    , owToIO
    , iowitness
    , hashOpenWitness
    ) where

import Control.Concurrent.MVar
import Control.Monad.Fix
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.Hashable
import Data.Int
import Data.Kind
import Data.List ((\\), union)
import Data.Traversable
import Data.Type.Witness
import Language.Haskell.TH hiding (Type)
import Language.Haskell.TH qualified
import Prelude
import System.IO.Unsafe (unsafePerformIO)
import System.Random
import Unsafe.Coerce

unsafeSameType :: forall ka kb (a :: ka) (b :: kb). a :~~: b
unsafeSameType = unsafeCoerce HRefl

unsafeWEQ :: forall k (a :: k) (b :: k). WOrdering a b
unsafeWEQ = unsafeCoerce WEQ

-- | A witness type that can witness to any type.
-- But values cannot be constructed; they can only be generated in 'IO' and certain other monads.
newtype OpenWitness :: Type -> forall (k :: Type). k -> Type where
    MkOpenWitness :: forall (k :: Type) (s :: Type) (a :: k). Int64 -> OpenWitness s a

-- type role OpenWitness nominal nominal -- doesn't compile
type role OpenWitness nominal _

instance Eq (OpenWitness s a) where
    (MkOpenWitness p) == (MkOpenWitness q) = p == q

instance TestHetEquality (OpenWitness s) where
    testHetEquality (MkOpenWitness ua) (MkOpenWitness ub) =
        if ua == ub
            then Just unsafeSameType
            else Nothing

instance TestEquality (OpenWitness s) where
    testEquality wa wb = fmap hetHomoEq $ testHetEquality wa wb

instance TestOrder (OpenWitness s) where
    testCompare (MkOpenWitness ua) (MkOpenWitness ub) =
        case compare ua ub of
            LT -> WLT
            EQ -> unsafeWEQ
            GT -> WGT

-- | The @s@ type for running 'OW' in 'IO'.
data RealWorld

-- | An 'OpenWitness' for 'IO'.
type IOWitness :: forall k. k -> Type
type IOWitness = OpenWitness RealWorld

ioWitnessSource :: MVar Int64
{-# NOINLINE ioWitnessSource #-}
ioWitnessSource = unsafePerformIO (newMVar 0)

-- | Generate a new 'IOWitness' in 'IO'.
newIOWitness :: forall a. IO (IOWitness a)
newIOWitness = do
    val <- takeMVar ioWitnessSource
    putMVar ioWitnessSource (succ val)
    return (MkOpenWitness val)

type OWState = Int64

-- | A runnable monad in which 'OpenWitness' values can be generated.
-- The @s@ parameter plays the same role as it does in 'ST', preventing 'OpenWitness' values from one run being used in another.
type OW :: forall k. k -> Type -> Type
newtype OW s a =
    MkOW (State OWState a)
    deriving newtype (Functor, Applicative, Monad, MonadFix)

-- | Run an 'OW' computation.
runOW :: forall a. (forall s. OW s a) -> a
runOW uw = (\(MkOW st) -> evalState st 0) uw

-- | Generate a new 'OpenWitness' in 'OW'.
newOpenWitnessOW :: forall s a. OW s (OpenWitness s a)
newOpenWitnessOW = MkOW (StateT (\val -> Identity (MkOpenWitness val, succ val)))

-- | Run an 'OW' computation in 'IO'.
owToIO :: OW RealWorld a -> IO a
owToIO (MkOW st) =
    modifyMVar
        ioWitnessSource
        (\start -> let
             (a, count) = runState st start
             in return (count, a))

-- | An unsafe hack to generate 'IOWitness' values.
-- This is safe if you use a different integer each time, and if @a@ is a single type.
unsafeIOWitnessFromInt64 :: Int64 -> IOWitness a
unsafeIOWitnessFromInt64 = MkOpenWitness

-- | An unsafe hack to generate 'IOWitness' values.
-- This is safe if you use a different string each time (and 'hashString' doesn't collide), and if @a@ is a single type.
unsafeIOWitnessFromString :: String -> IOWitness a
unsafeIOWitnessFromString = unsafeIOWitnessFromInt64 . fromIntegral . hash

-- | Template Haskell function to declare 'IOWitness' values.
iowitness :: TypeQ -> Q Exp
iowitness qt = do
    t <- qt
    _ <- forM (freevarsType t) (\v -> reportError ("Type variable " ++ (show v) ++ " free in iowitness type"))
    l <- location
    rnd :: Int64 <- runIO randomIO
    key <- return ((showLoc l) ++ "/" ++ (show rnd))
    keyExpr <- return (LitE (StringL key))
    untypedWitExpr <- [|unsafeIOWitnessFromString $(return keyExpr)|]
    [|$(return untypedWitExpr) :: IOWitness $(return t)|]
  where
    showLoc :: Loc -> String
    showLoc l =
        (loc_filename l) ++
        "=" ++ (loc_package l) ++ ":" ++ (loc_module l) ++ (show (loc_start l)) ++ (show (loc_end l))
    unionList :: (Eq a) => [[a]] -> [a]
    unionList [] = []
    unionList (l:ls) = union l (unionList ls)
    bindingvarTVB :: TyVarBndr Specificity -> Name
    bindingvarTVB (PlainTV n _) = n
    bindingvarTVB (KindedTV n _ _) = n
    freevarsType :: Language.Haskell.TH.Type -> [Name]
    freevarsType (ForallT tvbs ps t) =
        (union (freevarsType t) (unionList (fmap freevarsType ps))) \\ (fmap bindingvarTVB tvbs)
    freevarsType (VarT name) = [name]
    freevarsType (AppT t1 t2) = union (freevarsType t1) (freevarsType t2)
    freevarsType (SigT t _) = freevarsType t
    freevarsType _ = []

hashOpenWitness :: Hashable t => OpenWitness s a -> t -> OpenWitness s a
hashOpenWitness (MkOpenWitness i) t = MkOpenWitness $ fromIntegral $ hash (i, t)
