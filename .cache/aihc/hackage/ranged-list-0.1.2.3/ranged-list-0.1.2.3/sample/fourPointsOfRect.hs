{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances,
	UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=Plugin.TypeCheck.Nat.Simple #-}

import GHC.TypeNats
import Control.Monad.Fix
import Control.Monad.Catch
import Data.List.Length
import Text.Read

fourPoints :: LengthR 4 Double -> LengthR 4 (Double, Double)
fourPoints (NilR :+ l :+ t :+ w :+ h) =
	NilR :+ (l, t) :+ (l + w, t) :+ (l, t + h) :+ (l + w, t + h)

data DeleteOr a = Delete | Value a deriving Show
data NothingToDeleteException = NothingToDeleteException deriving Show
instance Exception NothingToDeleteException

class GetElems n v where
	getElems :: MonadThrow m =>
		LengthR n a -> m (Maybe (DeleteOr a)) -> m (LengthR (n + v) a)

instance GetElems 0 0 where getElems NilR _ = pure NilR

instance {-# OVERLAPPABLE #-} 1 <= n => GetElems n 0 where
	getElems xs@(_ :+ _) _ = pure xs

instance {-# OVERLAPPABLE #-} GetElems 1 (v - 1) => GetElems 0 v where
	getElems NilR gt = gt >>= \case
		Nothing -> getElems NilR gt
		Just Delete -> throwM NothingToDeleteException
		Just (Value x) -> getElems @1 @(v - 1) (NilR :+ x) gt

instance {-# OVERLAPPABLE #-}
	(1 <= n, GetElems (n - 1) (v + 1), GetElems (n + 1) (v - 1)) =>
	GetElems n v where
	getElems xa@(xs :+ _) gt = gt >>= \case
		Nothing -> getElems xa gt
		Just Delete -> getElems @(n - 1) @(v + 1) xs gt
		Just (Value x) -> getElems @(n + 1) @(v - 1) (xa :+ x) gt

titles :: (Show a, Applicative (LengthR n)) =>
	Int -> LengthR n String -> LengthR n a -> LengthR n String
titles n ts xs = (\t x -> t ++ replicate (n - length t) ' ' ++ ": " ++ show x)
	<$> ts <*> xs

printResult :: LengthR 4 Double -> IO ()
printResult r = do
	putStrLn ""
	putStrLn `mapM_` titles 6 t r; putStrLn ""
	putStrLn `mapM_` titles 12 u (fourPoints r); putStrLn ""
	where
	t = NilR :+ "left" :+ "top" :+ "width" :+ "height"
	u = NilR :+ "left-top" :+ "right-top" :+ "left-bottom" :+ "right-bottom"

getRect :: forall n . GetElems n (4 - n) =>
	LengthR n Double -> IO (LengthR 4 Double)
getRect xs = (<$) <$> id <*> printResult =<<
	getElems @n @(4 - n) xs ((<$> getLine) \case
		"d" -> Just Delete; l -> Value <$> readMaybe l)
	`catch`
	\(_ :: NothingToDeleteException) ->
		putStrLn "*** Nothing to delete." >> getRect @0 NilR

main :: IO ()
main = getRect NilR >>= fix \go xa@(xs :+ _) -> getLine >>= \case
	"q" -> pure ();
	"d" -> go =<< getRect xs
	_ -> putStrLn "q or d" >> go xa
