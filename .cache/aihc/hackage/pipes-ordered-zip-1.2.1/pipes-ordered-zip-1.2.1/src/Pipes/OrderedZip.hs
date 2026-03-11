-- |A function to tie together two sorted Haskell Iterators
module Pipes.OrderedZip (orderedZip, orderedZipAll, orderCheckPipe, WrongInputOrderException(..)) where

import           Control.Exception      (Exception)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef             (newIORef, readIORef, writeIORef)
import           Data.Maybe             (catMaybes)
import           Pipes                  (Pipe, Producer, cat, for, lift, next,
                                         yield, (>->))
import qualified Pipes.Prelude          as P
import           Pipes.Safe             (MonadSafe, throwM)

-- |orderedZip takes a comparison function and two producers and merges them
-- together, creating a new Producer that yields pairs of Maybes of the two
-- datatables provided by the two original producers.
-- The output pairs reflect the Union of the two input producers, with Nothings indicating
-- missing data in one of the producers at that location.
orderedZip :: (Monad m) => (a -> b -> Ordering) -- ^The function to compare types of a with b
           -> Producer a m r1 -- ^The first producer (assumed to be ordered)
           -> Producer b m r2 -- ^The second producer (assumed to be ordered)
           -> Producer (Maybe a, Maybe b) m (r1, r2) -- ^The merged producer
orderedZip ord p1 p2 = do
    p1Front <- lift $ next p1
    p2Front <- lift $ next p2
    go ord p1Front p1 p2Front p2
    where
        go ord' p1Front p1' p2Front p2' = case (p1Front, p2Front) of
            (Left p1r, Left p2r) -> return (p1r, p2r)
            (Left _, Right (p2a, p2Rest)) -> do
                yield (Nothing, Just p2a)
                p2Front' <- lift $ next p2Rest
                go ord' p1Front p1' p2Front' p2Rest
            (Right (p1a, p1Rest), Left _) -> do
                yield (Just p1a, Nothing)
                p1Front' <- lift $ next p1Rest
                go ord' p1Front' p1Rest p2Front p2'
            (Right (p1a, p1Rest), Right (p2a, p2Rest)) -> case ord p1a p2a of
                LT -> do
                    yield (Just p1a, Nothing)
                    p1Front' <- lift $ next p1Rest
                    go ord' p1Front' p1Rest p2Front p2'
                EQ -> do
                    yield (Just p1a, Just p2a)
                    p1Front' <- lift $ next p1Rest
                    p2Front' <- lift $ next p2Rest
                    go ord' p1Front' p1Rest p2Front' p2Rest
                GT -> do
                    yield (Nothing, Just p2a)
                    p2Front' <- lift $ next p2Rest
                    go ord' p1Front p1' p2Front' p2Rest

-- |orderedZipAll takes a comparison function and a list of producers and merges them
-- together, creating a new Producer that yields lists of Maybes of the
-- input data type provided by the original producers.
-- The output list reflects the Union of all input producers, with Nothings indicating
-- missing data in any of the producers at that instance.
orderedZipAll :: (Monad m) => (a -> a -> Ordering) -- ^The function to compare types of a with itself
              -> [Producer a m r] -- ^A list of producers (have to be ordered)
              -> Producer [Maybe a] m [r] -- ^The merged producer
orderedZipAll compFunc prodList = go compFunc (length prodList) prodList
  where
    go :: (Monad m) => (a -> a -> Ordering) -> Int -> [Producer a m r] -> Producer [Maybe a] m [r]
    go compFunc _ [] = error "orderedZipAll error: empty list" -- just to make the function complete
    go compFunc _ [prod] = fmap return (prod >-> P.map (return . Just))
    go compFunc n (prod1:prods) =
        (fmap (\(r, rs) -> (r:rs)) (orderedZip (compFunc2 compFunc) prod1 (go compFunc (n - 1) prods))) >-> P.map mergeMaybeTuples
      where
        mergeMaybeTuples :: (Maybe a, Maybe [Maybe a]) -> [Maybe a]
        mergeMaybeTuples (Nothing, Nothing) = error "orderedZipAll - should never happen" -- just to make this function complete
        mergeMaybeTuples (Nothing, Just mla) = (Nothing: mla)
        mergeMaybeTuples (Just a, Nothing) = (Just a : replicate (n - 1) Nothing)
        mergeMaybeTuples (Just a, Just mla) = (Just a : mla)
        compFunc2 :: (a -> a -> Ordering) -> a -> [Maybe a] -> Ordering
        compFunc2 compFunc x xs =
            let allJusts = catMaybes xs
            in  case allJusts of
                    []      -> error "orderedZipAll compFunc2 - should never happen" -- just to make this complete
                    (xs1:_) -> compFunc x xs1

-- an exception type to represent invalid input order
data WrongInputOrderException = WrongInputOrderException String
    deriving (Show, Eq)
instance Exception WrongInputOrderException

-- a pipe to check wether the stream is ordered according to a custom ordering function
orderCheckPipe :: (MonadIO m, MonadSafe m, Show a) => (a -> a -> Ordering) -- ^the custom ordering function
               -> Pipe a a m r -- ^the resulting pipe
orderCheckPipe cmpFunc = do
    lastValRef <- liftIO $ newIORef (Nothing)
    for cat $ \entry -> do
        lastVal <- liftIO $ readIORef lastValRef
        case lastVal of
            Nothing -> return ()
            Just p -> case cmpFunc entry p of
                LT -> throwM $ WrongInputOrderException ("ordering violated: " ++ show p ++ " should come after " ++ show entry)
                _ -> return ()
        liftIO $ writeIORef lastValRef (Just entry)
        yield entry
