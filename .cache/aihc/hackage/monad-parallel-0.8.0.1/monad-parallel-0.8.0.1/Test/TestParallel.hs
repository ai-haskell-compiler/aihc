{- 
    Copyright 2010 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | The "Control.Monad.Parallel" tests.

module Main where

import Prelude hiding (mapM)
import Control.Monad (liftM)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Parallel (pseq)
import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

import Control.Monad.Parallel

parFib :: MonadParallel m => Int -> Int -> m Integer
parFib _ 0 = return 1
parFib _ 1 = return 1
parFib 1 n = applyM fib n
parFib k n = bindM2 (\a b-> return (a + b)) (parFib (mod k 2) (n - 1)) (parFib (mod (k + 1) 2) (n - 2))

forkFib :: MonadFork m => Int -> Int -> m Integer
forkFib _ 0 = return 1
forkFib _ 1 = return 1
forkFib 1 n = applyM fib n
forkFib k n = do mf1 <- forkExec (forkFib (mod k 2) (n - 1))
                 f2 <- forkFib (mod (k + 1) 2) (n - 2)
                 f1 <- mf1
                 return (f1 + f2)

parSeqFib :: MonadParallel m => Int -> Int -> m Integer
parSeqFib t n = liftM sum $ mapM (applyM fib) (split t n)
   where split 1 n = [n]
         split k n | k > 1 = let (max : rest) = split (k - 1) n
                             in rest ++ [max - 1, max - 2]

applyM f n = let result = f n in result `pseq` return result

fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

main = do args <- getArgs
          if length args /= 5
             then putStr help
             else do let [taskName, method, monad, size, threads] = args
                         task :: MonadFork m => Int -> Int -> m Integer
                         task = case (method, taskName) of ("par", "fib") -> parFib
                                                           ("fork", "fib") -> forkFib
                                                           ("[par]", "fib") -> parSeqFib
                                                           _ -> error (help ++ "Bad method or task.")
                         parOnlyTask :: MonadParallel m => Int -> Int -> m Integer
                         parOnlyTask = case (method, taskName) of ("par", "fib") -> parFib
                                                                  ("[par]", "fib") -> parSeqFib
                                                                  _ -> error ("Monad " ++ monad ++ " can't do "
                                                                              ++ method)
                     result <- case monad of "Identity" -> return $ runIdentity $ parOnlyTask (read threads) (read size)
                                             "IdentityT-[]" -> return $ head $ runIdentityT 
                                                               $ task (read threads) (read size)
                                             "IdentityT-IO" -> runIdentityT $ task (read threads) (read size)
                                             "Maybe" -> return $ fromJust $ task (read threads) (read size)
                                             "MaybeT-[]" -> return $ fromJust $ head $ runMaybeT
                                                            $ task (read threads) (read size)
                                             "MaybeT-IO" -> liftM fromJust $ runMaybeT 
                                                            $ task (read threads) (read size)
                                             "ErrorT-[]" -> return $ (either error id) $ head $ runExceptT
                                                            $ task (read threads) (read size)
                                             "ErrorT-IO" -> liftM (either error id) $ runExceptT 
                                                            $ task (read threads) (read size)
                                             "->" -> return $ ($ 0) $ task (read threads) (read size)
                                             "[]" -> return $ head $ task (read threads) (read size)
                                             "ListT-Maybe" -> return $ head $ fromJust $ runListT 
                                                              $ task (read threads) (read size)
                                             "ListT-IO" -> liftM head $ runListT 
                                                           $ task (read threads) (read size)
                                             "ReaderT-[]" -> return $ head 
                                                             $ runReaderT (task (read threads) (read size)) ()
                                             "ReaderT-IO" -> runReaderT (task (read threads) (read size)) ()
                                             "IO" -> task (read threads) (read size)
                                             _ -> error (help ++ "Bad monad.")
                     print result

help = "Usage: test-parallel <task> <method> <monad> <threads> <size>\n"
       ++ "  where <task>    is 'fib',\n"
       ++ "        <method>  is 'par', 'fork', or '[par]'\n"
       ++ "        <monad>   is 'Identity', 'Maybe', '->', '[]', "
       ++ "                     'IdentityT-IO', 'IdentityT-[]', 'MaybeT-IO', 'MaybeT-[]'," 
       ++ "                     'ErrorT-IO', 'ErrorT-[]', 'ListT-IO', 'ListT-Maybe',"
       ++ "                     'ReaderT-IO', 'ReaderT-[]', or 'IO',\n"
       ++ "        <threads> is the number of threads to launch,\n"
       ++ "    and <size>    is the size of the task.\n"
