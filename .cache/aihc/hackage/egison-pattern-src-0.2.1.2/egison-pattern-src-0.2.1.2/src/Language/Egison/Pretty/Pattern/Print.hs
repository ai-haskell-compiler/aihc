-- |
--
-- Module:      Language.Egison.Pretty.Pattern.Print
-- Description: Printer monad
-- Stability:   experimental
--
-- This module defines a pretty printing monad 'Print'.

module Language.Egison.Pretty.Pattern.Print
  ( Print
  , askMode
  , askContext
  , operatorOf
  , withContext
  , runPrint
  )
where

import qualified Data.Map                      as Map
                                                ( Map
                                                , empty
                                                , lookup
                                                , insert
                                                )
import           Control.Monad.Except           ( MonadError(..) )
import           Control.Monad.Reader           ( ReaderT
                                                , MonadReader(..)
                                                , runReaderT
                                                )

import           Language.Egison.Pretty.Pattern.Error
                                                ( Error(UnknownInfixOperator) )
import           Language.Egison.Pretty.Pattern.Context
                                                ( Context(World) )
import           Language.Egison.Pretty.Pattern.PrintMode
                                                ( PrintMode(..)
                                                , PrintFixity(..)
                                                , Fixity(..)
                                                )
import           Language.Egison.Pretty.Pattern.Operator
                                                ( Operator(..) )


type OperatorTable n = Map.Map n Operator

data Env n v e
  = Env { mode :: PrintMode n v e
        , table :: OperatorTable n
        , context :: Context
        }

buildOperatorTable :: Ord n => [PrintFixity n] -> OperatorTable n
buildOperatorTable = foldr go Map.empty
 where
  go fixity@PrintFixity { fixity = Fixity { symbol } } =
    Map.insert symbol (toOperator fixity)
  toOperator PrintFixity { fixity = Fixity { precedence, associativity }, printed }
    = InfixOp { precedence, associativity, symbol = printed }

initialEnv :: Ord n => PrintMode n v e -> Env n v e
initialEnv mode@PrintMode { fixities } =
  Env { mode, table = buildOperatorTable fixities, context = World }

newtype Print n v e a = Print { unParse :: ReaderT (Env n v e) (Either (Error n)) a }
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadReader (Env n v e))
  deriving newtype (MonadError (Error n))

askMode :: Print n v e (PrintMode n v e)
askMode = do
  Env { mode } <- ask
  pure mode

askContext :: Print n v e Context
askContext = do
  Env { context } <- ask
  pure context

withContext :: Context -> Print n v e a -> Print n v e a
withContext = local . updateContext
  where updateContext context env = env { context }

runPrint
  :: (Ord n, MonadError (Error n) m) => Print n v e a -> PrintMode n v e -> m a
runPrint p mode = case runReaderT (unParse p) (initialEnv mode) of
  Left  err -> throwError err
  Right x   -> pure x

operatorOf :: Ord n => n -> Print n v e Operator
operatorOf n = do
  Env { table } <- ask
  case Map.lookup n table of
    Just op -> pure op
    Nothing -> throwError $ UnknownInfixOperator n
