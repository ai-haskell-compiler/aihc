module System.Path.RegularExpression where

import qualified Control.Monad.Trans.State as MS
import Control.Monad (guard)
import Control.Applicative (liftA2, (<|>))

import qualified Data.List.HT as ListHT
import Data.Monoid (Monoid, mempty, mappend)
import Data.Semigroup (Semigroup, (<>))
import Data.Maybe (fromMaybe)


newtype Parser a = Parser (MS.StateT [a] Maybe [a])

instance Semigroup (Parser a) where
    Parser x <> Parser y = Parser $ liftA2 (++) x y

instance Monoid (Parser a) where
    mempty = Parser $ return []
    mappend = (<>)

infixr 5 -|-

(-|-) :: Parser a -> Parser a -> Parser a
Parser x -|- Parser y = Parser $ x <|> y

single :: (a -> Bool) -> Parser a
single p = Parser $ do
    c <- MS.StateT ListHT.viewL
    guard $ p c
    return [c]

run :: Parser a -> MS.State [a] [a]
run (Parser x) =
    MS.state $ \str -> fromMaybe ([], str) $ MS.runStateT x str
