module Main where

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System.Simple as Sys
import UniqueLogic.ST.TF.Expression ((=:=))

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.Writer as MW
import Control.Monad.Trans.Identity (IdentityT, )
import Control.Monad.ST (ST, runST, )
import Control.Monad (join, liftM2, )
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup, (<>), )

import Data.List (sortBy, )
import Data.Ord.HT (comparing, )

import qualified Data.NonEmpty as NonEmpty
import qualified Test.QuickCheck as QC


shuffle :: NonEmpty.T [] Int -> [a] -> [a]
shuffle order =
   map snd . sortBy (comparing fst) .
   zip (NonEmpty.flatten $ NonEmpty.cycle order)

newtype Check s = Check {runCheck :: s Bool}

instance (Monad s) => Semigroup (Check s) where
   Check x <> Check y = Check $ liftM2 (&&) x y

instance (Monad s) => Monoid (Check s) where
   mempty = Check $ return True
   mappend = (<>)

{-
Take a system of six equations and seven variables
where one variable is randomly chosen and initialized with the correct value.
The other six variables must be determined by the solver.
Then we pose the six equations and
finally check whether all variables got the right value.
-}
example :: Int -> NonEmpty.T [] Int -> Bool
example var order =
   runST
      (join . fmap runCheck . Sys.solve $ MW.execWriterT $ do
         let variable ::
                Int -> Rational ->
                MW.WriterT (Check (ST s)) (Sys.T (ST s))
                   (Expr.T IdentityT (ST s) Rational)
             variable n x = do
                v <-
                   MT.lift $
                   if mod var 7 == n
                     then Sys.constant x
                     else Sys.localVariable
                MW.tell $ Check $ fmap (Just x ==) $ Sys.query v
                return $ Expr.fromVariable v

         c  <- variable 0 1
         x0 <- variable 1 2
         x1 <- variable 2 3
         y0 <- variable 3 4
         y1 <- variable 4 5
         z0 <- variable 5 6
         z1 <- variable 6 7

         MT.lift $ sequence_ $ shuffle order $
            (c+1 =:= x0) :
            (x1*2 =:= x0*3) :
            (2*c + y0/2 =:= 4) :
            (y0 =:= subtract 1 y1) :
            (c =:= z0/6) :
            (z0*z1 =:= 42) :
            [] )


tests :: [(String, IO ())]
tests = [("example", QC.quickCheck example)]

main :: IO ()
main = mapM_ (\(msg, test) -> putStr (msg ++ " ") >> test) tests
