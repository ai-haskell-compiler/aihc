module Main where

import Control.Monad.Exception.Asynchronous
   (Exceptional(Exceptional), force, pure, throwMonoid, broken, result, exception, )
import Control.Monad (mplus, )
import Data.Monoid (Monoid, mappend, mempty, )
import Prelude hiding (pure)


convert :: [Either String a] -> Exceptional String [a]
convert =
   emconcat .
   map (force . either throwMonoid (pure . (:[])))

emconcat :: Monoid a => [Exceptional e a] -> Exceptional e a
emconcat =
   force .
   foldr
      (\(Exceptional e a) ~(Exceptional es as) ->
          Exceptional (mplus e es) (mappend a as))
      (pure mempty)

econcat :: [Exceptional e a] -> Exceptional e [a]
econcat =
   force .
   foldr
      (\(Exceptional e a) ~(Exceptional es as) ->
          Exceptional (mplus e es) (a:as))
      (pure [])

convert0 :: [Either String a] -> Exceptional String [a]
convert0 =
   force .
   -- not quite mconcat, because we need lazy matching on the right operand
   foldr
      (\a b -> mappend a (force b))
      (pure []) .
   map (either throwMonoid (pure . (:[])))

convert1 :: [Either String a] -> Exceptional String [a]
convert1 =
   force .
   foldr
      (\ea -> force .
         either (mappend . throwMonoid) (\entry -> fmap (entry:)) ea)
      (pure [])

-- the String argument prevents caching and thus a space-leak
infinite :: String -> [Either String Integer]
infinite msg =
   map Right (iterate (1+) 0) ++ [Left msg]

-- the String argument prevents caching and thus a space-leak
infiniteExc :: String -> [Exceptional String Integer]
infiniteExc msg =
   map (Exceptional Nothing) (iterate (1+) 0) ++ [broken msg 0]

skip :: [a] -> [a]
skip = map head . iterate (drop 1000)

spaceLeak0 :: IO ()
spaceLeak0 =
   let r  = convert $ infinite "bla"
       e  = exception r
       xs = result r
   in  do mapM_ print $ skip xs
          print e

spaceLeak1 :: IO ()
spaceLeak1 =
   let Exceptional e xs = convert $ infinite "bla"
   in  do mapM_ print $ skip xs
          print e

spaceLeak2 :: IO ()
spaceLeak2 =
   let Exceptional e xs = econcat $ infiniteExc "bla"
   in  do mapM_ print $ skip xs
          print e

noSpaceLeak0 :: IO ()
noSpaceLeak0 =
   let r  = convert $ infinite "bla"
       _e = exception r
       xs = result r
   in  mapM_ print $ skip xs

noSpaceLeak1 :: IO ()
noSpaceLeak1 =
   let Exceptional _e xs = convert $ infinite "bla"
   in  mapM_ print $ skip xs

{-
ee-test +RTS -M32m -c30 -RTS
-}
main :: IO ()
main = spaceLeak2
