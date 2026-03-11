{-# OPTIONS -fglasgow-exts #-}

{- | This module is highly experimental! -}

module Test.LazySmallCheck.Generic
  ( depthCheck  -- :: (Data a, Show a) => Int -> (a -> Bool) -> IO [a]
  , (==>)       -- :: Bool -> Bool -> Bool
  ) where

import Data.Maybe
import Data.Generics
import Control.Exception
import Control.Monad
import System.Exit

uniquePrefix = "UP:"

lenUniquePrefix = length uniquePrefix

type Position = String

initPData :: a
initPData = error uniquePrefix

data HLP a = HLP Int (Either a [a])

refinePData :: Data a => String -> Int -> Position -> a -> [a]
refinePData s d = r
 where
  depleft = d - (length s - lenUniquePrefix)
  r :: Data a => Position -> a -> [a]
  r [] x =
    let dt = dataTypeOf x
    in case dataTypeRep dt of
         AlgRep cons ->
           let cons = dataTypeConstrs dt
               z x = (0, x)
               k (i, g) = (i + 1, g (error $ s ++ [toEnum i]))
               xs' = map (gunfold k z) cons
           in  if   depleft > 0
               then map snd xs'
               else mapMaybe (\(ncon, x') ->
                                 if   ncon == 0
                                 then Just x'
                                 else Nothing) xs'
         IntRep -> mkPrim dt (mkIntegralConstr dt . toInteger)
                             [-depleft .. depleft]
         CharRep -> mkPrim dt (mkCharConstr dt)
                                (take (depleft+1) ['a' .. 'z'])
         _ -> error $ "LazySmallCheck.Generic: Can't generate type "
                   ++ dataTypeName dt
  r (c:ps) x =
   let p = fromEnum c
       z y = HLP 0 (Left y)
       k (HLP i (Left xs)) y | i == p = HLP (i + 1) (Right $ map xs (r ps y))
       k (HLP i (Left xs)) y = HLP (i + 1) (Left $ xs y)
       k (HLP i (Right xss)) y = HLP (i + 1) (Right $ map (\xs -> xs y) xss)
       HLP _ (Right x') = gfoldl k z x
   in  x'

mkPrim dt mk vs = map (\i -> fromJust $ gunfold undefined Just $ mk i) vs

--

mapVars :: Data a => (forall b . Data b => b -> IO b) -> a -> IO a
mapVars f = gmapM (\x -> Control.Exception.catch
  (mapVars f x)
  (\exc -> case exc of
    ErrorCall s | take (length uniquePrefix) s == uniquePrefix ->
     f x
    _ -> throw exc
  )
 )

-- Taken from Ralf Laemmel, SYB website
-- Generate all terms of a given depth
enumerate :: Data a => Int -> [a]
enumerate 0 = []
enumerate d = result
   where
     -- Getting hold of the result (type)
     result = concat (map recurse cons')

     -- Find all terms headed by a specific Constr
     recurse :: Data a => Constr -> [a]
     recurse con = gmapM (\_ -> enumerate (d-1)) 
                         (fromConstr con)

     -- We could also deal with primitive types easily.
     -- Then we had to use cons' instead of cons.
     --
     cons' :: [Constr]
     cons' = case dataTypeRep ty of
              AlgRep cons -> cons
              IntRep      -> map (mkIntegralConstr ty . toInteger) [-d .. d]
              CharRep     -> map (mkCharConstr ty) (take d ['a'..'z'])
              --FloatRep  ->
      where
        ty = dataTypeOf (head result)     

smallValue :: Data a => a
smallValue = f 1
 where
  f d = case enumerate d of
   [] -> f (d + 1)
   (x:_) -> x

smallInstance :: Data a => a -> IO a
smallInstance = mapVars (\_ -> return smallValue)

--

refute :: (Show a, Data a) => Int -> (a -> Bool) -> IO Int
refute d p = r initPData
  where
    r x = do res <- try (evaluate (p x))
             case res of
               Right True -> return 1
               Right False -> stop x "Counter example found:"
               Left (ErrorCall s)
                 | take (lenUniquePrefix) s == uniquePrefix ->
                     let pos = drop lenUniquePrefix s
                     in  do ns <- mapM r (refinePData s d pos x)
                            return (1 + sum ns)
               Left e -> stop x "Property crashed on input:"

    stop x s = do putStrLn s
                  x' <- smallInstance x
                  putStrLn (show x')
                  exitWith ExitSuccess
                     
--

depthCheck :: (Show a, Data a) => Int -> (a -> Bool) -> IO ()
depthCheck d f = do count <- refute d f
                    putStrLn $ "Completed " ++ show count
                            ++  " tests without finding a counter example."

--

infixr 0 ==>

(==>) :: Bool -> Bool -> Bool
False ==> a = True
True ==> a = a
