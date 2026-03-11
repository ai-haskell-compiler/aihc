{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures  #-}

module Control.Isomorphism.Partial.Constructors 
  ( nil
  , cons
  , listCases
  , left
  , right
  , nothing
  , just
  ) where

import Prelude ()

import Data.Either (Either (Left, Right))
import Data.Maybe (Maybe (Just, Nothing))

import Control.Isomorphism.Partial.Unsafe (Iso (Iso))
import Control.Isomorphism.Partial.TH (defineIsomorphisms)

nil :: Iso () [alpha]
nil = Iso f g where
  f ()  =  Just []
  g []  =  Just ()
  g _   =  Nothing

cons :: Iso (alpha, [alpha]) [alpha]
cons = Iso f g where
  f (x, xs)   =  Just (x : xs)
  g (x : xs)  =  Just (x, xs)
  g _         =  Nothing

listCases :: Iso (Either () (alpha, [alpha])) [alpha]
listCases = Iso f g
  where
    f (Left ())        =  Just []
    f (Right (x, xs))  =  Just (x : xs)
    g []               =  Just (Left ())
    g (x:xs)           =  Just (Right (x, xs))

$(defineIsomorphisms ''Either)
$(defineIsomorphisms ''Maybe)
