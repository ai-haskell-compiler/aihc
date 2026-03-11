{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.MockCat.ParamDivider (ParamDivider, args, return, returnValue) where

import Test.MockCat.Cons
import Test.MockCat.Param
import Prelude hiding (return)

class ParamDivider params args return | params -> args, params -> return where
  args :: params -> args
  return :: params -> return

instance ParamDivider 
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i :> Param r)
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param i) (Param r) where
  args (a :> b :> c :> d :> e :> f :> g :> h :> i :> _) = a :> b :> c :> d :> e :> f :> g :> h :> i
  return (_ :> _ :> _ :> _ :> _ :> _ :> _ :> _ :> _ :> r) = r

instance ParamDivider 
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h :> Param r)
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param h) (Param r) where
  args (a :> b :> c :> d :> e :> f :> g :> h :> _) = a :> b :> c :> d :> e :> f :> g :> h
  return (_ :> _ :> _ :> _ :> _ :> _ :> _ :> _ :> r) = r

instance ParamDivider 
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g :> Param r)
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param g) (Param r) where
  args (a :> b :> c :> d :> e :> f :> g :> _) = a :> b :> c :> d :> e :> f :> g
  return (_ :> _ :> _ :> _ :> _ :> _ :> _ :> r) = r

instance ParamDivider 
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f :> Param r)
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param f) (Param r) where
  args (a :> b :> c :> d :> e :> f :> _) = a :> b :> c :> d :> e :> f
  return (_ :> _ :> _ :> _ :> _ :> _ :> r) = r

instance ParamDivider 
  (Param a :> Param b :> Param c :> Param d :> Param e :> Param r)
  (Param a :> Param b :> Param c :> Param d :> Param e) (Param r) where
  args (a :> b :> c :> d :> e :> _) = a :> b :> c :> d :> e
  return (_ :> _ :> _ :> _ :> _ :> r) = r

instance ParamDivider (Param a :> Param b :> Param c :> Param d :> Param r) (Param a :> Param b :> Param c :> Param d) (Param r) where
  args (a :> b :> c :> d :> _) = a :> b :> c :> d
  return (_ :> _ :> _ :> _ :> r) = r

instance ParamDivider (Param a :> Param b :> Param c :> Param r) (Param a :> Param b :> Param c) (Param r) where
  args (a :> b :> c :> _) = a :> b :> c
  return (_ :> _ :> _ :> r) = r

instance ParamDivider (Param a :> Param b :> Param r) (Param a :> Param b) (Param r) where
  args (a :> b :> _) = a :> b
  return (_ :> _ :> r) = r

instance ParamDivider (Param a :> Param r) (Param a) (Param r) where
  args (a :> _) = a
  return (_ :> r) = r

returnValue :: ParamDivider params args (Param r) => params -> r
returnValue = value . return
