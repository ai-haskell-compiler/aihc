{- ORACLE_TEST xfail lawful instance with constraints -}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE UndecidableSuperclasses #-}
module LawfulInstance where

class c t => Lawful c t
