{- ORACLE_TEST pass -}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE UndecidableSuperclasses #-}
module LawfulInstance where

class c t => Lawful c t
