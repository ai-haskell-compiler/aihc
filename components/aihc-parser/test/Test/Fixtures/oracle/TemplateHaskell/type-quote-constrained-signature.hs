{- ORACLE_TEST pass -}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module TypeQuoteConstrainedSignature where

x = [t| C |] :: (:+) => ()
