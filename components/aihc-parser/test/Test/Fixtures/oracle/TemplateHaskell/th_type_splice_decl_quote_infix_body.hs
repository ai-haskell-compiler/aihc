{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module TH_TypeSpliceDeclQuoteInfixBody where

x = [d| instance Show $(return $ ConT name) where show _ = abbrev |]
