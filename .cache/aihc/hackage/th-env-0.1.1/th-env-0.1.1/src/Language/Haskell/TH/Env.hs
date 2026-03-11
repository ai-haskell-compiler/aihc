{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Haskell.TH.Env (envQ, envQ')

where

import Data.String
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.Compat
import System.Environment

-- | Produce a typed expression with the current value of an
-- environment variable, or Nothing if it's not set.
envQ :: IsString a
     => String
     -- ^ Environment variable name.
     -> SpliceQ (Maybe a)
envQ name = liftSplice $
  runIO (lookupEnv name) >>= \case
    Just v  -> fromCode $ toCode [|| Just (fromString v) ||]
    Nothing -> fromCode $ toCode [|| Nothing ||]

-- | Produce a typed expression with the current value of an
-- environment variable. Fail if it's not set.
envQ' :: IsString a
      => String
      -- ^ Environment variable name.
      -> SpliceQ a
envQ' name = liftSplice $
  runIO (lookupEnv name) >>= \case
    Just v  -> fromCode $ toCode [|| fromString v ||]
    Nothing -> fail $ "Environment variable " ++ name ++ " is not set"
