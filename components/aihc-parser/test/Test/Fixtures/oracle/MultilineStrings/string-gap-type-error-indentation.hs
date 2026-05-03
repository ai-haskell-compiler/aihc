{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module StringGapTypeErrorIndentation where

import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (Text), TypeError)

type family UrlAlphabet k :: Constraint where
  UrlAlphabet 'True = ()
  UrlAlphabet _ = TypeError
    ( 'Text "Cannot prove base64 value is encoded using the url-safe \
            \alphabet. Please re-encode using the url-safe encoders, or use \
            \a lenient decoder for the url-safe alphabet instead."
    )
