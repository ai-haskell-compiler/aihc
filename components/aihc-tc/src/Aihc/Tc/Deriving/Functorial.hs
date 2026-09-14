{-# LANGUAGE OverloadedStrings #-}

-- | How the field of a constructor uses the last parameter of its datatype.
--
-- @Functor@, @Foldable@ and @Traversable@ are derived over the last
-- parameter of a datatype: the instance head drops it, and every field is
-- rewritten in terms of what it does with it. A field either ignores the
-- parameter, is the parameter, or is a type applied to something that uses
-- it -- and in the last case the derived body hands the work to the
-- instance of that type, so the context needs it.
--
-- The three classes differ only in the code they write for each of those
-- cases, so the analysis is here and each generator reads it.
module Aihc.Tc.Deriving.Functorial
  ( FieldUse (..),
    fieldUse,
    fieldUseObligations,
  )
where

import Aihc.Tc.Annotations (renderTcType)
import Aihc.Tc.Types
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- | What a field type does with the last parameter of the datatype.
data FieldUse
  = -- | The field does not mention the parameter. A derived body leaves
    -- such a field alone.
    FieldAbsent
  | -- | The field is the parameter itself. A derived body applies the
    -- function it was given to it.
    FieldParameter
  | -- | The field is a type applied to something that uses the parameter,
    -- such as @Maybe a@ or @f (g a)@. The first component is the type
    -- without that last argument, whose instance does the work; the second
    -- is what the argument itself does with the parameter.
    FieldContainer !TcType !FieldUse
  deriving (Eq, Show)

-- | How a field type uses the parameter, or why the class cannot be derived
-- over it. GHC and this analysis agree: a datatype whose last parameter
-- appears anywhere but as the last argument of a type has no derived
-- instance, because there is no instance to hand that position to.
fieldUse :: String -> TyVarId -> TcType -> Either String FieldUse
fieldUse mechanism parameter = go
  where
    go ty
      | not (mentions ty) = Right FieldAbsent
      | TcTyVar tyVar <- ty, tvUnique tyVar == tvUnique parameter = Right FieldParameter
      | Just (function, argument) <- splitLastArgument ty,
        not (mentions function) =
          FieldContainer function <$> go argument
      | otherwise = Left (positionError mechanism parameter ty)
    mentions = typeMentionsTyVar Map.empty parameter

-- | A type as a function and its last argument. A saturated arrow is the
-- arrow type constructor applied to its domain, so a field of function type
-- is a container like any other -- and a parameter in the domain is a
-- parameter the function part mentions, which is rejected, as it must be:
-- no instance can map over it.
splitLastArgument :: TcType -> Maybe (TcType, TcType)
splitLastArgument ty =
  case ty of
    TcTyCon _ [] -> Nothing
    TcTyCon tyCon arguments -> Just (TcTyCon tyCon (init arguments), last arguments)
    TcAppTy function argument -> Just (function, argument)
    TcFunTy domain result -> Just (TcAppTy TcArrowTy domain, result)
    _ -> Nothing

-- | The classes the fields of a derived instance need: one for every type
-- whose instance does the work at a nested position.
fieldUseObligations :: TyCon -> FieldUse -> [Pred]
fieldUseObligations classTyCon use =
  case use of
    FieldAbsent -> []
    FieldParameter -> []
    FieldContainer function inner -> ClassPred classTyCon [function] : fieldUseObligations classTyCon inner

positionError :: String -> TyVarId -> TcType -> String
positionError mechanism parameter ty =
  mechanism
    <> " requires "
    <> T.unpack (tvName parameter)
    <> " to appear only as the last argument of a type, but a constructor field has type "
    <> renderTcType ty
