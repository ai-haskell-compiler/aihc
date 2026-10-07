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
-- A boxed tuple is not handed to an instance. As in GHC, a derived body
-- takes the tuple apart and visits each component, so a field such as
-- @(Int, a)@ needs no instance for the tuple type.
--
-- @Functor@ can also go through a function: the result of a function is a
-- position like any other, and the domain is a position of the opposite
-- polarity. The parameter can occur in a domain of a domain, but not in a
-- domain alone, because nothing can map over it there.
--
-- The three classes differ only in the code they write for each of those
-- cases, so the analysis is here and each generator reads it.
module Aihc.Tc.Deriving.Functorial
  ( FieldUse (..),
    FunctionFields (..),
    fieldUse,
    fieldUseObligations,
  )
where

import Aihc.Tc.Annotations (renderTcType)
import Aihc.Tc.Types
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
  | -- | The field is a boxed tuple of two or more components. Each element
    -- is what one component does with the parameter.
    FieldTuple ![FieldUse]
  | -- | The field is a function. The first component is what the domain
    -- does with the parameter, at the opposite polarity. The second is
    -- what the result does with it.
    FieldFunction !FieldUse !FieldUse
  | -- | The field is a polymorphic type. The derived body goes through the
    -- quantifier, and the bound variables are not instance parameters: an
    -- instance at a type that mentions them comes from the givens of the
    -- field type, and not from the instance context.
    FieldForAll ![TyVarId] !FieldUse
  deriving (Eq, Show)

-- | Whether a class can be derived through a function field. @Functor@
-- can, because a function maps its result. @Foldable@ and @Traversable@
-- cannot, because a function has no elements to visit.
data FunctionFields
  = FunctionFieldsMapped
  | FunctionFieldsRejected
  deriving (Eq, Show)

-- | How a field type uses the parameter, or why the class cannot be derived
-- over it. GHC and this analysis agree: a datatype whose last parameter
-- appears anywhere but as the last argument of a type, or the result of a
-- function when the class permits functions, has no derived instance,
-- because there is no instance to hand that position to.
fieldUse :: TcKinds -> String -> FunctionFields -> TyVarId -> TcType -> Either String FieldUse
fieldUse kinds mechanism functions parameter field = go True field
  where
    -- The flag is the polarity of the position: 'True' when the position
    -- is covariant, 'False' when it is in an odd number of domains.
    go covariant ty
      | not (mentions ty) = Right FieldAbsent
      | TcTyVar tyVar <- ty,
        tvUnique tyVar == tvUnique parameter =
          if covariant then Right FieldParameter else reject
      | TcTyCon tyCon arguments <- ty,
        length arguments >= 2,
        tyCon == kindsBoxedTupleTyCon kinds (length arguments) =
          FieldTuple <$> mapM (go covariant) arguments
      | functions == FunctionFieldsMapped,
        TcFunTy domain result <- ty =
          FieldFunction <$> go (not covariant) domain <*> go covariant result
      | functions == FunctionFieldsMapped,
        TcForAllTy {} <- ty =
          let (bound, body) = splitForAll ty
           in FieldForAll bound <$> go covariant body
      | functions == FunctionFieldsMapped,
        TcQualTy predicates body <- ty,
        not (any (predicateMentionsTyVar parameter) predicates) =
          go covariant body
      | Just (function, argument) <- splitLastArgument ty,
        not (mentions function) =
          FieldContainer function <$> go covariant argument
      | otherwise = reject
    mentions = typeMentionsTyVar parameter
    reject = Left (positionError mechanism functions parameter field)

-- | The variables a polymorphic type binds, outermost first, and its body.
splitForAll :: TcType -> ([TyVarId], TcType)
splitForAll ty =
  case ty of
    TcForAllTy tyVar body ->
      let (bound, rest) = splitForAll body
       in (tyVar : bound, rest)
    _ -> ([], ty)

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
-- whose instance does the work at a nested position. A type that mentions
-- a variable of a polymorphic field needs no instance from the context.
fieldUseObligations :: TyCon -> FieldUse -> [Pred]
fieldUseObligations classTyCon = go []
  where
    go bound use =
      case use of
        FieldAbsent -> []
        FieldParameter -> []
        FieldContainer function inner
          | any (`typeMentionsTyVar` function) bound -> go bound inner
          | otherwise -> ClassPred classTyCon [function] : go bound inner
        FieldTuple components -> concatMap (go bound) components
        FieldFunction domain result -> go bound domain <> go bound result
        FieldForAll tyVars inner -> go (tyVars <> bound) inner

positionError :: String -> FunctionFields -> TyVarId -> TcType -> String
positionError mechanism functions parameter ty =
  mechanism
    <> " requires "
    <> T.unpack (tvName parameter)
    <> " to appear only "
    <> positions
    <> ", but a constructor field has type "
    <> renderTcType ty
  where
    positions =
      case functions of
        FunctionFieldsMapped -> "as the last argument of a type or in a covariant position of a function"
        FunctionFieldsRejected -> "as the last argument of a type"
