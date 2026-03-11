{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

-- | Template Haskell has a class 'Lift' that allows you to promote values
-- from Haskell-land into the land of metaprogramming - 'Q'.
--
-- @
-- class 'Lift' a where
--     'lift' :: a -> 'Q' 'Exp'
--
--     'liftTyped' :: a -> 'Q' ('TExp' a)
-- @
--
-- However, there wasn't a way to promote a *type* into a @'Q' 'Type'@.
--
-- This library provides exactly that function. It requires a 'Typeable'
-- constraint, but this is automatically satisfied by GHC.
--
-- @since 0.1.0.0
module LiftType where

import Data.Foldable (asum)
import qualified Data.Kind as Kind
import Data.Maybe (fromMaybe)
import Language.Haskell.TH.Syntax
import Text.Read (readMaybe)
import Type.Reflection

-- | Convert a type argument into a Template Haskell 'Type'.
--
-- Use with @TypeApplications@.
--
-- Example:
--
-- @
-- >>> :set -XTypeApplications
-- >>> liftType \@Bool
-- ConT GHC.Types.Bool
-- >>> liftType \@[Char]
-- AppT (ConT GHC.Types.[]) (ConT GHC.Types.Char)
-- @
--
-- This works with data kinds, too.
--
-- @
-- >>> :set -XDataKinds
-- >>> liftType \@3
-- LitT (NumTyLit 3)
-- >>> liftType \@"hello"
-- LitT (StrTyLit "hello")
-- >>> liftType \@'[Int, Char]
-- AppT (AppT (PromotedT GHC.Types.:) (ConT GHC.Types.Int)) (AppT (AppT (PromotedT GHC.Types.:) (ConT GHC.Types.Char)) (PromotedT GHC.Types.[]))
-- >>> liftType \@'(Int, Char)
-- AppT (AppT (PromotedT GHC.Tuple.(,)) (ConT GHC.Types.Int)) (ConT GHC.Types.Char)
-- @
--
-- @since 0.1.0.0
liftType :: forall t. Typeable t => Type
liftType =
    typeRepToType (SomeTypeRep (typeRep @t))

-- | 'liftType' promoted to the 'Q' monad.
--
-- @since 0.1.0.0
liftTypeQ :: forall t. Typeable t => Q Type
liftTypeQ = pure $ liftType @t

-- | Promote a 'SomeTypeRep' into a 'Type'.
--
-- @since 0.1.1.0
typeRepToType :: SomeTypeRep -> Type
typeRepToType (SomeTypeRep a) = go a
  where
    go :: forall k (a :: k). TypeRep a -> Type
    go tr
        | Just HRefl <- eqTypeRep (typeRep @Kind.Type) tr
        = ConT ''Kind.Type
        | otherwise =
        case tr of
            Con tyCon ->
                mk tyCon
            Fun trA trB ->
                ConT ''(->) `AppT` go trA `AppT` go trB
            App trA trB ->
                AppT (go trA) (go trB)

    mk :: TyCon -> Type
    mk tyCon =
        let
            tcName =
                tyConName tyCon
            typeOrDataName =
                tyConToName tyCon
            trySymbol =
                case tcName of
                    '"' : cs ->
                        Just $ LitT (StrTyLit (zipWith const cs (drop 1 cs)))
                    _ ->
                        Nothing
            tryTicked =
                case typeOrDataName of
                    PromotedDataName name ->
                        Just (PromotedT name)
                    _ ->
                        Nothing
            tryNat =
                LitT . NumTyLit <$> readMaybe tcName
            plainType =
                ConT (getTypeOrDataName typeOrDataName)
        in fromMaybe plainType $ asum [tryTicked, trySymbol, tryNat]

-- | Extract the 'TypeOrDataName' from a 'TyCon'. You probably want to use
-- 'typeToName' instead. See that function for documentation and more
-- information.
--
-- @since 0.1.2.0
tyConToName :: TyCon -> TypeOrDataName
tyConToName tyCon =
    let
        tcName =
            tyConName tyCon
        tryTicked =
            case tcName of
                '\'' : dcName ->
                    let nameBase =
                            mkOccName dcName

                        flavor =
                            NameG
                                DataName
                                (mkPkgName $ tyConPackage tyCon)
                                (mkModName $ tyConModule tyCon)
                        name =
                            Name
                                nameBase
                                flavor
                    in
                        Just (PromotedDataName name)
                _ ->
                    Nothing
        plainType =
            let
                nameBase =
                    mkOccName tcName
                flavor =
                    NameG
                        TcClsName
                        (mkPkgName $ tyConPackage tyCon)
                        (mkModName $ tyConModule tyCon)
                name =
                    Name
                        nameBase
                        flavor
            in
                TypeName name
    in fromMaybe plainType tryTicked

-- | This function returns the name of the outermost type constructor.
--
-- >>> typeToName @Char
-- TypeName ''Char
-- >>> typeToName @Maybe
-- TypeName ''Maybe
-- >>> typeToName @(Maybe Char)
-- TypeName ''Maybe
-- >>> typeToName @(Int -> Char)
-- TypeName ''(->)
-- >>> typeToName @'False
-- PromotedDataName 'False
--
-- @since 0.1.2.0
typeToName :: forall t. Typeable t => TypeOrDataName
typeToName = tyConToName (typeRepTyCon (typeRep @t))

-- | It's possible to use a data constructor with a @DataKinds@ promotion.
-- This disambiguates where the name comes from.
--
-- @since 0.1.2.0
data TypeOrDataName
    = TypeName Name
    | PromotedDataName Name
    deriving (Show, Eq)

-- | Retrieve the 'Name' from a 'TypeOrDataName', forgetting how it was
-- parsed.
--
-- @since 0.1.2.0
getTypeOrDataName :: TypeOrDataName -> Name
getTypeOrDataName d =
    case d of
        TypeName n ->
            n
        PromotedDataName n ->
            n
