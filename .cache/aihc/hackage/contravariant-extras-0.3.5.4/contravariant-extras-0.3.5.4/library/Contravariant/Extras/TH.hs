module Contravariant.Extras.TH
  ( opContrazipDecs,
    contrazipDecs,
    contrazipExp,
  )
where

import Contravariant.Extras.Prelude
import Language.Haskell.TH.Syntax
import qualified TemplateHaskell.Compat.V0208 as Compat

-- |
-- Generates declarations in the spirit of the following:
--
-- @
-- tuple3 :: Monoid a => Op a b1 -> Op a b2 -> Op a b3 -> Op a ( b1 , b2 , b3 )
-- tuple3 ( Op op1 ) ( Op op2 ) ( Op op3 ) =
--   Op $ \( v1 , v2 , v3 ) -> mconcat [ op1 v1 , op2 v2 , op3 v3 ]
-- @
opContrazipDecs :: String -> Int -> [Dec]
opContrazipDecs baseName arity =
  [signature, value]
  where
    name =
      mkName (showString baseName (show arity))
    signature =
      SigD name type_
      where
        type_ =
          ForallT vars cxt type_
          where
            vars =
              map (Compat.specifiedPlainTV . mkName) ("a" : bs)
              where
                bs =
                  map b (enumFromTo 1 arity)
                  where
                    b index =
                      showString "b" (show index)
            cxt =
              [pred]
              where
                pred =
                  Compat.classP ''Monoid [a]
                  where
                    a =
                      VarT (mkName "a")
            type_ =
              foldr appArrowT result params
              where
                appArrowT a b =
                  AppT (AppT ArrowT a) b
                a =
                  VarT (mkName "a")
                result =
                  AppT (AppT (ConT ''Op) a) tuple
                  where
                    tuple =
                      foldl AppT (TupleT arity) params
                      where
                        params =
                          map param (enumFromTo 1 arity)
                          where
                            param index =
                              VarT (mkName (showString "b" (show index)))
                params =
                  map param (enumFromTo 1 arity)
                  where
                    param index =
                      AppT (AppT (ConT ''Op) a) b
                      where
                        b =
                          VarT (mkName (showString "b" (show index)))
    value =
      FunD name clauses
      where
        clauses =
          [clause]
          where
            clause =
              Clause pats body []
              where
                pats =
                  map pat (enumFromTo 1 arity)
                  where
                    pat index =
                      Compat.conP 'Op pats
                      where
                        pats =
                          [VarP name]
                          where
                            name =
                              mkName (showString "op" (show index))
                body =
                  NormalB (AppE (ConE 'Op) lambda)
                  where
                    lambda =
                      LamE pats exp
                      where
                        pats =
                          [TupP pats]
                          where
                            pats =
                              map pat (enumFromTo 1 arity)
                              where
                                pat index =
                                  VarP (mkName (showString "v" (show index)))
                        exp =
                          AppE (VarE 'mconcat) (ListE applications)
                          where
                            applications =
                              map application (enumFromTo 1 arity)
                              where
                                application index =
                                  AppE (VarE opName) (VarE varName)
                                  where
                                    opName =
                                      mkName (showString "op" (show index))
                                    varName =
                                      mkName (showString "v" (show index))

-- |
-- Generates declarations in the spirit of the following:
--
-- @
-- contrazip4 :: Divisible f => f a1 -> f a2 -> f a3 -> f a4 -> f ( a1 , a2 , a3 , a4 )
-- contrazip4 f1 f2 f3 f4 =
--   divide $(TupleTH.splitTupleAt 4 1) f1 $
--   divide $(TupleTH.splitTupleAt 3 1) f2 $
--   divide $(TupleTH.splitTupleAt 2 1) f3 $
--   f4
-- @
contrazipDecs :: String -> Int -> [Dec]
contrazipDecs baseName arity = [signature, value]
  where
    name = mkName (showString baseName (show arity))
    signature = SigD name (contrazipType arity)
    value = FunD name clauses
      where
        clauses = [clause]
          where
            clause = Clause [] body []
              where
                body = NormalB (contrazipExp arity)

contrazipType :: Int -> Type
contrazipType arity = ForallT vars cxt type_
  where
    fName = mkName "f"
    aNames = map aName (enumFromTo 1 arity)
      where
        aName index = mkName (showString "a" (show index))
    vars = map Compat.specifiedPlainTV (fName : aNames)
    cxt = [pred]
      where
        pred = Compat.classP ''Divisible [VarT fName]
    type_ = foldr appArrowT result params
      where
        appArrowT a b = AppT (AppT ArrowT a) b
        result = AppT (VarT fName) tuple
          where
            tuple = foldl AppT (TupleT arity) (map VarT aNames)
        params = map param aNames
          where
            param aName = AppT (VarT fName) (VarT aName)

-- |
-- Contrazip lambda expression of specified arity.
--
-- Allows to create contrazip expressions of any arity:
--
-- >>>:t $(return (contrazipExp 2))
-- \$(return (contrazipExp 2))
--   :: Data.Functor.Contravariant.Divisible.Divisible f =>
--      f a1 -> f a2 -> f (a1, a2)
contrazipExp :: Int -> Exp
contrazipExp arity = SigE (LamE pats body) (contrazipType arity)
  where
    pats = map pat (enumFromTo 1 arity)
      where
        pat index = VarP name
          where
            name = mkName (showString "f" (show index))
    body = exp arity
      where
        exp index = case index of
          1 -> VarE (mkName (showString "f" (show arity)))
          _ ->
            foldl1
              AppE
              [ VarE 'divide,
                splitTupleAtExp index 1,
                VarE (mkName (showString "f" (show (arity - index + 1)))),
                exp (pred index)
              ]

splitTupleAtExp :: Int -> Int -> Exp
splitTupleAtExp arity position =
  let nameByIndex index = Name (OccName ('_' : show index)) NameS
      names = enumFromTo 0 (pred arity) & map nameByIndex
      pats = names & map VarP
      pat = TupP pats
      exps = names & map VarE
      body = splitAt position exps & \(a, b) -> Compat.tupE [Compat.tupE a, Compat.tupE b]
   in LamE [pat] body
