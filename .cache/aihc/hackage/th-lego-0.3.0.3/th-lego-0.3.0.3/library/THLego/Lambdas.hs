module THLego.Lambdas where

import Language.Haskell.TH
import THLego.Helpers
import THLego.Prelude
import qualified TemplateHaskell.Compat.V0208 as Compat

-- |
-- Van Larrhoven lens.
vlLens ::
  -- | Constructor name.
  Name ->
  -- | Total amount of members.
  Int ->
  -- | Index of the member.
  Int ->
  -- |
  --  Lambda expression of the following type:
  --
  --  > forall f. Functor f => (a -> f b) -> s -> f t
  Exp
vlLens conName numMembers index =
  LamE [onMemberP, productP] exp
  where
    -- Reference implementation:
    -- \ memberMapper (Product a b) -> fmap (\ newMember -> Product newMember b) (memberMapper a)
    onMemberName =
      mkName "memberMapper"
    memberNames =
      fmap alphabeticIndexName (enumFromTo 0 (pred numMembers))
    onMemberP =
      VarP onMemberName
    productP =
      Compat.conP conName pats
      where
        pats =
          fmap VarP memberNames
    exp =
      multiAppE (VarE 'fmap) [setterE, onMemberE]
      where
        setterE =
          LamE [VarP valueName] exp
          where
            valueName =
              mkName "newMember"
            exp =
              multiAppE (ConE conName) (fmap VarE argNames)
              where
                argNames =
                  take index memberNames
                    <> [valueName]
                    <> drop (succ index) memberNames
        onMemberE =
          AppE (VarE onMemberName) (VarE (alphabeticIndexName index))

-- |
-- Simulates lambda-case without the need for extension.
matcher :: [Match] -> Exp
matcher matches =
  LamE [VarP aName] (CaseE (VarE aName) matches)

-- |
-- Lambda expression, which extracts a product member by index.
productGetter ::
  -- | Constructor name.
  Name ->
  -- | Total amount of members.
  Int ->
  -- | Index of the member.
  Int ->
  -- |
  --  Lambda expression of the following form:
  --
  --  > product -> member
  Exp
productGetter conName numMembers index =
  LamE [pat] exp
  where
    varName =
      alphabeticIndexName index
    pat =
      Compat.conP conName pats
      where
        pats =
          replicate index WildP
            <> pure (VarP varName)
            <> replicate (numMembers - index - 1) WildP
    exp =
      VarE varName

-- |
-- Lambda expression, which sets a product member by index.
productSetter ::
  -- | Constructor name.
  Name ->
  -- | Total amount of members.
  Int ->
  -- | Index of the member.
  Int ->
  -- |
  --  Lambda expression of the following form:
  --
  --  > product -> member -> product
  Exp
productSetter conName numMembers index =
  LamE [stateP, valP] exp
  where
    memberName =
      alphabeticIndexName index
    memberNames =
      fmap alphabeticIndexName (enumFromTo 0 (pred numMembers))
    stateP =
      Compat.conP conName pats
      where
        pats =
          (memberNames & take index & fmap VarP)
            <> [WildP]
            <> (memberNames & drop (succ index) & fmap VarP)
    valP =
      VarP memberName
    exp =
      foldl' AppE (ConE conName) (fmap VarE memberNames)

-- |
-- Lambda expression, which maps a product member by index.
productMapper ::
  -- | Constructor name.
  Name ->
  -- | Total amount of members.
  Int ->
  -- | Index of the member.
  Int ->
  -- |
  --  Lambda expression of the following form:
  --
  --  > (member -> member) -> product -> product
  Exp
productMapper conName numMembers index =
  LamE [mapperP, stateP] exp
  where
    memberNames =
      fmap alphabeticIndexName (enumFromTo 0 (pred numMembers))
    valName =
      alphabeticIndexName index
    fnName =
      mkName "fn"
    mapperP =
      VarP fnName
    stateP =
      Compat.conP conName pats
      where
        pats =
          fmap VarP memberNames
    exp =
      foldl' AppE (ConE conName)
        $ fmap (VarE . alphabeticIndexName) (enumFromTo 0 (pred index))
        <> pure (AppE (VarE fnName) (VarE valName))
        <> fmap (VarE . alphabeticIndexName) (enumFromTo (succ index) (pred numMembers))

-- |
-- Lambda expression, which maps a sum member by index.
sumMapper ::
  -- | Constructor name.
  Name ->
  -- | Total amount of members.
  Int ->
  -- |
  --  Lambda expression of the following form:
  --
  --  > (membersTuple -> membersTuple) -> sum -> sum
  Exp
sumMapper conName numMembers =
  LamE [mapperP] (matcher matches)
  where
    fnName =
      mkName "fn"
    mapperP =
      VarP fnName
    matches =
      [pos, neg]
      where
        pos =
          Match (Compat.conP conName memberPats) (NormalB bodyExp) []
          where
            memberVarNames =
              fmap alphabeticIndexName (enumFromTo 0 (pred numMembers))
            memberPats =
              fmap VarP memberVarNames
            bodyExp =
              AppE
                (tupleOrSingletonToProduct conName numMembers)
                (multiAppE (VarE fnName) (fmap VarE memberVarNames))
        neg =
          Match (VarP aName) (NormalB (VarE aName)) []

adtConstructorNarrower :: Name -> Int -> Exp
adtConstructorNarrower conName numMembers =
  matcher [positive, negative]
  where
    positive =
      Match (Compat.conP conName (fmap VarP varNames)) (NormalB exp) []
      where
        varNames =
          fmap alphabeticIndexName (enumFromTo 0 (pred numMembers))
        exp =
          AppE (ConE 'Just) (Compat.tupE (fmap VarE varNames))
    negative =
      Match WildP (NormalB (ConE 'Nothing)) []

enumConstructorToBool :: Name -> Exp
enumConstructorToBool constructorName =
  matcher [positive, negative]
  where
    positive =
      Match (Compat.conP constructorName []) (NormalB bodyExp) []
      where
        bodyExp =
          ConE 'True
    negative =
      Match WildP (NormalB bodyExp) []
      where
        bodyExp =
          ConE 'False

singleConstructorAdtToTuple :: Name -> Int -> Exp
singleConstructorAdtToTuple conName numMembers =
  LamE [pat] exp
  where
    varNames =
      fmap alphabeticIndexName (enumFromTo 0 (pred numMembers))
    pat =
      Compat.conP conName (fmap VarP varNames)
    exp =
      Compat.tupE (fmap VarE varNames)

tupleToProduct :: Name -> Int -> Exp
tupleToProduct conName numMembers =
  LamE [pat] exp
  where
    varNames =
      fmap alphabeticIndexName (enumFromTo 0 (pred numMembers))
    pat =
      TupP (fmap VarP varNames)
    exp =
      multiAppE (ConE conName) (fmap VarE varNames)

tupleOrSingletonToProduct :: Name -> Int -> Exp
tupleOrSingletonToProduct conName numMembers =
  if numMembers == 1
    then ConE conName
    else tupleToProduct conName numMembers

namedFieldSetter :: Name -> Exp
namedFieldSetter fieldName =
  LamE [VarP aName, VarP bName] (RecUpdE (VarE aName) [(fieldName, VarE bName)])
