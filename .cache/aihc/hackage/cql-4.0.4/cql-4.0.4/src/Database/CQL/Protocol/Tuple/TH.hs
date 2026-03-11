{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CQL.Protocol.Tuple.TH where

import Control.Applicative
import Control.Monad
import Language.Haskell.TH
import Prelude

-- Templated instances ------------------------------------------------------

genInstances :: Int -> Q [Dec]
genInstances n = join <$> mapM tupleInstance [2 .. n]

tupleInstance :: Int -> Q [Dec]
tupleInstance n = do
    let cql = mkName "Cql"
    vnames <- replicateM n (newName "a")
    let vtypes    = map VarT vnames
    let tupleType = foldl1 ($:) (TupleT n : vtypes)
    let ctx = map (AppT (ConT cql)) vtypes
    td <- tupleDecl n
    sd <- storeDecl n
    return
        [ InstanceD Nothing ctx (tcon "PrivateTuple" $: tupleType)
            [ FunD (mkName "count") [countDecl n]
            , FunD (mkName "check") [taggedDecl (var "typecheck") vnames]
            , FunD (mkName "tuple") [td]
            , FunD (mkName "store") [sd]
            ]
        , InstanceD Nothing ctx (tcon "Tuple" $: tupleType) []
        ]

countDecl :: Int -> Clause
countDecl n = Clause [] (NormalB body) []
  where
    body = con "Tagged" $$ litInt n

-- Tagged $ ident
--    [ untag (ctype :: Tagged x ColumnType)
--    , untag (ctype :: Tagged y ColumnType)
--    , ...
--    ])
taggedDecl :: Exp -> [Name] -> Clause
taggedDecl ident names = Clause [] (NormalB body) []
  where
    body  = con "Tagged" $$ (ident $$ ListE (map fn names))
    fn n  = var "untag" $$ SigE (var "ctype") (tty n)
    tty n = tcon "Tagged" $: VarT n $: tcon "ColumnType"

-- tuple v = (,)  <$> element v ctype <*> element v ctype
-- tuple v = (,,) <$> element v ctype <*> element v ctype <*> element v ctype
-- ...
tupleDecl :: Int -> Q Clause
tupleDecl n = do
    let v = mkName "v"
    Clause [VarP v, WildP] (NormalB $ body v) <$> comb
  where
    body v = UInfixE (var "combine") (var "<$>") (foldl1 star (elts v))
    elts v = replicate n (var "element" $$ VarE v $$ var "ctype")
    star   = flip UInfixE (var "<*>")
    comb   = do
        names <- replicateM n (newName "x")
        let f = NormalB $ mkTup (map VarE names)
        return [ FunD (mkName "combine") [Clause (map VarP names) f []] ]

-- store v (a, b) = put (2 :: Word16) >> putValue v (toCql a) >> putValue v (toCql b)
storeDecl :: Int -> Q Clause
storeDecl n = do
    let v = mkName "v"
    names <- replicateM n (newName "k")
    return $ Clause [VarP v, TupP (map VarP names)] (NormalB $ body v names) []
  where
#if MIN_VERSION_template_haskell(2,17,0)
    body x names = DoE Nothing (NoBindS size : map (NoBindS . value x) names)
#else
    body x names = DoE (NoBindS size : map (NoBindS . value x) names)
#endif
    size         = var "put" $$ SigE (litInt n) (tcon "Word16")
    value x v    = var "putValue" $$ VarE x $$ (var "toCql" $$ VarE v)

genCqlInstances :: Int -> Q [Dec]
genCqlInstances n = join <$> mapM cqlInstances [2 .. n]

-- instance (Cql a, Cql b) => Cql (a, b) where
--     ctype = Tagged $ TupleColumn
--         [ untag (ctype :: Tagged a ColumnType)
--         , untag (ctype :: Tagged b ColumnType)
--         ]
--     toCql (a, b) = CqlTuple [toCql a, toCql b]
--     fromCql (CqlTuple [a, b]) = (,) <$> fromCql a <*> fromCql b
--     fromCql _                 = Left "Expected CqlTuple with 2 elements."
cqlInstances :: Int -> Q [Dec]
cqlInstances n = do
    let cql = mkName "Cql"
    vnames <- replicateM n (newName "a")
    let vtypes    = map VarT vnames
    let tupleType = foldl1 ($:) (TupleT n : vtypes)
    let ctx = map (AppT (ConT cql)) vtypes
    tocql   <- toCqlDecl
    fromcql <- fromCqlDecl
    return
        [ InstanceD Nothing ctx (tcon "Cql" $: tupleType)
            [ FunD (mkName "ctype")   [taggedDecl (con "TupleColumn") vnames]
            , FunD (mkName "toCql")   [tocql]
            , FunD (mkName "fromCql") [fromcql]
            ]
        ]
  where
    toCqlDecl = do
        names <- replicateM n (newName "x")
        let tocql nme = var "toCql" $$ VarE nme
        return $ Clause
            [TupP (map VarP names)]
            (NormalB . AppE (con "CqlTuple") $ ListE $ map tocql names)
            []

    fromCqlDecl = do
        names <- replicateM n (newName "x")
        Clause
            [VarP (mkName "t")]
            (NormalB $ CaseE (var "t")
#if MIN_VERSION_template_haskell(2,18,0)
                [ Match (ParensP (ConP (mkName "CqlTuple") [] [ListP (map VarP names)]))
#else
                [ Match (ParensP (ConP (mkName "CqlTuple") [ListP (map VarP names)]))
#endif
                        (NormalB $ body names)
                        []
                , Match WildP
                        (NormalB (con "Left" $$ failure))
                        []
                ])
            <$> combine
      where
        body names = UInfixE (var "combine") (var "<$>") (foldl1 star (fn names))
        star a b   = UInfixE a (var "<*>") b
        fn names   = map (AppE (var "fromCql") . VarE) names
        combine    = do
            names <- replicateM n (newName "x")
            let f = NormalB $ mkTup (map VarE names)
            return [ FunD (mkName "combine") [Clause (map VarP names) f []] ]
        failure = LitE (StringL $ "Expected CqlTuple with " ++ show n ++ " elements")

------------------------------------------------------------------------------
-- Helpers

litInt :: Integral i => i -> Exp
litInt = LitE . IntegerL . fromIntegral

var, con :: String -> Exp
var = VarE . mkName
con = ConE . mkName

tcon :: String -> Type
tcon = ConT . mkName

($$) :: Exp -> Exp -> Exp
($$) = AppE

($:) :: Type -> Type -> Type
($:) = AppT

mkTup :: [Exp] -> Exp
#if MIN_VERSION_template_haskell(2,16,0)
mkTup = TupE . map Just
#else
mkTup = TupE
#endif
