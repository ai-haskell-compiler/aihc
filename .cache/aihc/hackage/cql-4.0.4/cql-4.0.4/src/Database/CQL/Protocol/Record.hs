{-# LANGUAGE CPP          #-}
{-# LANGUAGE TypeFamilies #-}

module Database.CQL.Protocol.Record
    ( Record    (..)
    , TupleType
    , recordInstance
    ) where

import Control.Monad
import Language.Haskell.TH
import Database.CQL.Protocol.Tuple.TH (mkTup)

typeSynDecl :: Name -> Type -> Type -> Dec
#if __GLASGOW_HASKELL__ < 808
typeSynDecl x y z = TySynInstD x (TySynEqn [y] z)
#else
typeSynDecl x y z = TySynInstD (TySynEqn Nothing (AppT (ConT x) y) z)
#endif

type family TupleType a

-- | Record/Tuple conversion.
-- For example:
--
-- @
-- data Peer = Peer
--     { peerAddr :: IP
--     , peerRPC  :: IP
--     , peerDC   :: Text
--     , peerRack :: Text
--     } deriving Show
--
-- recordInstance ''Peer
--
-- map asRecord \<$\> performQuery "SELECT peer, rpc_address, data_center, rack FROM system.peers"
-- @
--
-- The generated type-class instance maps between record and tuple constructors:
--
-- @
-- type instance TupleType Peer = (IP, IP, Text, Text)
--
-- instance Record Peer where
--     asTuple (Peer a b c d) = (a, b, c, d)
--     asRecord (a, b, c, d)  = Peer a b c d
-- @
--
class Record a where
    asTuple  :: a -> TupleType a
    asRecord :: TupleType a -> a

recordInstance :: Name -> Q [Dec]
recordInstance n = do
    i <- reify n
    case i of
        TyConI d -> start d
        _        -> fail "expecting record type"

start :: Dec -> Q [Dec]
start (DataD _ tname _ _ cons _) = do
    unless (length cons == 1) $
        fail "expecting single data constructor"
    tt <- tupleType (head cons)
    at <- asTupleDecl (head cons)
    ar <- asRecrdDecl (head cons)
    return
        [ typeSynDecl (mkName "TupleType") (ConT tname) tt
        , InstanceD Nothing [] (ConT (mkName "Record") $: ConT tname)
            [ FunD (mkName "asTuple")  [at]
            , FunD (mkName "asRecord") [ar]
            ]
        ]
start _ = fail "unsupported data type"

tupleType :: Con -> Q Type
tupleType c = do
    let tt = types c
    return $ foldl1 ($:) (TupleT (length tt) : types c)
  where
    types (NormalC _ tt) = map snd tt
    types (RecC _ tt)    = map (\(_, _, t) -> t) tt
    types _              = fail "record and normal constructors only"

asTupleDecl ::Con -> Q Clause
asTupleDecl c =
    case c of
        (NormalC n t) -> go n t
        (RecC    n t) -> go n t
        _             -> fail "record and normal constructors only"
  where
    go n t = do
        vars <- replicateM (length t) (newName "a")
#if MIN_VERSION_template_haskell(2,18,0)
        return $ Clause [ConP n [] (map VarP vars)] (body vars) []
#else
        return $ Clause [ConP n (map VarP vars)] (body vars) []
#endif
    body = NormalB . mkTup . map VarE

asRecrdDecl ::Con -> Q Clause
asRecrdDecl c =
    case c of
        (NormalC n t) -> go n t
        (RecC    n t) -> go n t
        _             -> fail "record and normal constructors only"
  where
    go n t = do
        vars <- replicateM (length t) (newName "a")
        return $ Clause [TupP (map VarP vars)] (body n vars) []
    body n v = NormalB $ foldl1 ($$) (ConE n : map VarE v)

($$) :: Exp -> Exp -> Exp
($$) = AppE

($:) :: Type -> Type -> Type
($:) = AppT

