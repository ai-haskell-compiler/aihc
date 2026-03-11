
module MICP (
  -- ** Initiator Phases
  IPhase(..),

  IPhase1Priv,
  IPhase1Msg,
  iPhase1,

  IPhase2Priv,
  IPhase2Params,
  mkIPhase2Params,
  IPhase2Msg,
  iPhase2,

  IPhase3Params,
  mkIPhase3Params,
  IPhase3Msg(..),
  iPhase3,

  IPhase4Params,
  mkIPhase4Params,
  IPhase4Msg,
  iPhase4,

  IPhase5Msg,
  iPhase5,

  iGetK1Map,
  iGetK2Map,

  -- ** Responder Phases
  RPhase(..),

  RPhase1Priv,
  RPhase1Params,
  mkRPhase1Params,
  RPhase1Msg,
  rPhase1,

  RPhase2Params,
  mkRPhase2Params,
  RPhase2Msg,
  rPhase2,

  RPhase3Params,
  mkRPhase3Params,
  RPhase3Msg,
  rPhase3,

  RPhase4Params,
  mkRPhase4Params,
  RPhase4Msg,
  rPhase4,

  rGetK1Map,
  rGetK2Map

) where

import Protolude

import Crypto.Random.Types (MonadRandom(..))

import qualified Data.ByteArray as BA

import qualified Pedersen as P
import PrimeField
import MICP.Internal

-------------------------------------------------------------------------------
-- This module breaks the Mutually Independent Commitment Protocol into
-- understandable steps such that the protocol is easy to integrate into
-- existing distributed systems.
-------------------------------------------------------------------------------

-- Intiator API

data IPhase
  = IPhase1 IPhase1Msg
  | IPhase2 IPhase2Msg
  | IPhase3 IPhase3Msg
  | IPhase4 IPhase4Msg
  | IPhase5 IPhase5Msg

--------------------------
-- Initiator Phase 1
--------------------------

data IPhase1Priv = IPhase1Priv
  { iprivA :: Integer -- ^ Exponent such that g^iA = h (pedersen)
  }

data IPhase1Msg = IPhase1Msg
  { iCommitParams :: P.CommitParams -- ^ Bases to send to Responder
  }

iPhase1 :: MonadRandom m => Int -> m (IPhase1Priv, IPhase1Msg)
iPhase1 = fmap (bimap IPhase1Priv IPhase1Msg) . P.setup

--------------------------
-- Initiator Phase 2
--------------------------

data IPhase2Params = IPhase2Params
  { ip2pSecretBytes   :: [Word8]
  , ip2pRCommitParams :: P.CommitParams
  }

mkIPhase2Params :: ByteString -> RPhase1Msg -> IPhase2Params
mkIPhase2Params secret rp1msg =
  IPhase2Params
    { ip2pSecretBytes   = BA.unpack secret
    , ip2pRCommitParams = rCommitParams rp1msg
    }

data IPhase2Priv = IPhase2Priv
  { iprivK1Map  :: K1Map
  , iprivK2Map  :: K2Map
  , iprivR      :: Integer
  , iprivReveal :: P.Reveal  -- ^ Info to open the g^r commitment
  }

data IPhase2Msg = IPhase2Msg
  { iGtoK1Map   :: GtoK1Map
  , iGtoK2Map   :: GtoK2Map
  , iCommitment :: P.Commitment -- ^ Commitment of private R value
  , iC          :: Integer
  }

iPhase2 :: MonadRandom m => IPhase2Params -> SPFM m (IPhase2Priv, IPhase2Msg)
iPhase2 (IPhase2Params secretBytes rcp) = do
    (k1Map,k2Map) <- genKMaps secretBytes
    gToK1map <- kmapToGKMap k1Map
    gToK2map <- kmapToGKMap k2Map
    (r,pedersen) <- genAndCommitR rcp
    c <- genC
    let ip2Priv = IPhase2Priv k1Map k2Map r (P.reveal pedersen)
    let ip2Msg  = IPhase2Msg gToK1map gToK2map (P.commitment pedersen) c
    return (ip2Priv, ip2Msg)

--------------------------
-- Initiator Phase 3
--------------------------

data IPhase3Params = IPhase3Params
  { ip3pRCommitment   :: P.Commitment
  , ip3pRReveal       :: P.Reveal
  , ip3pRDMap         :: DMap
  , ip3pRGtoK1Map     :: GtoK1Map
  , ip3pRC            :: Integer
  , ip3pICommitParams :: P.CommitParams
  , ip3pIC            :: Integer
  , ip3pK1Map         :: K1Map
  , ip3pIR            :: Integer
  , ip3pIReveal       :: P.Reveal
  , ip3pA             :: Integer
  }

mkIPhase3Params
  :: IPhase1Priv
  -> IPhase1Msg
  -> IPhase2Priv
  -> IPhase2Msg
  -> RPhase1Msg
  -> RPhase2Msg
  -> IPhase3Params
mkIPhase3Params ip1priv ip1msg ip2priv ip2msg rp1msg rp2msg =
  IPhase3Params
    { ip3pRCommitment   = rCommit rp1msg
    , ip3pRReveal       = rReveal rp2msg
    , ip3pRDMap         = rDMap rp2msg
    , ip3pRGtoK1Map     = rGtoK1Map rp1msg
    , ip3pRC            = rC rp2msg
    , ip3pICommitParams = iCommitParams ip1msg
    , ip3pIC            = iC ip2msg
    , ip3pK1Map         = iprivK1Map ip2priv
    , ip3pIR            = iprivR ip2priv
    , ip3pIReveal       = iprivReveal ip2priv
    , ip3pA             = iprivA ip1priv
    }

data IPhase3Msg
  = IPhase3Reject
  | IPhase3Msg
      { iReveal :: P.Reveal
      , iDMap   :: DMap
      , iA      :: Integer
      }

iPhase3 :: MonadRandom m => IPhase3Params -> SPFM m IPhase3Msg
iPhase3 (IPhase3Params rcom rrev rdmap rgtok1map rc icp ic ik1map ir irev ia)
  | P.open icp rcom rrev = do
      dmapIsValid <- verifyDMap rdmap rgtok1map ic $ P.revealVal rrev
      if dmapIsValid then
        return IPhase3Msg
          { iReveal = irev
          , iDMap   = computeDMap rc ik1map ir
          , iA      = ia
          }
      else return IPhase3Reject
  | otherwise = return IPhase3Reject

--------------------------
-- Initiator Phase 4
--------------------------

data IPhase4Params = IPhase4Params
  { ip4pRA            :: Integer
  , ip4pRCommitParams :: P.CommitParams
  , ip4pRK2Map        :: K2Map
  , ip4pRGtoK2Map     :: GtoK2Map
  , ip4pIK2Map        :: K2Map
  }

mkIPhase4Params
  :: IPhase2Priv
  -> RPhase1Msg
  -> RPhase3Msg
  -> IPhase4Params
mkIPhase4Params ip2priv rp1msg rp3msg =
  IPhase4Params
    { ip4pRA            = rA rp3msg
    , ip4pRCommitParams = rCommitParams rp1msg
    , ip4pRK2Map        = rK2Map rp3msg
    , ip4pRGtoK2Map     = rGtoK2Map rp1msg
    , ip4pIK2Map        = iprivK2Map ip2priv
    }

data IPhase4Msg
  = IPhase4Reject
  | IPhase4Msg
      { iK2Map :: K2Map
      }

iGetK2Map :: IPhase4Msg -> Maybe K2Map
iGetK2Map IPhase4Reject = Nothing
iGetK2Map (IPhase4Msg k2Map) = Just k2Map

iPhase4 :: MonadRandom m => IPhase4Params -> SPFM m IPhase4Msg
iPhase4 (IPhase4Params ra rcp rk2map rgtok2map ik2map)
  | P.verifyCommitParams ra rcp = do
      gToK2Map <- kmapToGKMap rk2map
      if gToK2Map == rgtok2map then
        return IPhase4Msg
          { iK2Map = ik2map
          }
      else return IPhase4Reject
  | otherwise = return IPhase4Reject

--------------------------
-- Initiator Reveal Phase
--------------------------

data IPhase5Msg = IPhase5Msg
  { iK1Map :: K1Map }

iGetK1Map :: IPhase5Msg -> K1Map
iGetK1Map = iK1Map

iPhase5 :: IPhase2Priv -> IPhase5Msg
iPhase5 ip2priv = IPhase5Msg $ iprivK1Map ip2priv

--------------------------------------------------------------------------

-- Responder API

data RPhase
  = RPhase1 RPhase1Msg
  | RPhase2 RPhase2Msg
  | RPhase3 RPhase3Msg
  | RPhase4 RPhase4Msg

--------------------------
-- Responder Phase 1
--------------------------

data RPhase1Params = RPhase1Params
  { rp1pSecurityParam :: Int
  , rp1pSecretBytes   :: [Word8]
  , rp1pICommitParams :: P.CommitParams
  }

mkRPhase1Params :: Int -> ByteString -> IPhase1Msg -> RPhase1Params
mkRPhase1Params secParam secret ip1msg =
  RPhase1Params
    { rp1pSecurityParam = secParam
    , rp1pSecretBytes   = BA.unpack secret
    , rp1pICommitParams = iCommitParams ip1msg
    }

data RPhase1Priv = RPhase1Priv
  { rprivA      :: Integer  -- ^ Exponent such that g^rA = h (pedersen)
  , rprivK1Map  :: K1Map
  , rprivK2Map  :: K2Map
  , rprivReveal :: P.Reveal
  , rprivR      :: Integer
  }

data RPhase1Msg = RPhase1Msg
  { rCommitParams :: P.CommitParams
  , rGtoK1Map     :: GtoK1Map
  , rGtoK2Map     :: GtoK2Map
  , rCommit       :: P.Commitment -- ^ Commitment of private R value
  }

rPhase1 :: MonadRandom m => RPhase1Params -> SPFM m (RPhase1Priv, RPhase1Msg)
rPhase1 (RPhase1Params secParam secretBytes icp) = do
  (a,commitParams) <- lift $ P.setup secParam
  (k1Map,k2Map) <- genKMaps secretBytes
  gtoK1Map <- kmapToGKMap k1Map
  gtoK2Map <- kmapToGKMap k2Map
  (r,pedersen) <- genAndCommitR icp
  let rPhase1Priv = RPhase1Priv a k1Map k2Map (P.reveal pedersen) r
  let rPhase1Msg  = RPhase1Msg commitParams gtoK1Map gtoK2Map (P.commitment pedersen)
  return (rPhase1Priv, rPhase1Msg)

--------------------------
-- Responder Phase 2
--------------------------

data RPhase2Params = RPhase2Params
  { rp2pIC      :: Integer
  , rp2pRK1Map  :: K1Map
  , rp2pRReveal :: P.Reveal
  , rp2pRR      :: Integer
  }

mkRPhase2Params :: RPhase1Priv -> IPhase2Msg -> RPhase2Params
mkRPhase2Params rp1priv ip2msg =
  RPhase2Params
    { rp2pIC      = iC ip2msg
    , rp2pRK1Map  = rprivK1Map rp1priv
    , rp2pRReveal = rprivReveal rp1priv
    , rp2pRR      = rprivR rp1priv
    }

data RPhase2Msg = RPhase2Msg
  { rC      :: Integer
  , rReveal :: P.Reveal
  , rDMap   :: DMap
  }

rPhase2 :: MonadRandom m => RPhase2Params -> SPFM m RPhase2Msg
rPhase2 (RPhase2Params ic k1Map rreveal r) = do
  c <- genC
  let dmap = computeDMap ic k1Map r
  return RPhase2Msg
    { rC      = c
    , rReveal = rreveal
    , rDMap   = dmap
    }

--------------------------
-- Responder Phase 3
--------------------------

data RPhase3Params = RPhase3Params
  { rp3pRCommitParams :: P.CommitParams
  , rp3pICommitment   :: P.Commitment
  , rp3pIReveal       :: P.Reveal
  , rp3pIDMap         :: DMap
  , rp3pIGtoK1Map     :: GtoK1Map
  , rp3pRC            :: Integer
  , rp3pIA            :: Integer
  , rp3pICommitParams :: P.CommitParams
  , rp3pRK2Map        :: K2Map
  , rp3pRA            :: Integer
  }

mkRPhase3Params
  :: RPhase1Priv
  -> RPhase1Msg
  -> RPhase2Msg
  -> IPhase1Msg
  -> IPhase2Msg
  -> IPhase3Msg
  -> RPhase3Params
mkRPhase3Params rp1priv rp1msg rp2msg ip1msg ip2msg ip3msg =
  RPhase3Params
    { rp3pRCommitParams = rCommitParams rp1msg
    , rp3pICommitment   = iCommitment ip2msg
    , rp3pIReveal       = iReveal ip3msg
    , rp3pIDMap         = iDMap ip3msg
    , rp3pIGtoK1Map     = iGtoK1Map ip2msg
    , rp3pRC            = rC rp2msg
    , rp3pIA            = iA ip3msg
    , rp3pICommitParams = iCommitParams ip1msg
    , rp3pRK2Map        = rprivK2Map rp1priv
    , rp3pRA            = rprivA rp1priv
    }

data RPhase3Msg
  = RPhase3Reject
  | RPhase3Msg
      { rK2Map  :: K2Map
      , rA      :: Integer
      }

rGetK2Map :: RPhase3Msg -> K2Map
rGetK2Map = rK2Map

rPhase3 :: MonadRandom m => RPhase3Params -> SPFM m RPhase3Msg
rPhase3 (RPhase3Params rcp icom irev idmap igtoKMap rc ia icp rK2Map ra)
  | P.open rcp icom irev = do
      dmapIsValid <- verifyDMap idmap igtoKMap rc (P.revealVal irev)
      if dmapIsValid then
        return $ RPhase3Msg rK2Map ra
      else return RPhase3Reject
  | otherwise = return RPhase3Reject

--------------------------
-- Responder Reveal Phase XXX
--------------------------

data RPhase4Params = RPhase4Params
  { rp4pRK1Map    :: K1Map
  , rp4pIK2Map    :: K2Map
  , rp4pIGtoK2Map :: GtoK2Map
  }

mkRPhase4Params :: RPhase1Priv -> IPhase2Msg -> IPhase4Msg -> RPhase4Params
mkRPhase4Params rp1priv ip2msg ip4msg =
  RPhase4Params
    { rp4pRK1Map    = rprivK1Map rp1priv
    , rp4pIK2Map    = iK2Map ip4msg
    , rp4pIGtoK2Map = iGtoK2Map ip2msg
    }

-- | Final message in the protocol
data RPhase4Msg
  = RPhase4Reject
  | RPhase4Msg
      { rK1Map :: K1Map
      }

rGetK1Map :: RPhase4Msg -> K1Map
rGetK1Map = rK1Map

rPhase4 :: MonadRandom m => RPhase4Params -> SPFM m RPhase4Msg
rPhase4 (RPhase4Params rk1map ik2map igtok2map) = do
  igtok2map' <- kmapToGKMap ik2map
  if igtok2map == igtok2map' then
    return $ RPhase4Msg rk1map
  else return RPhase4Reject
