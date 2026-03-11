{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Example (
  micpWrapper,
  micpComponents,

  testPedersen,
  testBlumMicaliPRNG,
) where

import Protolude hiding (hash)

import Control.Concurrent.MVar

import Crypto.Hash
import Crypto.Number.Serialize (os2ip)
import Crypto.Random.Types (MonadRandom(..))

import qualified Data.ByteArray as BA
import Data.Maybe (fromJust)

import MICP
import MICP.Internal
import Pedersen
import PrimeField

testBlumMicaliPRNG :: IO Integer
testBlumMicaliPRNG = do
  let k = 256
  (a,cparams) <- setup k
  let spf = pedersenSPF cparams
  seed <- genPRNGSeed spf
  blumMicaliPRNG k seed spf

testPedersen :: ByteString -> IO Bool
testPedersen bs = do
  let hashedBs = os2ip $ sha256 bs
  (a,commitParams) <- setup 256 -- hashStorage uses sha256
  (Pedersen c r) <- commit hashedBs commitParams
  return $ open commitParams c r

-- | This example illustrates how you might implement the server logic for two
-- parties to use MICP in a distributed network. MVars are used to simulate
-- message passing, but can be replaced with any message passing construct.
-- Note: this example does not handle Reject messages properly.
micpWrapper :: Int -> IO Bool
micpWrapper nbits = do

    -- MVars for message passing between I and R
    iMVar <- newEmptyMVar
    rMVar <- newEmptyMVar
    -- MVars for MICP thread reporting result
    iResMVar <- newEmptyMVar
    rResMVar <- newEmptyMVar

    let aliceSecret = sha256 "123456789"
    let bobSecret = sha256 "987654321"

    -- Generate shared Safe Prime Field
    spf <- mkSPF nbits
    forkIO $ void $ runSPFT spf $ -- Alice thread
      alice aliceSecret iMVar rMVar iResMVar
    forkIO $ void $ runSPFT spf $ -- Bob thread
      bob bobSecret rMVar iMVar rResMVar

    -- Each party should have computed each other's secret
    iRes <- takeMVar iResMVar
    rRes <- takeMVar rResMVar

    return $ iRes == bobSecret && rRes == aliceSecret
  where
    alice
      :: ByteString
      -> MVar IPhase
      -> MVar RPhase
      -> MVar ByteString
      -> SPFM IO ()
    alice secret ipMVar rpMVar resMVar = do

      -- Phase 1
      (ip1priv, ip1Msg) <- lift $ iPhase1 nbits
      liftIO $ putMVar ipMVar $ IPhase1 ip1Msg
      (RPhase1 rp1msg) <- liftIO $ takeMVar rpMVar

      -- Phase 2
      let ip2params = mkIPhase2Params secret rp1msg
      (ip2priv, ip2Msg) <- iPhase2 ip2params
      liftIO $ putMVar ipMVar $ IPhase2 ip2Msg
      (RPhase2 rp2msg) <- liftIO $ takeMVar rpMVar

      -- Phase 3 (Should case match on rp3msg for RPhase3Reject)
      let ip3params = mkIPhase3Params ip1priv ip1Msg ip2priv ip2Msg rp1msg rp2msg
      ip3Msg <- iPhase3 ip3params
      liftIO $ putMVar ipMVar $ IPhase3 ip3Msg
      (RPhase3 rp3msg) <- liftIO $ takeMVar rpMVar

      -- Phase 4 (Should case match on rp4msg for RPhase4Reject)
      let ip4params = mkIPhase4Params ip2priv rp1msg rp3msg
      ip4Msg <- iPhase4 ip4params
      liftIO $ putMVar ipMVar $ IPhase4 ip4Msg
      (RPhase4 rp4msg) <- liftIO $ takeMVar rpMVar

      -- Phase 5
      let ip5Msg = iPhase5 ip2priv
      liftIO $ putMVar ipMVar $ IPhase5 ip5Msg

      -- Compute bob's secret
      let k1Map = rGetK1Map rp4msg
      let k2Map = rGetK2Map rp3msg
      rSecret <- micpReveal k1Map k2Map

      liftIO $ putMVar resMVar rSecret

    bob
      :: ByteString
      -> MVar RPhase
      -> MVar IPhase
      -> MVar ByteString
      -> SPFM IO ()
    bob secret rpMVar ipMVar resMVar = do

      -- Phase 1
      (IPhase1 ip1msg) <- liftIO $ takeMVar ipMVar
      let rp1params = mkRPhase1Params nbits secret ip1msg
      (rp1priv, rp1Msg) <- rPhase1 rp1params
      liftIO $ putMVar rpMVar $ RPhase1 rp1Msg

      -- Phase 2
      (IPhase2 ip2msg) <- liftIO $ takeMVar ipMVar
      let rp2params = mkRPhase2Params rp1priv ip2msg
      rp2Msg <- rPhase2 rp2params
      liftIO $ putMVar rpMVar $ RPhase2 rp2Msg

      -- Phase 3 (Should case match on ip3msg for IPhase3Reject)
      (IPhase3 ip3msg) <- liftIO $ takeMVar ipMVar
      case ip3msg of
        IPhase3Reject -> panic "IPhase3Reject"
        _ -> do
          let rp3params = mkRPhase3Params rp1priv rp1Msg rp2Msg ip1msg ip2msg ip3msg
          rp3Msg <- rPhase3 rp3params
          liftIO $ putMVar rpMVar $ RPhase3 rp3Msg

      -- Phase 4 (Should case match on ip4msg for IPhase4Reject)
      (IPhase4 ip4msg) <- liftIO $ takeMVar ipMVar
      let rp4params = mkRPhase4Params rp1priv ip2msg ip4msg
      rp4Msg <- rPhase4 rp4params
      liftIO $ putMVar rpMVar $ RPhase4 rp4Msg

      -- Phase 5
      (IPhase5 ip5msg) <- liftIO $ takeMVar ipMVar

      -- Compute Alice's secret
      let k1Map = iGetK1Map ip5msg
      let k2Map = fromJust $ iGetK2Map ip4msg
      aliceSecret <- micpReveal k1Map k2Map

      liftIO $ putMVar resMVar aliceSecret

-- | In this test, all values computed are in scope for both Alice & Bob, so
-- instead of "sending" those values to one another, we can just use them for
-- the respective counterparty computations.
micpComponents :: Int -> IO Bool
micpComponents secParam = do
  let aliceMsg = sha256 "123456789"
  let aliceMsgBytes = BA.unpack aliceMsg
  let bobMsg   = sha256 "987654321"
  let bobMsgBytes = BA.unpack bobMsg

  putText "\nCreating Shared SPF and Local Params..."
  sharedSPF <- mkSPF secParam

  -- 1, 2(a): send pedersen bases to each other
  (aliceA, aCommitParams) <- setup secParam
  (bobA, bCommitParams) <- setup secParam

  -- All further computation takes places in SPF
  runSPFT sharedSPF $ do

    -- 2(b): Send bobGKMap to alice
    putText "Gen bob kmap"
    (bobKMap,bobK'Map) <- genKMaps bobMsgBytes
    bobGtoKMap <- kmapToGKMap bobKMap
    bobGtoK'Map <- kmapToGKMap bobK'Map

    -- 2(c): Send bobCommit to alice using alice params
    putText "Gen bob r"
    (bobR, bobPedersen) <- genAndCommitR aCommitParams
    let (Pedersen bobCommitment bobReveal) = bobPedersen

    -- 3(a): Send aliceGKMap to bob
    putText "Gen alice kmap"
    (aliceKMap, aliceK'Map) <- genKMaps aliceMsgBytes
    aliceGtoKMap <- kmapToGKMap aliceKMap
    aliceGtoK'Map <- kmapToGKMap aliceK'Map

    -- 3(b): Send aliceCommit to bob
    putText "Gen alice r"
    (aliceR, alicePedersen) <- genAndCommitR bCommitParams
    let (Pedersen aliceCommitment aliceReveal) = alicePedersen

    -- 3(c): Send aliceC to bob
    putText "Gen alice c"
    aliceC <- genC

    -- 4(a): Send bobC to alice
    putText "Gen bob c"
    bobC <- genC

    -- 4(b): Send bobReveal to alice

    -- 4(c): Send bobDMap to alice
    putText "Compute bob dmap"
    let bobDMap = computeDMap aliceC bobKMap bobR

    -- 5(a): alice checks bob's commit
    unless (open aCommitParams bobCommitment bobReveal) $
      panic "Bob's commit is illegitimate!"
    --       alice verifies g^di = (g^ki)^c + g^r
    bobDMapVerified <- verifyDMap bobDMap bobGtoKMap aliceC $ revealVal bobReveal
    unless bobDMapVerified $
      panic "Bob's computations are wrong!"

    -- 5(b): Send aliceReveal to bob

    -- 5(c): Send aliceDMap to bob
    putText "Compute alice dmap"
    let aliceDMap = computeDMap bobC aliceKMap aliceR

    -- 5(d): send alice's 'a' to bob

    -- 6(a): bob checks alice's commit
    unless (open bCommitParams aliceCommitment aliceReveal) $
      panic "Alice's commit is illegitimate!"
    --       bob verifies g^di = (g^ki)^c + g^r
    aliceDMapVerified <- verifyDMap aliceDMap aliceGtoKMap bobC $ revealVal aliceReveal
    unless aliceDMapVerified $
      panic "Alice's computations are wrong!"

    -- 6(b): bob checks that alice's ga^a == ha
    unless (verifyCommitParams aliceA aCommitParams) $
      panic "Alice's pedersen bases are not valid!"

    -- 6(c): bob sends k'map and bob's 'a' to alice

    -- 7(a): alice checks that bob's ga^a == ha
    unless (verifyCommitParams bobA bCommitParams) $
      panic "Bob's pedersen bases are not valid!"

    -- 7(b): alice checks k'map from bob matches gk'map received earlier
    bobGtoK'MapCheck <- kmapToGKMap bobK'Map
    unless (bobGtoK'MapCheck == bobGtoK'Map) $
      panic "Bob's k' and gk' maps are invalid!"

    -- 7(c): alice sends k'map to bob

    -- 8(a): bob checks k'map from alice matches gk'map recieved earlier
    aliceGtoK'MapCheck <- kmapToGKMap aliceK'Map
    unless (aliceGtoK'MapCheck == aliceGtoK'Map) $
      panic "Alice's k' and gk' maps are invalid!"

    -- REVEAL STAGE:
    -- Alice & Bob reveal kMaps (map of k only, no k')

    -- Using bob/alice env respectively to show this reveal can happen within
    -- the shared env only, and doesn't care about local pedersen params
    aliceMsgRes <- micpReveal aliceKMap aliceK'Map
    let aliceResEqMsg = aliceMsgRes == aliceMsg
    bobMsgRes <- micpReveal bobKMap bobK'Map
    let bobResEqMsg = bobMsgRes == bobMsg

    return $ aliceResEqMsg && bobResEqMsg

sha256 :: ByteString -> ByteString
sha256 bs = BA.convert (hash bs :: Digest SHA3_256)
