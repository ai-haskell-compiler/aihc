{-# LANGUAGE OverloadedStrings #-}
module Data.XML.Pickle.Tuples
  ( tErr
  , getRest
  , xp2Tuple
  , xp3Tuple
  , xp4Tuple
  , xp5Tuple
  , xp6Tuple
  )
  where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.XML.Pickle.Basic

-- | Try to extract the remainig elements, fail if there are none.
getRest :: UnpickleResult [a] b -> UnpickleResult [a] (b, [a])
getRest (Result r (Just t)) = Result (r, t) Nothing
getRest (Result r Nothing) = Result (r, []) Nothing
getRest (NoResult e) = missingE $ Text.unpack e
getRest (UnpickleError e) = UnpickleError e

tErr :: Text -> UnpickleResult t a -> UnpickleResult t a
tErr tr = mapUnpickleError (("tuple", tr) <++>)

-- | Combines 2 picklers.
xp2Tuple :: PU [a] b1 -> PU [a] b2 -> PU [a] (b1, b2)
xp2Tuple xp1 xp2 = "xp2Tuple" <??>
                   PU {pickleTree = \(t1, t2) ->
                        pickleTree xp1 t1 ++ pickleTree xp2 t2
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    -- The @Either String@ monad.
    (x1 ,r1) <- tErr "1" . getRest $ unpickleTree xp1 r0
    x2 <- tErr "2" $ unpickleTree xp2 r1
    return (x1,x2)

-- | Combines 3 picklers.
xp3Tuple :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] (a1, a2, a3)
xp3Tuple xp1 xp2 xp3 = "xp3Tuple" <??> PU {pickleTree = \(t1, t2, t3) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    (x1, r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2, r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    x3 <- tErr "3" $ unpickleTree xp3 r2
    return (x1,x2,x3)

-- | Combines 4 picklers.
xp4Tuple :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4
             -> PU [a] (a1, a2, a3,a4)
xp4Tuple xp1 xp2 xp3 xp4
     = "xp4Tuple" <??>
       PU {pickleTree = \(t1, t2, t3, t4) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    (x1 , r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2 , r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    (x3 , r3) <- tErr "3" $ getRest $ unpickleTree xp3 r2
    x4 <- tErr "4" $ unpickleTree xp4 r3
    return (x1,x2,x3,x4)

-- | Combines 5 picklers.
xp5Tuple :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4 -> PU [a] a5
             -> PU [a] (a1, a2, a3, a4, a5)
xp5Tuple xp1 xp2 xp3 xp4 xp5
  = "xp5Tuple" <??>
    PU {pickleTree = \(t1, t2, t3, t4, t5) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                        ++ pickleTree xp5 t5
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    (x1 , r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2 , r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    (x3 , r3) <- tErr "3" $ getRest $ unpickleTree xp3 r2
    (x4 , r4) <- tErr "4" $ getRest $ unpickleTree xp4 r3
    x5 <- tErr "5" $ unpickleTree xp5 r4
    return (x1,x2,x3,x4,x5)

-- | You guessed it ... Combines 6 picklers.
xp6Tuple :: PU [a] a1 -> PU [a] a2 -> PU [a] a3 -> PU [a] a4 -> PU [a] a5
             -> PU [a] a6
             -> PU [a] (a1, a2, a3, a4, a5, a6)
xp6Tuple xp1 xp2 xp3 xp4 xp5 xp6
  = "xp6Tuple" <??>
    PU {pickleTree = \(t1, t2, t3, t4, t5, t6) ->
                        pickleTree xp1 t1
                        ++ pickleTree xp2 t2
                        ++ pickleTree xp3 t3
                        ++ pickleTree xp4 t4
                        ++ pickleTree xp5 t5
                        ++ pickleTree xp6 t6
                    , unpickleTree = doUnpickleTree
                    } where
  doUnpickleTree r0 = do
    (x1 , r1) <- tErr "1" $ getRest $ unpickleTree xp1 r0
    (x2 , r2) <- tErr "2" $ getRest $ unpickleTree xp2 r1
    (x3 , r3) <- tErr "3" $ getRest $ unpickleTree xp3 r2
    (x4 , r4) <- tErr "4" $ getRest $ unpickleTree xp4 r3
    (x5 , r5) <- tErr "5" $ getRest $ unpickleTree xp5 r4
    x6 <- tErr "6" $ unpickleTree xp6 r5
    return (x1,x2,x3,x4,x5,x6)
