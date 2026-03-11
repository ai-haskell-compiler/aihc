{-# LANGUAGE OverloadedStrings #-}
module WildBind.SeqSpec
    ( main
    , spec
    ) where

import           Control.Applicative       ((<*>))
import           Control.Monad             (forM_)
import           Control.Monad.IO.Class    (liftIO)
import qualified Control.Monad.Trans.State as State
import           Data.IORef                (modifyIORef, newIORef, readIORef)
import           Data.Monoid               ((<>))
import           Test.Hspec

import           WildBind.Binding          (Binding, actDescription, as, binds, boundActions,
                                            boundInputs, justBefore, on, run)
import           WildBind.Description      (ActionDescription)
import           WildBind.Seq              (fromSeq, prefix, reviseSeq, toSeq, withCancel,
                                            withPrefix)

import           WildBind.ForTest          (SampleInput (..), SampleState (..), boundDescs,
                                            checkBoundDesc, checkBoundDescs, checkBoundInputs,
                                            curBoundDesc, curBoundDescs, curBoundInputs,
                                            evalStateEmpty, execAll, withRefChecker)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  spec_prefix
  spec_SeqBinding
  spec_reviseSeq

spec_prefix :: Spec
spec_prefix = describe "prefix" $ do
  let base_b = binds $ do
        on SIa `as` "a" `run` return ()
        on SIb `as` "b" `run` return ()
  specify "no prefix" $ do
    let b = prefix [] [] base_b
    boundDescs b (SS "") `shouldMatchList`
      [ (SIa, "a"),
        (SIb, "b")
      ]
  specify "one prefix" $ evalStateEmpty $ do
    State.put $ prefix [] [SIc] base_b
    checkBoundInputs (SS "") [SIc]
    execAll (SS "") [SIc]
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
    execAll (SS "") [SIc]
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIc]
  specify "two prefixes" $ evalStateEmpty $ do
    State.put $ prefix [] [SIc, SIb] base_b
    checkBoundInputs (SS "") [SIc]
    execAll (SS "") [SIc]
    checkBoundInputs (SS "") [SIb]
    execAll (SS "") [SIb]
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIc]
  specify "cancel binding" $ evalStateEmpty $ do
    State.put $ prefix [SIa] [SIc, SIb] base_b
    checkBoundInputs (SS "") [SIc]
    execAll (SS "") [SIc] -- there is no cancel binding at the top level.
    checkBoundInputs (SS "") [SIa, SIb]
    checkBoundDesc (SS "") SIa "cancel"
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIc]
    execAll (SS "") [SIc, SIb]
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]  -- cancel binding should be weak and overridden.
    execAll (SS "") [SIb]
    checkBoundInputs (SS "") [SIc]
    execAll (SS "") [SIc, SIb]
    checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIc]


spec_SeqBinding :: Spec
spec_SeqBinding = describe "SeqBinding" $ do
  let b_a = binds $ on SIa `as` "a" `run` return ()
      b_b = binds $ on SIb `as` "b" `run` return ()
  describe "withPrefix" $ do
    it "should allow nesting" $ evalStateEmpty $ do
      State.put $ fromSeq $ withPrefix [SIb] $ withPrefix [SIc] $ withPrefix [SIa] $ toSeq (b_a <> b_b)
      checkBoundInputs (SS "") [SIb]
      execAll (SS "") [SIb]
      checkBoundInputs (SS "") [SIc]
      execAll (SS "") [SIc]
      checkBoundInputs (SS "") [SIa]
      execAll (SS "") [SIa]
      checkBoundDescs (SS "") [(SIa, "a"), (SIb, "b")]
      execAll (SS "") [SIa]
      checkBoundInputs (SS "") [SIb]
  describe "mappend" $ do
    it "should be able to combine SeqBindings with different prefixes." $ evalStateEmpty $ do
      State.put $ fromSeq $ withPrefix [SIc] $ ( (withPrefix [SIa, SIc] $ toSeq $ b_a)
                                                 <> (withPrefix [SIa] $ toSeq $ b_b)
                                               )
      checkBoundInputs (SS "") [SIc]
      execAll (SS "") [SIc]
      checkBoundInputs (SS "") [SIa]
      execAll (SS "") [SIa]
      checkBoundInputs (SS "") [SIc, SIb]
      checkBoundDesc (SS "") SIb "b"
      execAll (SS "") [SIb]
      checkBoundInputs (SS "") [SIc]
      execAll (SS "") [SIc, SIa]
      checkBoundInputs (SS "") [SIc, SIb]
      execAll (SS "") [SIc]
      checkBoundDescs (SS "") [(SIa, "a")]
      execAll (SS "") [SIa]
      checkBoundInputs (SS "") [SIc]
  describe "withCancel" $ do
    it "should weakly add 'cancel' binding when at least one prefix is kept in the state." $ evalStateEmpty $ do
      State.put $ fromSeq $ withPrefix [SIa, SIc] $ withCancel [SIa, SIb, SIc] $ ( toSeq b_a
                                                                                   <> (withPrefix [SIc] $ toSeq b_b)
                                                                                 )
      let checkPrefixOne = do
            checkBoundInputs (SS "") [SIa]
            execAll (SS "") [SIa]
            checkBoundInputs (SS "") [SIa, SIb, SIc]
            forM_ [SIa, SIb] $ \c -> checkBoundDesc (SS "") c "cancel"
      checkPrefixOne
      execAll (SS "") [SIa]
      checkPrefixOne
      execAll (SS "") [SIc]
      checkBoundInputs (SS "") [SIa, SIb, SIc]
      checkBoundDesc (SS "") SIa "a"
      checkBoundDesc (SS "") SIb "cancel"
      execAll (SS "") [SIa]
      checkPrefixOne
      execAll (SS "") [SIc, SIb]
      checkPrefixOne
      execAll (SS "") [SIc, SIc]
      checkBoundDescs (SS "") [(SIa, "cancel"), (SIb, "b"), (SIc, "cancel")]
      execAll (SS "") [SIb]
      checkPrefixOne

spec_reviseSeq :: Spec
spec_reviseSeq = describe "reviseSeq" $ do
  it "should allow access to prefix keys input so far" $ evalStateEmpty $ withRefChecker [] $ \out checkOut -> do
    act_out <- liftIO $ newIORef ("" :: String)
    let sb = withCancel [SIa] $ withPrefix [SIa, SIb, SIc] $ toSeq $ base_b
        base_b = binds $ on SIb `as` "B" `run` modifyIORef act_out (++ "B executed")
        rev ps _ _ = justBefore $ modifyIORef out (++ [ps])
    State.put $ fromSeq $ reviseSeq rev sb
    execAll (SS "") [SIa, SIa]
    checkOut [[], [SIa]]
    execAll (SS "") [SIa, SIb, SIc]
    checkOut [[], [SIa], [], [SIa], [SIa, SIb]]
    liftIO $ readIORef act_out `shouldReturn` ""
    execAll (SS "") [SIb]
    checkOut [[], [SIa], [], [SIa], [SIa, SIb], [SIa, SIb, SIc]]
    liftIO $ readIORef act_out `shouldReturn` "B executed"
  it "should allow unbinding" $ evalStateEmpty $ do
    let sb = withPrefix [SIa]
             ( toSeq ba
               <> (withPrefix [SIb] $ toSeq bab)
               <> (withPrefix [SIa] $ toSeq baa)
             )
        ba  = binds $ on SIc `as` "c on a" `run` return ()
        bab = binds $ on SIc `as` "c on ab" `run` return ()
        baa = binds $ do
          on SIc `as` "c on aa" `run` return ()
          on SIb `as` "b on aa" `run` return ()
        rev ps _ i act = if (ps == [SIa] && i == SIb) || (ps == [SIa,SIa] && i == SIc)
                         then Nothing
                         else Just act
    State.put $ fromSeq $ reviseSeq rev sb
    checkBoundInputs (SS "") [SIa]
    execAll (SS "") [SIa]
    checkBoundInputs (SS "") [SIa, SIc] -- SIb should be canceled
    execAll (SS "") [SIa]
    checkBoundDescs (SS "") [(SIb, "b on aa")] -- SIc should be canceled
    execAll (SS "") [SIb]
    checkBoundInputs (SS "") [SIa]


