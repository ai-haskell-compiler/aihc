module Common where

import qualified Sound.ALSA.Sequencer.Connect as Connect
import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc

import Control.Monad.Trans.Cont (ContT, runContT, )

import qualified System.Exit as Exit
import qualified System.IO as IO


parseAndConnect ::
   (SndSeq.AllowOutput mode) =>
   SndSeq.T mode -> Port.T -> String -> IO Connect.T
parseAndConnect h p dest =
   Connect.createTo h p =<< Addr.parse h dest

parseDestArgs ::
   (SndSeq.AllowOutput mode) =>
   SndSeq.T mode -> Addr.T -> [String] -> IO Connect.T
parseDestArgs h me args = do
   let p = Addr.port me
   case args of
      [] -> do
         putStrLn "Enter destination or connect to a synthesizer and hit ENTER"
         destStr <- getLine
         if null destStr
           then return (Connect.toSubscribers me)
           else parseAndConnect h p destStr
      [destStr] ->
         parseAndConnect h p destStr
      _ ->
         IO.hPutStrLn IO.stderr "too many arguments"
         >>
         Exit.exitFailure

handleExceptionCont :: ContT () IO () -> IO ()
handleExceptionCont = handleException . runContUnit

handleException :: IO () -> IO ()
handleException act =
   act
   `AlsaExc.catch` \e ->
      putStrLn $ "alsa_exception: " ++ AlsaExc.show e

runContUnit :: (Monad m) => ContT a m a -> m a
runContUnit cont = runContT cont return
