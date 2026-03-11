import Common (handleExceptionCont, )

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Subscribe.Query as Query
import qualified Sound.ALSA.Sequencer as SndSeq
import Control.Monad.Trans.Cont (ContT(ContT), )
import Control.Monad.IO.Class (liftIO, )
import Control.Monad (forM_, )
import System.Environment (getArgs, )


showQ :: Query.T -> IO String
showQ q = do
   ad <- Query.getAddr q
   qq <- Query.getQueue q
   ex <- Query.getExclusive q
   tu <- Query.getTimeUpdate q
   tr <- Query.getTimeReal q
   return (show ad ++ ":" ++ show qq 
     ++ (if tu then " update" else "")
     ++ (if tr then " realtime" else "")
     ++ (if ex then " exclusive" else ""))

main :: IO ()
main = handleExceptionCont $ do
   args <- liftIO getArgs
   h <- ContT $ SndSeq.withDefault SndSeq.Block
   arg <- ContT $ forM_ args
   a <- liftIO $ Addr.parse (h :: SndSeq.T SndSeq.InputMode) arg
   t <- ContT $ forM_ [Query.Read, Query.Write]
   liftIO $ putStrLn ("subscribers of " ++ show a ++ " for " ++ show t ++ ":")
   answers <- liftIO $ Query.queryAll h a t
   answer <- ContT $ forM_ answers
   liftIO $ putStrLn . (' ':) =<< showQ answer
