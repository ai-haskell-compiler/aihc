{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Graphics.Vty as V
import Graphics.Vty ((<->))
import Graphics.Vty.CrossPlatform (mkVty)

import Control.Arrow
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.RWS (MonadReader(..), MonadState(..), RWST, execRWST, modify)

import Data.Sequence (Seq, (<|) )
import qualified Data.Sequence as Seq

eventBufferSize :: Int
eventBufferSize = 1000

type App = RWST V.Vty () (Seq String) IO

main :: IO ()
main = do
    vty <- mkVty V.defaultConfig
    _ <- execRWST (vtyInteract False) vty Seq.empty
    V.shutdown vty

vtyInteract :: Bool -> App ()
vtyInteract shouldExit = do
    updateDisplay
    unless shouldExit $ handleNextEvent >>= vtyInteract

updateDisplay :: App ()
updateDisplay = do
    let info = V.string V.defAttr "Press ESC to exit."
    eventLog <- foldMap (V.string V.defAttr) <$> get
    let pic = V.picForImage $ info <-> eventLog
    vty <- ask
    liftIO $ V.update vty pic

handleNextEvent :: App Bool
handleNextEvent = ask >>= liftIO . V.nextEvent >>= handleEvent
    where
        handleEvent e = do
            modify $ (<|) (show e) >>> Seq.take eventBufferSize
            return $ e == V.EvKey V.KEsc []
