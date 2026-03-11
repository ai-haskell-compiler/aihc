
module Main where

import Control.Monad
import Data.Text.IO as T (readFile,putStrLn)
import System.Console.CmdArgs
import Data.Vector (Vector)

import Data.Forest.Static

import Biobase.Newick hiding (label)



data Options = Options
  { input :: String
  }
  deriving (Show,Data,Typeable)

oOptions = Options
  { input = def &= args
  }

main = do
  Options{..} <- cmdArgs oOptions
  let input = "../Lib-ADPfusionForest/examples/t7.nwk"
  ft <- T.readFile input
  T.putStrLn ft
  ns <- newicksFromFile input
  forM_ ns $ \n -> do
    let post :: Forest Post Vector Info = forestPost $ map getNewickTree n
    let pre  :: Forest Pre  Vector Info = forestPre  $ map getNewickTree n
    putStr "label    "
    print $ label    post
    putStr "children "
    print $ children post
    putStr "lsib     "
    print $ lsib     post
    putStr "rsib     "
    print $ rsib     post
    putStr "roots    "
    print $ roots    post
    putStr "lml      "
    print $ leftMostLeaves post
    return ()

