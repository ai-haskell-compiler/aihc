module Visualize.Data.Char.Small where

import qualified Data.Char.Small as Small

import Data.Foldable (forM_, )


visualize :: IO ()
visualize = do
   forM_ ['\32' .. '\127'] $ \c ->
      case (Small.superscriptMaybe c, Small.subscriptMaybe c) of
         (Nothing, Nothing) -> return ()
         (super, sub) ->
            putStrLn $
               c : ' ' : maybe ' ' id super : ' ' : maybe ' ' id sub : []
