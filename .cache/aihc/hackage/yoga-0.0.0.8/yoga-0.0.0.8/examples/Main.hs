module Main where

--------------------------------------------------------------------------------
import qualified Yoga
--------------------------------------------------------------------------------

block :: Int -> Yoga.Layout Int
block = Yoga.exact 100.0 100.0

renderIntFn :: Yoga.LayoutInfo -> Int -> IO ()
renderIntFn info x = do
  putStr $ concat ["Node ", show x, ": "]
  print info

main :: IO ()
main = do
  let cs = [ block y | y <- [6..9]]
      cs2 = replicate 4 $ Yoga.setPadding Yoga.Edge'Left 10.0 (block 23)
      mkHbox = Yoga.hbox . Yoga.startToEnd
  let tree =
        flip Yoga.vbox 3 $
        Yoga.startToEnd [
          mkHbox cs 0,
          Yoga.setMargin Yoga.Edge'Top 10.0 $ mkHbox cs 1,
          mkHbox cs2 2]
  _ <- Yoga.render tree renderIntFn
  return ()
