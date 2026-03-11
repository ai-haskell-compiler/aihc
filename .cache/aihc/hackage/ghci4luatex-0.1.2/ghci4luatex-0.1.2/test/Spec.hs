import Test.Hspec
import Test.QuickCheck

import qualified Data.Memoizer.Commands as Cmd

type CmdResults = [(Int,Int)]

memoizeCommands :: CmdResults -> Cmd.CmdMemoizer Int Int
memoizeCommands = foldl (flip $ uncurry Cmd.storeResult) Cmd.empty

firstDiff :: Int -> CmdResults -> Cmd.CmdMemoizer Int Int -> (Int, Cmd.CmdMemoizer Int Int)
firstDiff acc [] m = (acc, Cmd.deleteResult m)
firstDiff acc ((a,b):q) m =
  case Cmd.lookup a m of
    Nothing -> (acc, Cmd.storeResult a b m)
    Just b' ->
      if b == b' then
        firstDiff (acc + 1) q (Cmd.nextCmd m)
      else
        (acc,Cmd.storeResult a b m)

main :: IO ()
main = hspec $ do
  describe "Data.Memoizer.Commands" $ do
    it "Cannot guess when not memoized" $ property $ \a l->
      Cmd.lookup a (memoizeCommands l) `shouldBe` Nothing
    it "Memoizes first command" $ property $ \a b l ->
      Cmd.lookup a (Cmd.restart (memoizeCommands ((a,b): l)))
      `shouldBe` Just b
    it "Memoizes second command" $ property $ \a b l a' b' ->
      Cmd.lookup a' (Cmd.nextCmd (Cmd.restart (memoizeCommands ((a,b):(a',b'): l))))
      `shouldBe` Just b'
    it "Finds the first modification" $ property $ \a b a' l1 l2 ->
      let l   = l1 ++ [(a,b)] ++ l2
          l'  = l1 ++ [(a',b)] ++ l2
      in
      if a == a' then
        fst (firstDiff 0 l' (Cmd.restart (memoizeCommands l)))
          `shouldBe` length l'
      else
        fst (firstDiff 0 l' (Cmd.restart (memoizeCommands l)))
          `shouldBe` length l1
    it "Do not memoize after first modif" $ property $ \a'' a b a' l1 l2 ->
      let l   = l1 ++ [(a,b)] ++ l2
          l'  = l1 ++ [(a',b)] ++ l2
      in
      (Cmd.lookup a'' $
        snd $ firstDiff 0 l' $ Cmd.restart $ memoizeCommands l)
        `shouldBe` Nothing
