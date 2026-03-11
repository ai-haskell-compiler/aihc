{-#OPTIONS_GHC -fno-warn-missing-fields#-}
module Text.Happy.Quote (
    parseHappy
  , parseHappyInfo
  , compileHappy
  , happy
  , HappyStk(..)
  , HappyInfo
  , happyWarn
  ) where

import Text.Happy(runHappy, HappyInfo(..))
import Text.Happy.HappyTemplate

import Language.Haskell.TH.Quote
import Language.Haskell.TH

import Language.Haskell.Meta

import Control.Monad(when)
import System.IO(hPutStrLn,stderr)

-- Runtime (The infixr declaration can not be spliced by TH)
data HappyStk a = HappyStk a (HappyStk a)
infixr 9 `HappyStk`


type Happy = String

compileHappy :: Happy -> Q [Dec]
compileHappy = return . either error id . parseDecs

happy :: QuasiQuoter
happy = QuasiQuoter {quoteExp = happyToExp . parseHappyInfo} -- (error "happy: pattern quoting is not supported") 


parseHappy :: String -> Happy
parseHappy = fst . parseHappyInfo

parseHappyInfo :: String -> (Happy,HappyInfo)
parseHappyInfo s = (subst old "" $ code ++ "\n" ++ happyTemplate, info)
  where
    (code,info) = either error id $ runHappy [] s
    old = unlines ["infixr 9 `HappyStk`",
                     "data HappyStk a = HappyStk a (HappyStk a)"]

happyWarn :: HappyInfo -> Q ()
happyWarn i = do
  loc <- location
  let warnMsg msg = do
      let (row,col)    = loc_start loc
          (file)       = loc_filename loc
      runIO $ hPutStrLn stderr $ file ++ ":"++show row++":"++show col++":"
      runIO $ hPutStrLn stderr $ "    " ++ msg
  when (sr i > 0) $ warnMsg $ "Warning: "++show (sr i)++"shift/reduce conflicts"
  when (rr i > 0) $ warnMsg $ "Warning: "++show (rr i)++ "reduce/reduce conflicts"


happyToExp (code,info) = happyWarn info >> litE (StringL code)

--	optIO (not (null unused_rules))
--	   (hPutStrLn stderr ("unused rules: " ++ show (length unused_rules))) >>
--	optIO (not (null unused_terminals))
--	   (hPutStrLn stderr ("unused terminals: " ++ show (length unused_terminals))) >>


-- This is some really bad code but it works for this purpose.
subst _    _  [       ] = []
subst from to xs@(a:as) =
    if isPrefixOf from xs
        then to ++ drop (length from) xs
        else a : subst from to as
    where isPrefixOf as bs = and $ zipWith (==) as bs
