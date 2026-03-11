module Text.Happy (runHappy, CLIFlags(..), HappyInfo(..)) where

import ProduceCode
import Parser
import ParseMonad
import AbsSyn
import LALR
import First
import Grammar
import GenUtils
import Target
-- import Text.Happy.HappyTemplate
import Data.Array( assocs, elems, (!) )
import Data.List( nub )

data HappyInfo = HappyInfo { unused :: ([Int],[String]), sr :: Int, rr :: Int}
 
runHappy :: [CLIFlags]
            -> String
            -> Either String (String, HappyInfo)
runHappy cli s = 
 case runP ourParser s 1 of
  FailP err -> Left err
  OkP abssyn@(AbsSyn _ _ _ tl) -> Right $
    case {-# SCC "Mangler" #-} (mangler "" abssyn) of
      Failed e -> die (unlines e ++ "\n")
      Succeeded g -> let 
        first     = {-# SCC "First" #-} (mkFirst g)
        closures  = {-# SCC "Closures" #-} (precalcClosure0 g)
        sets      = {-# SCC "LR0_Sets" #-} (genLR0items g closures)
        _lainfo@(spont,prop) = {-# SCC "Prop" #-} (propLookaheads g sets first)
        la      = {-# SCC "Calc" #-} (calcLookaheads (length sets) spont prop)
        items2	= {-# SCC "Merge" #-} (mergeLookaheadInfo la sets)
        goto   	= {-# SCC "Goto" #-} (genGotoTable g sets)
        action 	= {-# SCC "Action" #-} (genActionTable g first items2)
        (conflictArray,(sr,rr))   = {-# SCC "Conflict" #-} (countConflicts action)

	reduction_filter | OptGLR `elem` cli = any_reduction
	                 | otherwise         = first_reduction
        (unused_rules, unused_terminals) 
                                  = find_redundancies reduction_filter g action

	target = getTarget cli

	opt_coerce = getCoerce target cli
	opt_strict = getStrict cli
	opt_ghc = getGhc cli


        -- templ   = getTemplate 
        outfile = produceParser 
          g
          action
          goto
          (optsToInject target cli)
          Nothing
          tl
          TargetHaskell
          opt_coerce
          opt_ghc
          opt_strict
        in
          (outfile,HappyInfo (unused_rules, unused_terminals) sr rr)



die :: String -> a
die s = error s

find_redundancies 
        :: (LRAction -> [Int]) -> Grammar -> ActionTable -> ([Int], [String])
find_redundancies extract_reductions g action_table = 
	(unused_rules, map (env !) unused_terminals)
    where
	Grammar { terminals = terms,
		  token_names = env,
		  eof_term = eof,
		  starts = starts',
		  productions = productions'
	        } = g

	actions		 = concat (map assocs (elems action_table))
	start_rules	 = [ 0 .. (length starts' - 1) ]
	used_rules       = start_rules ++
			   nub [ r | (_,a) <- actions, r <- extract_reductions a ]
	used_tokens      = errorTok : eof : 
			       nub [ t | (t,a) <- actions, is_shift a ]
	n_prods		 = length productions'
	unused_terminals = filter (`notElem` used_tokens) terms
	unused_rules     = filter (`notElem` used_rules ) [0..n_prods-1]

is_shift :: LRAction -> Bool
is_shift (LR'Shift _ _)             = True
is_shift (LR'Multiple _ LR'Shift{}) = True
is_shift _                          = False

-- selects what counts as a reduction when calculating used/unused

any_reduction :: LRAction -> [Int]
any_reduction (LR'Reduce r _)    = [r] 
any_reduction (LR'Multiple as a) = concatMap any_reduction (a : as)
any_reduction _                  = []

first_reduction :: LRAction -> [Int]
first_reduction (LR'Reduce r _)   = [r] 
first_reduction (LR'Multiple _ a) = first_reduction a   -- eg R/R conflict
first_reduction _                 = []

optsToInject :: Target -> [CLIFlags] -> String
optsToInject tgt cli 
	| OptGhcTarget `elem` cli   = "-fglasgow-exts -cpp"
 	| tgt == TargetArrayBased   = "-cpp"
	| OptDebugParser `elem` cli = "-cpp"
	| otherwise                 = ""

optToTarget :: CLIFlags -> Maybe Target
optToTarget OptArrayTarget 	= Just TargetArrayBased
optToTarget _			= Nothing

data CLIFlags =
                DumpVersion
                | DumpHelp
		| OptInfoFile (Maybe String)
		| OptTemplate String
		| OptMagicName String

		| OptGhcTarget
		| OptArrayTarget
		| OptUseCoercions
		| OptDebugParser
		| OptStrict
		| OptOutputFile String
		| OptGLR
		| OptGLR_Decode
		| OptGLR_Filter
  deriving Eq


getTarget :: [CLIFlags] -> Target
getTarget cli = case [ t | (Just t) <- map optToTarget cli ] of
			(t:ts) | all (==t) ts -> t
			[]  -> TargetHaskell
			_   -> error "getTarget: multiple target options"

-- > getTemplate :: IO String -> [CLIFlags] -> IO String
-- > getTemplate def cli
-- > 	= case [ s | (OptTemplate s) <- cli ] of
-- >		[]	   -> def
-- >		f:fs       -> return (last (f:fs))
{-
> getMagicName :: [CLIFlags] -> IO (Maybe String)
> getMagicName cli
> 	= case [ s | (OptMagicName s) <- cli ] of
>		[]	   -> return Nothing
>		f:fs       -> return (Just (map toLower (last (f:fs))))
-}
getCoerce :: Target -> [CLIFlags] -> Bool
getCoerce _target cli
	= if OptUseCoercions `elem` cli 
	     then if OptGhcTarget `elem` cli
			then True
			else error ("-c/--coerce may only be used " ++
				       "in conjunction with -g/--ghc\n")
	     else False

getGhc :: [CLIFlags] ->  Bool
getGhc cli = OptGhcTarget `elem` cli

getStrict :: [CLIFlags] -> Bool
getStrict cli = OptStrict `elem` cli

