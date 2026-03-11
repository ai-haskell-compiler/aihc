













> module LALR
>	(genActionTable, genGotoTable, genLR0items, precalcClosure0,
>	 propLookaheads, calcLookaheads, mergeLookaheadInfo, countConflicts,
>	 Lr0Item(..), Lr1Item)
>	where


> import GenUtils
> import Data.Set ( Set )
> import qualified Data.Set as Set hiding ( Set )
> import qualified NameSet
> import NameSet ( NameSet )
> import Grammar


> import Control.Monad.ST
> import Data.Array.ST
> import Data.Array as Array
> import Data.List (nub)


> unionMap :: (Ord b) => (a -> Set b) -> Set a -> Set b
> unionMap f = Set.fold (Set.union . f) Set.empty


> unionNameMap :: (Name -> NameSet) -> NameSet -> NameSet
> unionNameMap f = NameSet.fold (NameSet.union . f) NameSet.empty






> data Lr0Item = Lr0 {-#UNPACK#-}!Int {-#UNPACK#-}!Int			-- (rule, dot)
>       deriving (Eq,Ord)


> data Lr1Item = Lr1 {-#UNPACK#-}!Int {-#UNPACK#-}!Int NameSet  -- (rule, dot, lookahead)
> type RuleList = [Lr0Item]














> precalcClosure0 :: Grammar -> Name -> RuleList
> precalcClosure0 g = 
>	\n -> case lookup n info' of
>		Nothing -> []
>		Just c  -> c
>  where
>
>	info' :: [(Name, RuleList)]
>	info' = map (\(n,rules) -> (n,map (\rule -> Lr0 rule 0) (NameSet.toAscList rules))) info


>	info :: [(Name, NameSet)]
>	info = mkClosure (==) (\f -> map (follow f) f)
>			(map (\nt -> (nt,NameSet.fromList (lookupProdsOfName g nt))) nts)


>	follow :: [(Name, NameSet)] -> (Name, NameSet) -> (Name, NameSet)
>	follow f (nt,rules) = (nt, unionNameMap (followNT f) rules `NameSet.union` rules)


>	followNT :: [(Name, NameSet)] -> Int -> NameSet
>	followNT f rule = 
>		case findRule g rule 0 of
>			Just nt	| nt >= firstStartTok && nt < fst_term ->
>				case lookup nt f of
>					Just rs -> rs
>					Nothing -> error "followNT"
>			_ -> NameSet.empty


>	nts = non_terminals g
>	fst_term = first_term g


> closure0 :: Grammar -> (Name -> RuleList) -> Set Lr0Item -> Set Lr0Item
> closure0 g closureOfNT set = Set.fold addRules Set.empty set
>    where
> 	fst_term = first_term g
>	addRules rule set' = Set.union (Set.fromList (rule : closureOfRule rule)) set'
> 
>	closureOfRule (Lr0 rule dot) = 
>           case findRule g rule dot of 
>           	(Just nt) | nt >= firstStartTok && nt < fst_term 
>		   -> closureOfNT nt
>               _  -> []








> closure1 :: Grammar -> ([Name] -> NameSet) -> [Lr1Item] -> [Lr1Item]
> closure1 g first set
>       = fst (mkClosure (\(_,new) _ -> null new) addItems ([],set))
>	where
>	fst_term = first_term g


>	addItems :: ([Lr1Item],[Lr1Item]) -> ([Lr1Item],[Lr1Item])
>	addItems (old_items, new_items) = (new_old_items, new_new_items)
>	  where
>		new_old_items = new_items `union_items` old_items
>		new_new_items = subtract_items 
>				   (foldr union_items [] (map fn new_items))
>					new_old_items


>		fn :: Lr1Item -> [Lr1Item]
>		fn (Lr1 rule dot as) =
>		    case lookupProdNo g rule of { (_name,lhs,_,_) ->
>		    case drop dot lhs of
>			(b:beta) | b >= firstStartTok && b < fst_term ->
>			    let terms = unionNameMap 
>						(\a -> first (beta ++ [a])) as
>			    in
>			    [ (Lr1 rule' 0 terms) | rule' <- lookupProdsOfName g b ]
>			_ -> []
>		    }






> subtract_items :: [Lr1Item] -> [Lr1Item] -> [Lr1Item]
> subtract_items items1 items2 = foldr (subtract_item items2) [] items1










> subtract_item :: [Lr1Item] -> Lr1Item -> [Lr1Item] -> [Lr1Item]
> subtract_item [] i result = i : result
> subtract_item ((Lr1 rule dot as):items) i@(Lr1 rule' dot' as') result =
>	case compare rule' rule of
>		LT -> i : result
>		GT -> carry_on
>		EQ -> case compare dot' dot of
>			LT -> i : result
>			GT -> carry_on
>			EQ -> case NameSet.difference as' as of
>				bs | NameSet.null bs -> result
>				   | otherwise -> (Lr1 rule dot bs) : result
>  where
>	carry_on = subtract_item items i result






> union_items :: [Lr1Item] -> [Lr1Item] -> [Lr1Item]
> union_items is [] = is
> union_items [] is = is
> union_items (i@(Lr1 rule dot as):is) (i'@(Lr1 rule' dot' as'):is') =
>	case compare rule rule' of
>		LT -> drop_i
>		GT -> drop_i'
>		EQ -> case compare dot dot' of
>			LT -> drop_i
>			GT -> drop_i'
>			EQ -> (Lr1 rule dot (as `NameSet.union` as')) : union_items is is'
>  where
>	drop_i  = i  : union_items is (i':is')
>	drop_i' = i' : union_items (i:is) is'
















> gotoClosure :: Grammar -> Set Lr0Item -> Name -> Set Lr0Item
> gotoClosure gram i x = unionMap fn i
>    where
>       fn (Lr0 rule_no dot) =
>          case findRule gram rule_no dot of
>               Just t | x == t -> Set.singleton (Lr0 rule_no (dot+1))
>               _ -> Set.empty           
























> type ItemSetWithGotos = (Set Lr0Item, [(Name,Int)])


> genLR0items :: Grammar -> (Name -> RuleList) -> [ItemSetWithGotos]
> genLR0items g precalcClosures
>	= fst (mkClosure (\(_old,new) _ -> null new)
>               addItems
>                 (([],startRules)))
>  where


>    n_starts = length (starts g)
>    startRules :: [Set Lr0Item]
>    startRules = [ Set.singleton (Lr0 rule 0) | rule <- [0..n_starts] ]


>    tokens = non_terminals g ++ terminals g


>    addItems :: ([ItemSetWithGotos], [Set Lr0Item])
>	      -> ([ItemSetWithGotos], [Set Lr0Item])
>	      
>    addItems (oldSets,newSets) = (newOldSets, reverse newNewSets)
>     where
>	
>	newOldSets = oldSets ++ (zip newSets intgotos)


>	itemSets = map fst oldSets ++ newSets








>	gotos :: [[(Name,Set Lr0Item)]]
>	gotos = map (filter (not . Set.null . snd))
>	    (map (\i -> let i' = closure0 g precalcClosures i in
>	    		[ (x,gotoClosure g i' x) | x <- tokens ]) newSets)










































>	numberSets 
>		:: [(Name,Set Lr0Item)] 
>		-> (Int,
>		    [[(Name,Int)]],
>		    [Set Lr0Item])
>		-> (Int, [[(Name,Int)]], [Set Lr0Item])
>
>	numberSets [] (i,gotos',newSets') = (i,([]:gotos'),newSets')
>	numberSets ((x,gotoix):rest) (i,g':gotos',newSets')
>	   = numberSets rest
>	   	(case indexInto 0 gotoix (itemSets ++ reverse newSets') of
>			Just j  -> (i,  ((x,j):g'):gotos', newSets')
>			Nothing -> (i+1,((x,i):g'):gotos', gotoix:newSets'))
>	numberSets _ _ = error "genLR0items/numberSets: Unhandled case"






>	intgotos :: [[(Name,Int)]]
>	newNewSets  :: [Set Lr0Item]
>	(_, ([]:intgotos), newNewSets) =
>		foldr numberSets (length newOldSets, [[]], []) gotos


> indexInto :: Eq a => Int -> a -> [a] -> Maybe Int
> indexInto _ _ []		   = Nothing
> indexInto i x (y:ys) | x == y    = Just i
>		       | otherwise = indexInto (i+1) x ys














> propLookaheads 
>	:: Grammar
>	-> [(Set Lr0Item,[(Name,Int)])]		-- LR(0) kernel sets
>	-> ([Name] -> NameSet)			-- First function
>	-> (
>		[(Int, Lr0Item, NameSet)],	-- spontaneous lookaheads
>		Array Int [(Lr0Item, Int, Lr0Item)]	-- propagated lookaheads
>	   )


> propLookaheads gram sets first = (concat s, array (0,length sets - 1) 
>			[ (a,b) | (a,b) <- p ])
>   where


>     (s,p) = unzip (zipWith propLASet sets [0..])


>     propLASet :: (Set Lr0Item, [(Name, Int)]) -> Int -> ([(Int, Lr0Item, NameSet)],(Int,[(Lr0Item, Int, Lr0Item)]))
>     propLASet (set,goto) i = (start_spont ++ concat s', (i, concat p'))
>	where


>	  (s',p') = unzip (map propLAItem (Set.toAscList set))


>	  -- spontaneous EOF lookaheads for each start state & rule...
>	  start_info :: [(String, Name, Name, Bool)]
>	  start_info = starts gram	


>	  start_spont :: [(Int, Lr0Item ,NameSet)]
>	  start_spont	= [ (start, (Lr0 start 0), 
>			     NameSet.singleton (startLookahead gram partial))
>			  | (start, (_,_,_,partial)) <- 
>				zip [ 0 .. length start_info - 1] start_info]


>	  propLAItem :: Lr0Item -> ([(Int, Lr0Item, NameSet)], [(Lr0Item, Int, Lr0Item)])
>	  propLAItem item@(Lr0 rule dot) = (spontaneous, propagated)
>	    where


>		j = closure1 gram first [Lr1 rule dot (NameSet.singleton dummyTok)]


>		spontaneous :: [(Int, Lr0Item, NameSet)]
>		spontaneous = concat [ 
>		 (case findRule gram rule' dot' of
>		     Nothing -> []
>		     Just x  -> case lookup x goto of
>			 	  Nothing -> error "spontaneous"
>				  Just k  ->
>					case NameSet.filter (/= dummyTok) ts of
>					   ts' | NameSet.null ts' -> []
>					       | otherwise -> [(k, Lr0 rule' (dot' + 1), ts')])
>			| (Lr1 rule' dot' ts) <- j ]


>		propagated :: [(Lr0Item, Int, Lr0Item)]
>		propagated = concat [
>		 (case findRule gram rule' dot' of
>		     Nothing -> []
>		     Just x  -> case lookup x goto of
>				  Nothing -> error "propagated"
>				  Just k  -> [(item, k, Lr0 rule' (dot' + 1))])
>			| (Lr1 rule' dot' ts) <- j, dummyTok `elem` (NameSet.toAscList ts) ]












> startLookahead :: Grammar -> Bool -> Name
> startLookahead gram partial = if partial then errorTok else eof_term gram












> calcLookaheads
>	:: Int					-- number of states
>	-> [(Int, Lr0Item, NameSet)]		-- spontaneous lookaheads
>	-> Array Int [(Lr0Item, Int, Lr0Item)]	-- propagated lookaheads
>	-> Array Int [(Lr0Item, NameSet)]


> calcLookaheads n_states spont prop
>	= runST (do
>	    arr <- newArray (0,n_states) []
>	    propagate arr (foldr fold_lookahead [] spont)
>	    freeze arr
>	)


>   where
>	propagate :: STArray s Int [(Lr0Item, NameSet)]
>			 -> [(Int, Lr0Item, NameSet)] -> ST s ()
>	propagate _   []  = return ()
>	propagate arr new = do 
>		let
>		   items = [ (i,item'',s) | (j,item,s) <- new, 
>				            (item',i,item'') <- prop ! j,
>				            item == item' ]
>		new_new <- get_new arr items []
>		add_lookaheads arr new
>		propagate arr new_new










> add_lookaheads :: STArray s Int [(Lr0Item, NameSet)]
>                -> [(Int, Lr0Item, NameSet)]
>                -> ST s ()
> add_lookaheads _      [] = return ()
> add_lookaheads arr ((i,item,s) : lookaheads) = do
>	las <- readArray arr i
>	writeArray arr i (add_lookahead item s las)
>	add_lookaheads arr lookaheads


> get_new :: STArray s Int [(Lr0Item, NameSet)]
>         -> [(Int, Lr0Item, NameSet)]
>         -> [(Int, Lr0Item, NameSet)]
>         -> ST s [(Int, Lr0Item, NameSet)]
> get_new _   []                   new = return new
> get_new arr (l@(i,_item,_s):las) new = do
>	state_las <- readArray arr i
>	get_new arr las (get_new' l state_las new)


> add_lookahead :: Lr0Item -> NameSet -> [(Lr0Item,NameSet)] ->
> 			[(Lr0Item,NameSet)]
> add_lookahead item s [] = [(item,s)]
> add_lookahead item s (m@(item',s') : las)
>	| item == item' = (item, s `NameSet.union` s') : las
>	| otherwise     = m : add_lookahead item s las


> get_new' :: (Int,Lr0Item,NameSet) -> [(Lr0Item,NameSet)] ->
>		 [(Int,Lr0Item,NameSet)] -> [(Int,Lr0Item,NameSet)]
> get_new' l [] new = l : new
> get_new' l@(i,item,s) ((item',s') : las) new
>	| item == item' =
>		let s'' = NameSet.filter (\x -> not (NameSet.member x s')) s in
>		if NameSet.null s'' then new else
>		((i,item,s''):new)
>	| otherwise = 
>		get_new' l las new


> fold_lookahead :: (Int,Lr0Item,NameSet) -> [(Int,Lr0Item,NameSet)]
>		-> [(Int,Lr0Item,NameSet)]
> fold_lookahead l [] = [l]
> fold_lookahead l@(i,item,s) (m@(i',item',s'):las)
>  	| i == i' && item == item' = (i,item, s `NameSet.union` s'):las
>	| i < i' = (i,item,s):m:las
>	| otherwise = m : fold_lookahead l las










      -> [(Int, Lr0Item, Set Name)]		-- spontaneous lookaheads
      -> Array Int [(Lr0Item, Int, Lr0Item)]	-- propagated lookaheads
      -> Array Int [(Lr0Item, Set Name)]




      = rebuildArray $ fst (mkClosure (\(_,new) _ -> null new) propagate






        rebuildArray :: [(Int, Lr0Item, Set Name)] -> Array Int [(Lr0Item, Set Name)]
















      	   new_new = foldr (\i new -> getNew i las new) [] items








addLookahead :: (Int,Lr0Item,Set Name) -> [(Int,Lr0Item,Set Name)]
      	-> [(Int,Lr0Item,Set Name)]












getNew :: (Int,Lr0Item,Set Name) -> [(Int,Lr0Item,Set Name)]
      -> [(Int,Lr0Item,Set Name)] -> [(Int,Lr0Item,Set Name)]




























> mergeLookaheadInfo
>	:: Array Int [(Lr0Item, NameSet)] 	-- lookahead info
>	-> [(Set Lr0Item, [(Name,Int)])] 	-- state table
>	-> [ ([Lr1Item], [(Name,Int)]) ]


> mergeLookaheadInfo lookaheads sets
>	= zipWith mergeIntoSet sets [0..]
>	where


>	  mergeIntoSet :: (Set Lr0Item, [(Name, Int)]) -> Int -> ([Lr1Item], [(Name, Int)])
>	  mergeIntoSet (items, goto) i
>		= (concat (map mergeIntoItem (Set.toAscList items)), goto)
>		where


>	  	  mergeIntoItem :: Lr0Item -> [Lr1Item]
>	  	  mergeIntoItem item@(Lr0 rule dot)
>		     = [Lr1 rule dot la]
>		     where la = case [ s | (item',s) <- lookaheads ! i,
>					    item == item' ] of
>					[] -> NameSet.empty
>					[x] -> x
>					_ -> error "mergIntoItem"


















> genGotoTable :: Grammar -> [(Set Lr0Item,[(Name,Int)])] -> GotoTable
> genGotoTable g sets = gotoTable
>   where
>	Grammar{ first_nonterm = fst_nonterm,
>		 first_term    = fst_term,
>		 non_terminals = non_terms } = g
>
>	-- goto array doesn't include %start symbols
>       gotoTable  = listArray (0,length sets-1)
>         [
>           (array (fst_nonterm, fst_term-1) [ 
>		(n, case lookup n goto of
>			Nothing -> NoGoto
>			Just s  -> Goto s)
>                             | n <- non_terms,
>			        n >= fst_nonterm, n < fst_term ])
>                 | (_set,goto) <- sets  ]








> genActionTable :: Grammar -> ([Name] -> NameSet) ->
>		 [([Lr1Item],[(Name,Int)])] -> ActionTable
> genActionTable g first sets = actionTable
>   where
>	Grammar { first_term = fst_term,
>		  terminals = terms,
>		  starts = starts',
>       	  priorities = prios } = g


>	n_starts = length starts'
>	isStartRule rule = rule < n_starts -- a bit hacky, but it'll do for now


>       term_lim = (head terms,last terms)
>       actionTable = array (0,length sets-1)
>             [ (set_no, accumArray res
>				 LR'Fail term_lim 
>				(possActions goto set))
>                   | ((set,goto),set_no) <- zip sets [0..] ]


>       possAction goto _set (Lr1 rule pos la) = 
>          case findRule g rule pos of
>               Just t | t >= fst_term || t == errorTok -> 
>			case lookup t goto of
>                       	Nothing -> []
>                               Just j  ->
>                                 case lookup t prios of
>                                       Nothing -> [ (t,LR'Shift j{-'-} No) ]
>                                       Just p  -> [ (t,LR'Shift j{-'-} p) ]
>               Nothing
>		   | isStartRule rule
>		   -> let (_,_,_,partial) = starts' !! rule in
>		      [ (startLookahead g partial, LR'Accept{-'-}) ]
>                  | otherwise   
>		   -> case lookupProdNo g rule of
>                          (_,_,_,p) -> zip (NameSet.toAscList la) (repeat (LR'Reduce rule p))
>               _ -> []


>	possActions goto coll = 
>		(concat [ possAction goto coll item |
>				item <- closure1 g first coll ])




























































>       res LR'Fail x = x
>       res x LR'Fail = x
>	res LR'MustFail _ = LR'MustFail
>	res _ LR'MustFail = LR'MustFail
>	res x x' | x == x' = x
>       res (LR'Accept) _ = LR'Accept
>       res _ (LR'Accept) = LR'Accept


>	res (LR'Multiple as x) (LR'Multiple bs x')
>        | x == x' = LR'Multiple (nub $ as ++ bs) x
>		-- merge dropped reductions for identical action


>	res (LR'Multiple as x) (LR'Multiple bs x')
>	       = case res x x' of 
>		   LR'Multiple cs a
>		     | a == x    -> LR'Multiple (nub $ x' : as ++ bs ++ cs) x
>		     | a == x'   -> LR'Multiple (nub $ x  : as ++ bs ++ cs) x'
>		     | otherwise -> error "failed invariant in resolve"
>		       		-- last means an unexpected change
>		   other -> other
>		-- merge dropped reductions for clashing actions, but only 
>		-- if they were S/R or R/R


>	res a@(LR'Multiple _ _) b = res a (LR'Multiple [] b)
>	res a b@(LR'Multiple _ _) = res (LR'Multiple [] a) b 
>	  -- leave cases above to do the appropriate merging


>       res a@(LR'Shift {}) b@(LR'Reduce {}) = res b a
>       res a@(LR'Reduce _ p) b@(LR'Shift _ p')
>		= case (p,p') of
>                      (No,_) -> LR'Multiple [a] b	-- shift wins
>                      (_,No) -> LR'Multiple [a] b	-- shift wins
>                      (Prio c i, Prio _ j)
>               		| i < j     -> b
>               		| i > j     -> a
>			        | otherwise ->
>				   case c of
>                                     LeftAssoc  -> a
>                                     RightAssoc -> b
>                                     None       -> LR'MustFail
>       res a@(LR'Reduce r p) b@(LR'Reduce r' p')
>		= case (p,p') of
>                      (No,_) -> LR'Multiple [a] b	-- give to earlier rule?
>                      (_,No) -> LR'Multiple [a] b
>                      (Prio _ i, Prio _ j)
>               		| i < j     -> b
>               		| j > i     -> a
>				| r < r'    -> LR'Multiple [b] a
>				| otherwise -> LR'Multiple [a] b
>       res _ _ = error "confict in resolve"








> countConflicts :: ActionTable -> (Array Int (Int,Int), (Int,Int))
> countConflicts action
>   = (conflictArray, foldr (\(a,b) (c,d) -> (a+c, b+d)) (0,0) conflictList)
>   
>   where
>	   
>	conflictArray = listArray (Array.bounds action) conflictList
>	conflictList  = map countConflictsState (assocs action)
>
>	countConflictsState (_state, actions)
>	  = foldr countMultiples (0,0) (elems actions)
>	  where
>	    countMultiples (LR'Multiple (_:_) (LR'Shift{})) (sr,rr) 
>	    	= (sr + 1, rr)
>	    countMultiples (LR'Multiple (_:_) (LR'Reduce{})) (sr,rr) 
>	    	= (sr, rr + 1)
>	    countMultiples (LR'Multiple _ _) _
>	    	= error "bad conflict representation"
>	    countMultiples _ c = c






> findRule :: Grammar -> Int -> Int -> Maybe Name
> findRule g rule dot = 
>	case lookupProdNo g rule of
>	   (_,lhs,_,_) -> case drop dot lhs of
>		            (a:_) -> Just a
>      			    _     -> Nothing
