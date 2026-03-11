module Text.Happy.HappyTemplate where
happyTemplate =
  "{-# LINE 1 \"templates\\GenericTemplate.hs\" #-}\n" ++ 
  "{-# LINE 1 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "{-# LINE 1 \"<built-in>\" #-}\n" ++ 
  "{-# LINE 1 \"<command line>\" #-}\n" ++ 
  "{-# LINE 1 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp \n" ++ 
  "\n" ++ 
  "{-# LINE 28 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "{-# LINE 49 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "\n" ++ 
  "{-# LINE 59 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "\n" ++ 
  "{-# LINE 68 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "\n" ++ 
  "infixr 9 `HappyStk`\n" ++ 
  "data HappyStk a = HappyStk a (HappyStk a)\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- starting the parse\n" ++ 
  "\n" ++ 
  "happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Accepting the parse\n" ++ 
  "\n" ++ 
  "-- If the current token is (1), it means we've just accepted a partial\n" ++ 
  "-- parse (a %partial parser).  We must ignore the saved token on the top of\n" ++ 
  "-- the stack in this case.\n" ++ 
  "happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =\n" ++ 
  "\thappyReturn1 ans\n" ++ 
  "happyAccept j tk st sts (HappyStk ans _) = \n" ++ 
  "\t (happyReturn1 ans)\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Arrays only: do the next action\n" ++ 
  "\n" ++ 
  "{-# LINE 155 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- HappyState data type (not arrays)\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "newtype HappyState b c = HappyState\n" ++ 
  "        (Int ->                    -- token number\n" ++ 
  "         Int ->                    -- token number (yes, again)\n" ++ 
  "         b ->                           -- token semantic value\n" ++ 
  "         HappyState b c ->              -- current state\n" ++ 
  "         [HappyState b c] ->            -- state stack\n" ++ 
  "         c)\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Shifting a token\n" ++ 
  "\n" ++ 
  "happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =\n" ++ 
  "     let i = (case x of { HappyErrorToken (i) -> i }) in\n" ++ 
  "--     trace \"shifting the error token\" $\n" ++ 
  "     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)\n" ++ 
  "\n" ++ 
  "happyShift new_state i tk st sts stk =\n" ++ 
  "     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)\n" ++ 
  "\n" ++ 
  "-- happyReduce is specialised for the common cases.\n" ++ 
  "\n" ++ 
  "happySpecReduce_0 i fn (1) tk st sts stk\n" ++ 
  "     = happyFail (1) tk st sts stk\n" ++ 
  "happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk\n" ++ 
  "     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)\n" ++ 
  "\n" ++ 
  "happySpecReduce_1 i fn (1) tk st sts stk\n" ++ 
  "     = happyFail (1) tk st sts stk\n" ++ 
  "happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')\n" ++ 
  "     = let r = fn v1 in\n" ++ 
  "       happySeq r (action nt j tk st sts (r `HappyStk` stk'))\n" ++ 
  "\n" ++ 
  "happySpecReduce_2 i fn (1) tk st sts stk\n" ++ 
  "     = happyFail (1) tk st sts stk\n" ++ 
  "happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')\n" ++ 
  "     = let r = fn v1 v2 in\n" ++ 
  "       happySeq r (action nt j tk st sts (r `HappyStk` stk'))\n" ++ 
  "\n" ++ 
  "happySpecReduce_3 i fn (1) tk st sts stk\n" ++ 
  "     = happyFail (1) tk st sts stk\n" ++ 
  "happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')\n" ++ 
  "     = let r = fn v1 v2 v3 in\n" ++ 
  "       happySeq r (action nt j tk st sts (r `HappyStk` stk'))\n" ++ 
  "\n" ++ 
  "happyReduce k i fn (1) tk st sts stk\n" ++ 
  "     = happyFail (1) tk st sts stk\n" ++ 
  "happyReduce k nt fn j tk st sts stk\n" ++ 
  "     = case happyDrop (k - ((1) :: Int)) sts of\n" ++ 
  "\t sts1@(((st1@(HappyState (action))):(_))) ->\n" ++ 
  "        \tlet r = fn stk in  -- it doesn't hurt to always seq here...\n" ++ 
  "       \t\thappyDoSeq r (action nt j tk st1 sts1 r)\n" ++ 
  "\n" ++ 
  "happyMonadReduce k nt fn (1) tk st sts stk\n" ++ 
  "     = happyFail (1) tk st sts stk\n" ++ 
  "happyMonadReduce k nt fn j tk st sts stk =\n" ++ 
  "        happyThen1 (fn stk tk) (\\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))\n" ++ 
  "       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))\n" ++ 
  "             drop_stk = happyDropStk k stk\n" ++ 
  "\n" ++ 
  "happyMonad2Reduce k nt fn (1) tk st sts stk\n" ++ 
  "     = happyFail (1) tk st sts stk\n" ++ 
  "happyMonad2Reduce k nt fn j tk st sts stk =\n" ++ 
  "       happyThen1 (fn stk tk) (\\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))\n" ++ 
  "       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))\n" ++ 
  "             drop_stk = happyDropStk k stk\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "             new_state = action\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "happyDrop (0) l = l\n" ++ 
  "happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t\n" ++ 
  "\n" ++ 
  "happyDropStk (0) l = l\n" ++ 
  "happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Moving to a new state after a reduction\n" ++ 
  "\n" ++ 
  "{-# LINE 253 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "happyGoto action j tk st = action j j tk (HappyState action)\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Error recovery ((1) is the error token)\n" ++ 
  "\n" ++ 
  "-- parse error if we are in recovery and we fail again\n" ++ 
  "happyFail  (1) tk old_st _ stk =\n" ++ 
  "--\ttrace \"failing\" $ \n" ++ 
  "    \thappyError_ tk\n" ++ 
  "\n" ++ 
  "{-  We don't need state discarding for our restricted implementation of\n" ++ 
  "    \"error\".  In fact, it can cause some bogus parses, so I've disabled it\n" ++ 
  "    for now --SDM\n" ++ 
  "\n" ++ 
  "-- discard a state\n" ++ 
  "happyFail  (1) tk old_st (((HappyState (action))):(sts)) \n" ++ 
  "\t\t\t\t\t\t(saved_tok `HappyStk` _ `HappyStk` stk) =\n" ++ 
  "--\ttrace (\"discarding state, depth \" ++ show (length stk))  $\n" ++ 
  "\taction (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))\n" ++ 
  "-}\n" ++ 
  "\n" ++ 
  "-- Enter error recovery: generate an error token,\n" ++ 
  "--                       save the old token and carry on.\n" ++ 
  "happyFail  i tk (HappyState (action)) sts stk =\n" ++ 
  "--      trace \"entering error recovery\" $\n" ++ 
  "\taction (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)\n" ++ 
  "\n" ++ 
  "-- Internal happy errors:\\n" ++ 
  "\n" ++ 
  "notHappyAtAll = error \"Internal Happy error\\n\"" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Hack to get the typechecker to accept our action functions\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Seq-ing.  If the --strict flag is given, then Happy emits \n" ++ 
  "--\thappySeq = happyDoSeq\n" ++ 
  "-- otherwise it emits\n" ++ 
  "-- \thappySeq = happyDontSeq\n" ++ 
  "\n" ++ 
  "happyDoSeq, happyDontSeq :: a -> b -> b\n" ++ 
  "happyDoSeq   a b = a `seq` b\n" ++ 
  "happyDontSeq a b = b\n" ++ 
  "\n" ++ 
  "-----------------------------------------------------------------------------\n" ++ 
  "-- Don't inline any functions from the template.  GHC has a nasty habit\n" ++ 
  "-- of deciding to inline happyGoto everywhere, which increases the size of\n" ++ 
  "-- the generated parser quite a bit.\n" ++ 
  "\n" ++ 
  "{-# LINE 317 \"templates\\\\GenericTemplate.hs\" #-}\n" ++ 
  "{-# NOINLINE happyShift #-}\n" ++ 
  "{-# NOINLINE happySpecReduce_0 #-}\n" ++ 
  "{-# NOINLINE happySpecReduce_1 #-}\n" ++ 
  "{-# NOINLINE happySpecReduce_2 #-}\n" ++ 
  "{-# NOINLINE happySpecReduce_3 #-}\n" ++ 
  "{-# NOINLINE happyReduce #-}\n" ++ 
  "{-# NOINLINE happyMonadReduce #-}\n" ++ 
  "{-# NOINLINE happyGoto #-}\n" ++ 
  "{-# NOINLINE happyFail #-}\n" ++ 
  "\n" ++ 
  "-- end of Happy Template."
