{-#Language QuasiQuotes#-}
module Text.Alex.AlexTemplate where
import AbsSyn
-- import Text.Alex.Verbatim


{-
alexTemplate GhcTarget = [verbatim|
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

{-# LINE 37 "templates\\GenericTemplate.hs" #-}

{-# LINE 47 "templates\\GenericTemplate.hs" #-}


data AlexAddr = AlexA# Addr#

-- Never happens
-- #if __GLASGOW_HASKELL__ < 503
-- uncheckedShiftL# = shiftL#
-- #endif

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr (AlexA# arr) off = |] ++
#ifdef WORDS_BIGENDIAN
  [verbatim|
    narrow16Int# i
    where
        i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
        high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
        low  = int2Word# (ord# (indexCharOffAddr# arr off'))
        off' = off *# 2#
  |]
#else
  [verbatim|
    indexInt16OffAddr# arr off
  |]
#endif 
  ++ [verbatim|





{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr (AlexA# arr) off =  |] ++
#ifdef WORDS_BIGENDIAN
  [verbatim|
    narrow32Int# i
    where
       !i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
                     (b2 `uncheckedShiftL#` 16#) `or#`
                     (b1 `uncheckedShiftL#` 8#) `or#` b0)
       !b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
       !b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
       !b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
       !b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
       !off' = off *# 4#
  |]
#else
  [verbatim|
    indexInt32OffAddr# arr off
  |]
#endif
  ++ [verbatim|




-- Never happens
-- #if __GLASGOW_HASKELL__ < 503
-- quickIndex arr i = arr ! i
-- #else
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
-- #endif




-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (I# (sc))
  = alexScanUser undefined input (I# (sc))

alexScanUser user input (I# (sc))
  = case alex_scan_tkn user input 0# input sc AlexNone of
        (AlexNone, input') ->
                case alexGetByte input of
                        Nothing -> 



                                   AlexEOF
                        Just _ ->



                                   AlexError input'

        (AlexLastSkip input'' len, _) ->



                AlexSkip input'' len

        (AlexLastAcc k input''' len, _) ->



                AlexToken input''' len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
        new_acc = (check_accs (alex_accept `quickIndex` (I# (s))))
  in
  new_acc `seq`
  case alexGetByte input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



        let
                (base) = alexIndexInt32OffAddr alex_base s
                ((I# (ord_c))) = fromIntegral c
                (offset) = (base +# ord_c)
                (check)  = alexIndexInt16OffAddr alex_check offset
                
                (new_s) = if (offset >=# 0#) && (check ==# ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of 
            -1# -> (new_acc, input)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len +# 1#) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc

  where
        check_accs [] = last_acc
        check_accs (AlexAcc a : _) = AlexLastAcc a input (I# (len))
        check_accs (AlexAccSkip : _)  = AlexLastSkip  input (I# (len))
        check_accs (AlexAccPred a predx : rest)
           | predx user orig_input (I# (len)) input
           = AlexLastAcc a input (I# (len))
        check_accs (AlexAccSkipPred predx : rest)
           | predx user orig_input (I# (len)) input
           = AlexLastSkip input (I# (len))
        check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

instance Functor AlexLastAcc where
    fmap f AlexNone = AlexNone
    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z
    fmap f (AlexLastSkip x y) = AlexLastSkip x y

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (I# (sc)) user _ _ input = 
     case alex_scan_tkn user input 0# input sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.

-- used by wrappers
iUnbox (I# (i)) = i
|]

-}






-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

alexTemplate _  = unlines[
  "alexIndexInt16OffAddr arr off = arr ! off",
  "alexIndexInt32OffAddr arr off = arr ! off",
  "quickIndex arr i = arr ! i",

  "-- -----------------------------------------------------------------------------",
  "-- Main lexing routines",
  "",
  "data AlexReturn a",
  "  = AlexEOF",
  "  | AlexError  !AlexInput",
  "  | AlexSkip   !AlexInput !Int",
  "  | AlexToken  !AlexInput !Int a",
  "",
  "-- alexScan :: AlexInput -> StartCode -> AlexReturn a",
  "alexScan input (sc)",
  "  = alexScanUser undefined input (sc)",
  "",
  "alexScanUser user input (sc)",
  "  = case alex_scan_tkn user input (0) input sc AlexNone of",
  "\t(AlexNone, input') ->",
  "\t\tcase alexGetByte input of",
  "\t\t\tNothing -> ",
  "",
  "",
  "",
  "\t\t\t\t   AlexEOF",
  "\t\t\tJust _ ->",
  "",
  "",
  "",
  "\t\t\t\t   AlexError input'",
  "",
  "\t(AlexLastSkip input'' len, _) ->",
  "",
  "",
  "",
  "\t\tAlexSkip input'' len",
  "",
  "\t(AlexLastAcc k input''' len, _) ->",
  "",
  "",
  "",
  "\t\tAlexToken input''' len k",
  "",
  "",
  "-- Push the input through the DFA, remembering the most recent accepting",
  "-- state it encountered.",
  "",
  "alex_scan_tkn user orig_input len input s last_acc =",
  "  input `seq` -- strict in the input",
  "  let ",
  "\tnew_acc = (check_accs (alex_accept `quickIndex` (s)))",
  "  in",
  "  new_acc `seq`",
  "  case alexGetByte input of",
  "     Nothing -> (new_acc, input)",
  "     Just (c, new_input) -> ",
  "",
  "",
  "",
  "\tlet",
  "\t\t(base) = alexIndexInt32OffAddr alex_base s",
  "\t\t((ord_c)) = fromIntegral c",
  "\t\t(offset) = (base + ord_c)",
  "\t\t(check)  = alexIndexInt16OffAddr alex_check offset",
  "\t\t",
  "\t\t(new_s) = if (offset >= (0)) && (check == ord_c)",
  "\t\t\t  then alexIndexInt16OffAddr alex_table offset",
  "\t\t\t  else alexIndexInt16OffAddr alex_deflt s",
  "\tin",
  "\tcase new_s of ",
  "\t    (-1) -> (new_acc, input)",
  "\t\t-- on an error, we want to keep the input *before* the",
  "\t\t-- character that failed, not after.",
  "    \t    _ -> alex_scan_tkn user orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)",
  "                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)",
  "\t\t\tnew_input new_s new_acc",
  "",
  "  where",
  "\tcheck_accs [] = last_acc",
  "\tcheck_accs (AlexAcc a : _) = AlexLastAcc a input (len)",
  "\tcheck_accs (AlexAccSkip : _)  = AlexLastSkip  input (len)",
  "\tcheck_accs (AlexAccPred a predx : rest)",
  "\t   | predx user orig_input (len) input",
  "\t   = AlexLastAcc a input (len)",
  "\tcheck_accs (AlexAccSkipPred predx : rest)",
  "\t   | predx user orig_input (len) input",
  "\t   = AlexLastSkip input (len)",
  "\tcheck_accs (_ : rest) = check_accs rest",
  "",
  "data AlexLastAcc a",
  "  = AlexNone",
  "  | AlexLastAcc a !AlexInput !Int",
  "  | AlexLastSkip  !AlexInput !Int",
  "",
  "instance Functor AlexLastAcc where",
  "    fmap f AlexNone = AlexNone",
  "    fmap f (AlexLastAcc x y z) = AlexLastAcc (f x) y z",
  "    fmap f (AlexLastSkip x y) = AlexLastSkip x y",
  "",
  "data AlexAcc a user",
  "  = AlexAcc a",
  "  | AlexAccSkip",
  "  | AlexAccPred a (AlexAccPred user)",
  "  | AlexAccSkipPred (AlexAccPred user)",
  "",
  "type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool",
  "",
  "-- -----------------------------------------------------------------------------",
  "-- Predicates on a rule",
  "",
  "alexAndPred p1 p2 user in1 len in2",
  "  = p1 user in1 len in2 && p2 user in1 len in2",
  "",
  "--alexPrevCharIsPred :: Char -> AlexAccPred _ ",
  "alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input",
  "",
  "alexPrevCharMatches f _ input _ _ = f (alexInputPrevChar input)",
  "",
  "--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ ",
  "alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input",
  "",
  "--alexRightContext :: Int -> AlexAccPred _",
  "alexRightContext (sc) user _ _ input = ",
  "     case alex_scan_tkn user input (0) input sc AlexNone of",
  "\t  (AlexNone, _) -> False",
  "\t  _ -> True",
  "\t-- TODO: there's no need to find the longest",
  "\t-- match when checking the right context, just",
  "\t-- the first match will do.",
  "",
  "-- used by wrappers",
  "iUnbox (i) = i"
  ]
