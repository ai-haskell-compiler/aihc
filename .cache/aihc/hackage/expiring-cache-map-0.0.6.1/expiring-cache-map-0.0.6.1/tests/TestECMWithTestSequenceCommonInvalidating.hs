{-# LANGUAGE OverloadedStrings #-}

module TestECMWithTestSequenceCommonInvalidating (
    someEventsOnlyI,
    numberEventsOnlyI,
    pattern'I,
    pattern''I,
    pattern'''I,
    pattern''''I,
    pattern'''''I,
    pattern''''''I,
    testLookupsI,
    printOutEventsI,
    printOutFailedPatternI
) where

import qualified Caching.ExpiringCacheMap.Utils.TestSequence as TestSeq

import qualified Data.ByteString.Char8 as BS

printOutFailedPatternI from_where filt_events' filt_events'' filt_events''' filt_events'''' filt_events''''' filt_events'''''' = do
  putStrLn $ "Failed sequence test in " ++ from_where ++ ":"
  if not (pattern'I (filt_events'))
    then putStrLn $ "Failed: pattern 1: " ++ (show filt_events')
    else return ()
  if not (pattern''I (filt_events''))
    then putStrLn $ "Failed: pattern 2: " ++ (show filt_events'')
    else return ()
  if not (pattern'''I (filt_events'''))
    then putStrLn $ "Failed: pattern 3: " ++ (show filt_events''')
    else return ()
  if not (pattern''''I (filt_events''''))
    then putStrLn $ "Failed: pattern 4: " ++ (show filt_events'''')
    else return ()
  if not (pattern'''''I (filt_events'''''))
    then putStrLn $ "Failed: pattern 5: " ++ (show filt_events''''')
    else return ()
  if not (pattern''''''I (filt_events''''''))
    then putStrLn $ "Failed: pattern 6: " ++ (show filt_events'''''')
    else return ()
  return ()


printOutEventsI events' events'' events''' events'''' events''''' events'''''' = do
  (putStrLn . show . filter someEventsOnlyI . reverse) events'
  (putStrLn . show . filter someEventsOnlyI . reverse) events''
  (putStrLn . show . filter someEventsOnlyI . reverse) events'''
  (putStrLn . show . filter someEventsOnlyI . reverse) events''''
  (putStrLn . show . filter someEventsOnlyI . reverse) events'''''
  (putStrLn . show . filter someEventsOnlyI . reverse) events''''''
  return ()

someEventsOnlyI a =
  case a of
    TestSeq.GetTime _    -> True
    TestSeq.ReadNumber _ -> True
    TestSeq.HaveNumber _ -> True
    _ -> False

numberEventsOnlyI a =
  case a of
    TestSeq.ReadNumber _ -> True
    TestSeq.HaveNumber _ -> True
    _ -> False

pattern'I c = 
  case c of -- 
    [ TestSeq.ReadNumber numr_4,  -- ReadNumber 4,GetTime 7,HaveNumber 4,HaveNumber 4
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4,
      TestSeq.HaveNumber numh_4',
      TestSeq.ReadNumber numr_21,  -- ReadNumber 21,GetTime 24,HaveNumber 21,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_21,
      TestSeq.ReadNumber numr_32,  -- ReadNumber 32,GetTime 35,HaveNumber 32,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_32,
      TestSeq.HaveNumber numh_21', -- HaveNumber 21,
      TestSeq.ReadNumber numr_50,  -- ReadNumber 50,GetTime 53,HaveNumber 50,HaveNumber 50,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_50,
      TestSeq.HaveNumber numh_50',
      
      TestSeq.HaveNumber 2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 2,
      
      TestSeq.ReadNumber numr_76,  -- ReadNumber 76,GetTime 79,HaveNumber 76,HaveNumber 76,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_76,
      TestSeq.HaveNumber numh_76',
      
      TestSeq.HaveNumber numh_4'',
      
      TestSeq.HaveNumber numh_76'', -- HaveNumber 76,HaveNumber 32,HaveNumber 32,
      TestSeq.HaveNumber numh_32'',
      TestSeq.HaveNumber numh_32''',
      
      TestSeq.HaveNumber 0,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,
      
      TestSeq.ReadNumber numr_126,  -- ReadNumber 126,GetTime 129,HaveNumber 126,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_126,
      TestSeq.ReadNumber numr_137,  -- ReadNumber 137,GetTime 140,HaveNumber 137,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_137,
      TestSeq.ReadNumber numr_148,  -- ReadNumber 148,GetTime 151,HaveNumber 148,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_148,
      
      TestSeq.HaveNumber numh_126', -- HaveNumber 126,HaveNumber 137,HaveNumber 148,
      TestSeq.HaveNumber numh_137',
      TestSeq.HaveNumber numh_148',

      TestSeq.HaveNumber 3,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 3]
      |   numr_4 == numh_4 &&
          numh_4 == numh_4' &&
          numh_4 == numh_4'' &&
          numr_21 == numh_21 &&
          numh_21 == numh_21' &&
          numr_32 == numh_32 &&
          numh_32 == numh_32'' &&
          numh_32 == numh_32''' &&
          numr_50 == numh_50 &&
          numh_50 == numh_50' &&
          numr_76 == numh_76 &&
          numh_76 == numh_76' &&
          numh_76 == numh_76'' &&
          numr_126 == numh_126 &&
          numh_126 == numh_126' &&
          numr_137 == numh_137 &&
          numh_137 == numh_137' &&
          numr_148 == numh_148 &&
          numh_148 == numh_148' &&
          numh_4 < numh_21 &&
          numh_21 < numh_32 &&
          numh_32 < numh_50 &&
          numh_50 < numh_76 &&
          numh_76 < numh_126 &&
          numh_126 < numh_137 &&
          numh_137 < numh_148
            -> True
    _ -> False
    
pattern''I c = 
  case c of  -- 
    [ TestSeq.ReadNumber numr_4, -- ReadNumber 4,GetTime 7,HaveNumber 4,GetTime 15,GetTime 18,HaveNumber 4,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4,
      TestSeq.GetTime _,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4',
      TestSeq.ReadNumber numr_26, -- ReadNumber 26,GetTime 29,HaveNumber 26,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_26,
      TestSeq.ReadNumber numr_37, -- ReadNumber 37,GetTime 40,HaveNumber 37,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_37,
      TestSeq.HaveNumber numh_26', -- HaveNumber 26,
      TestSeq.ReadNumber numr_55, -- ReadNumber 55,GetTime 58,HaveNumber 55,HaveNumber 55,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_55,
      TestSeq.HaveNumber numh_55',
      
      TestSeq.HaveNumber 2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 2,
      
      TestSeq.ReadNumber numr_81,  -- ReadNumber 81,GetTime 84,HaveNumber 81,GetTime 92,GetTime 95,HaveNumber 81
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_81,
      TestSeq.GetTime _,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_81',
      
      TestSeq.GetTime _, -- GetTime 103,GetTime 106,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4'',
      
      TestSeq.GetTime _, -- GetTime 114,GetTime 117,HaveNumber 81,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_81'',
      TestSeq.GetTime _, -- GetTime 125,GetTime 128,HaveNumber 37,HaveNumber 37,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_37',
      TestSeq.HaveNumber numh_37'',
      
      TestSeq.HaveNumber 0,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,
      
      TestSeq.ReadNumber numr_151,  -- ReadNumber 151,GetTime 154,HaveNumber 151,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_151,
      TestSeq.ReadNumber numr_162,  -- ReadNumber 162,GetTime 165,HaveNumber 162,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_162,
      TestSeq.ReadNumber numr_173,  -- ReadNumber 173,GetTime 176,HaveNumber 173,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_173,
      
      TestSeq.GetTime _, -- GetTime 184,GetTime 187,HaveNumber 151,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_151',
      TestSeq.GetTime _, -- GetTime 195,GetTime 198,HaveNumber 162,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_162',
      TestSeq.GetTime _, -- GetTime 206,GetTime 209,HaveNumber 173,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_173',

      TestSeq.HaveNumber 3,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 3]
      |   numr_4 == numh_4 &&
          numh_4 == numh_4' &&
          numh_4 == numh_4'' &&
          numr_26 == numh_26 &&
          numh_26 == numh_26' &&
          numr_37 == numh_37 &&
          numh_37 == numh_37' &&
          numh_37 == numh_37'' &&
          numr_55 == numh_55 &&
          numh_55 == numh_55' &&
          numr_81 == numh_81 &&
          numh_81 == numh_81' &&
          numh_81 == numh_81'' &&
          numr_151 == numh_151 &&
          numh_151 == numh_151' &&
          numr_162 == numh_162 &&
          numh_162 == numh_162' &&
          numr_173 == numh_173 &&
          numh_173 == numh_173' &&
          numh_4 < numh_26 &&
          numh_26 < numh_37 &&
          numh_37 < numh_55 &&
          numh_55 < numh_81 &&
          numh_81 < numh_151 &&
          numh_151 < numh_162 &&
          numh_162 < numh_173
            -> True
    _ -> False

pattern'''I c =
  case c of -- 
    [ TestSeq.ReadNumber numr_4, -- ReadNumber 4,GetTime 7,HaveNumber 4,HaveNumber 4,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4,
      TestSeq.HaveNumber numh_4',
      TestSeq.ReadNumber numr_21, -- ReadNumber 21,GetTime 24,HaveNumber 21,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_21,
      TestSeq.ReadNumber numr_32, -- ReadNumber 32,GetTime 35,HaveNumber 32,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_32,
      TestSeq.HaveNumber numh_21', -- HaveNumber 21,
      TestSeq.ReadNumber numr_50, -- ReadNumber 50,GetTime 53,HaveNumber 50,HaveNumber 50,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_50,
      TestSeq.HaveNumber numh_50',
      
      TestSeq.HaveNumber 2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,
      
      TestSeq.ReadNumber numr_76,  -- ReadNumber 76,GetTime 79,HaveNumber 76,HaveNumber 76,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_76,
      TestSeq.HaveNumber numh_76',
      
      TestSeq.HaveNumber numh_4'',
      
      TestSeq.HaveNumber numh_76'', -- HaveNumber 76,HaveNumber 32,HaveNumber 32,
      TestSeq.HaveNumber numh_32'',
      TestSeq.HaveNumber numh_32''',
      
      TestSeq.HaveNumber 0,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,
      
      TestSeq.ReadNumber numr_126,  -- ReadNumber 126,GetTime 129,HaveNumber 126,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_126,
      TestSeq.ReadNumber numr_137,  -- ReadNumber 137,GetTime 140,HaveNumber 137,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_137,
      TestSeq.ReadNumber numr_148,  -- ReadNumber 148,GetTime 151,HaveNumber 148,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_148,
      
      TestSeq.HaveNumber numh_126', -- HaveNumber 126,HaveNumber 137,HaveNumber 148,
      TestSeq.HaveNumber numh_137',
      TestSeq.HaveNumber numh_148',

      TestSeq.HaveNumber 3,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0 ]
      |   numr_4 == numh_4 &&
          numh_4 == numh_4' &&
          numh_4 == numh_4'' &&
          numr_21 == numh_21 &&
          numh_21 == numh_21' &&
          numr_32 == numh_32 &&
          numh_32 == numh_32'' &&
          numh_32 == numh_32''' &&
          numr_50 == numh_50 &&
          numh_50 == numh_50' &&
          numr_76 == numh_76 &&
          numh_76' == numh_76'' &&
          numr_126 == numh_126 &&
          numh_126 == numh_126' &&
          numr_137 == numh_137 &&
          numh_137 == numh_137' &&
          numr_148 == numh_148 &&
          numh_148 == numh_148' &&
          numh_4 < numh_21 &&
          numh_21 < numh_32 &&
          numh_32 < numh_50 &&
          numh_50 < numh_76 &&
          numh_76 < numh_126 &&
          numh_126 < numh_137 &&
          numh_137 < numh_148
            -> True
    _ -> False

pattern''''I c =
  case c of -- 
    [ TestSeq.ReadNumber numr_4, -- ReadNumber 4,GetTime 7,HaveNumber 4,GetTime 15,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4,
      TestSeq.GetTime _,
      TestSeq.ReadNumber numr_18, -- ReadNumber 18,GetTime 21,HaveNumber 18,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_18,
      TestSeq.ReadNumber numr_29, -- ReadNumber 29,GetTime 32,HaveNumber 29,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_29,
      TestSeq.ReadNumber numr_40, -- ReadNumber 40,GetTime 43,HaveNumber 40,HaveNumber 29,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_40,
      TestSeq.HaveNumber numh_29',
      
      TestSeq.ReadNumber numr_58, -- ReadNumber 58,GetTime 61,HaveNumber 58,HaveNumber 58,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_58,
      TestSeq.HaveNumber numh_58',
      
      TestSeq.HaveNumber 2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,
      
      TestSeq.ReadNumber numr_84, -- ReadNumber 84,GetTime 87,HaveNumber 84,GetTime 95,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_84,
      TestSeq.GetTime _,
      TestSeq.ReadNumber numr_98, -- ReadNumber 98,GetTime 101,HaveNumber 98,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_98,
      TestSeq.ReadNumber numr_109, -- ReadNumber 109,GetTime 112,HaveNumber 109,GetTime 120,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_109,
      TestSeq.GetTime _,
      TestSeq.ReadNumber numr_123, -- ReadNumber 123,GetTime 126,HaveNumber 123,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_123,
      TestSeq.ReadNumber numr_134, -- ReadNumber 134,GetTime 137,HaveNumber 134,HaveNumber 134,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_134,
      TestSeq.HaveNumber numh_134',
      
      TestSeq.HaveNumber 0,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,
      
      TestSeq.ReadNumber numr_160, -- ReadNumber 160,GetTime 163,HaveNumber 160,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_160,
      TestSeq.ReadNumber numr_171, -- ReadNumber 171,GetTime 174,HaveNumber 171,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_171,
      TestSeq.ReadNumber numr_182, -- ReadNumber 182,GetTime 185,HaveNumber 182,GetTime 193,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_182,
      TestSeq.GetTime _,
      TestSeq.ReadNumber numr_196, -- ReadNumber 196,GetTime 199,HaveNumber 196,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_196,
      TestSeq.ReadNumber numr_207, -- ReadNumber 207,GetTime 210,HaveNumber 207,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_207,
      TestSeq.ReadNumber numr_218, -- ReadNumber 218,GetTime 221,HaveNumber 218,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_218,

      TestSeq.HaveNumber 3,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0 ]
      
      |   numr_4 == numh_4 &&
          numr_18 == numh_18 &&
          numr_29 == numh_29 &&
          numh_29 == numh_29' &&
          numr_40 == numh_40 &&
          numr_58 == numh_58 &&
          numh_58 == numh_58' &&
          numr_84 == numh_84 &&
          numr_98 == numh_98 &&
          numr_109 == numh_109 &&
          numr_123 == numh_123 &&
          numr_134 == numh_134 &&
          numh_134 == numh_134' &&
          numr_160 == numh_160 &&
          numr_171 == numh_171 &&
          numr_182 == numh_182 &&
          numr_196 == numh_196 &&
          numr_207 == numh_207 &&
          numr_218 == numh_218 &&
          numr_4 < numh_18 &&
          numh_18 < numh_29 &&
          numh_29 < numh_40 &&
          numh_40 < numh_58 &&
          numh_58 < numh_84 &&
          numh_84 < numh_98 &&
          numh_98 < numh_109 &&
          numh_109 < numh_123 &&
          numh_123 < numh_134 &&
          numh_134 < numh_160 &&
          numh_160 < numh_171 &&
          numh_171 < numh_182 &&
          numh_182 < numh_196 &&
          numh_196 < numh_207 &&
          numh_207 < numh_218
            -> True
    _ -> False

pattern'''''I c =
  case c of -- 
    [ TestSeq.ReadNumber numr_4, -- ReadNumber 4,GetTime 7,HaveNumber 4,HaveNumber 4,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4,
      TestSeq.HaveNumber numh_4',
      TestSeq.ReadNumber numr_21, -- ReadNumber 21,GetTime 24,HaveNumber 21,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_21,
      TestSeq.ReadNumber numr_32, -- ReadNumber 32,GetTime 35,HaveNumber 32,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_32,
      TestSeq.HaveNumber numh_21', -- HaveNumber 21,
      TestSeq.ReadNumber numr_50, -- ReadNumber 50,GetTime 53,HaveNumber 50,HaveNumber 50,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_50,
      TestSeq.HaveNumber numh_50',
      
      TestSeq.HaveNumber 2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 1,
      
      TestSeq.ReadNumber numr_76,  -- ReadNumber 76,GetTime 79,HaveNumber 76,HaveNumber 76,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_76,
      TestSeq.HaveNumber numh_76',
      
      TestSeq.HaveNumber numh_4'',
      
      TestSeq.HaveNumber numh_76'', -- HaveNumber 76,HaveNumber 32,HaveNumber 32,
      TestSeq.HaveNumber numh_32'',
      TestSeq.HaveNumber numh_32''',
      
      TestSeq.HaveNumber 0,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,
      
      TestSeq.ReadNumber numr_126, -- ReadNumber 126,GetTime 129,HaveNumber 126,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_126,
      TestSeq.ReadNumber numr_137, -- ReadNumber 137,GetTime 140,HaveNumber 137,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_137,
      TestSeq.ReadNumber numr_148, -- ReadNumber 148,GetTime 151,HaveNumber 148,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_148,
      
      TestSeq.HaveNumber numh_126', -- HaveNumber 126,HaveNumber 137,HaveNumber 148,
      TestSeq.HaveNumber numh_137',
      TestSeq.HaveNumber numh_148',

      TestSeq.HaveNumber 3,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 2 ]
      
      |   numr_4 == numh_4 &&
          numh_4 == numh_4' &&
          numh_4 == numh_4'' &&
          numr_21 == numh_21 &&
          numh_21 == numh_21' &&
          numr_32 == numh_32 &&
          numh_32 == numh_32'' &&
          numh_32 == numh_32''' &&
          numr_50 == numh_50 &&
          numh_50 == numh_50' &&
          numr_76 == numh_76 &&
          numh_76' == numh_76'' &&
          numr_126 == numh_126 &&
          numh_126 == numh_126' &&
          numr_137 == numh_137 &&
          numh_137 == numh_137' &&
          numr_148 == numh_148 &&
          numh_148 == numh_148' &&
          numh_4 < numh_21 &&
          numh_21 < numh_32 &&
          numh_32 < numh_50 &&
          numh_50 < numh_76 &&
          numh_76 < numh_126 &&
          numh_126 < numh_137 &&
          numh_137 < numh_148
          
            -> True
    _ -> False

pattern''''''I c =
  case c of -- 
    [ TestSeq.ReadNumber numr_4, -- ReadNumber 4,GetTime 7,HaveNumber 4,GetTime 15,GetTime 18,HaveNumber 4,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4,
      TestSeq.GetTime _,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_4',
      TestSeq.ReadNumber numr_26, -- ReadNumber 26,GetTime 29,HaveNumber 26,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_26,
      TestSeq.ReadNumber numr_37, -- ReadNumber 37,GetTime 40,HaveNumber 37,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_37,
      TestSeq.HaveNumber numh_26', -- HaveNumber 26,
      TestSeq.ReadNumber numr_55, -- ReadNumber 55,GetTime 58,HaveNumber 55,HaveNumber 55,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_55,
      TestSeq.HaveNumber numh_55',
      
      TestSeq.HaveNumber 2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 1,
      
      TestSeq.ReadNumber numr_81, -- ReadNumber 81,GetTime 84,HaveNumber 81,GetTime 92,GetTime 95,HaveNumber 81,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_81,
      TestSeq.GetTime _,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_81',
      
      TestSeq.ReadNumber numr_103, -- ReadNumber 103,GetTime 106,HaveNumber 103,GetTime 114,GetTime 117,HaveNumber 81,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_103,
      TestSeq.GetTime _,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_81'',
      
      TestSeq.ReadNumber numr_125, -- ReadNumber 125,GetTime 128,HaveNumber 125,HaveNumber 125,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_125,
      TestSeq.HaveNumber numh_125',
      
      TestSeq.HaveNumber 0,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 0,

      TestSeq.ReadNumber numr_151, -- ReadNumber 151,GetTime 154,HaveNumber 151,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_151,
      TestSeq.ReadNumber numr_162, -- ReadNumber 162,GetTime 165,HaveNumber 162,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_162,
      TestSeq.ReadNumber numr_173, -- ReadNumber 173,GetTime 176,HaveNumber 173,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_173,

      TestSeq.GetTime _, -- GetTime 184,GetTime 187,HaveNumber 151,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_151',
      TestSeq.GetTime _, -- GetTime 195,GetTime 198,HaveNumber 162,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_162',
      TestSeq.GetTime _, -- GetTime 206,GetTime 209,HaveNumber 173,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh_173',

      TestSeq.HaveNumber 2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber 1 ]
      |   numr_4 == numh_4 &&
          numh_4 == numh_4' &&
          numr_26 == numh_26 &&
          numh_26 == numh_26' &&
          numr_37 == numh_37 &&
          numr_55 == numh_55 &&
          numh_55 == numh_55' &&
          numr_81 == numh_81 &&
          numh_81 == numh_81' &&
          numh_81 == numh_81'' &&
          numr_103 == numh_103 &&
          numr_125 == numh_125 &&
          numh_125 == numh_125' &&
          numr_151 == numh_151 &&
          numh_151 == numh_151' &&
          numr_162 == numh_162 &&
          numh_162 == numh_162' &&
          numr_173 == numh_173 &&
          numh_173 == numh_173' &&
          numh_4 < numh_26 &&
          numh_26 < numh_37 &&
          numh_37 < numh_55 &&
          numh_55 < numh_81 &&
          numh_81 < numh_103 &&
          numh_103 < numh_125 &&
          numh_125 < numh_151 &&
          numh_151 < numh_162 &&
          numh_162 < numh_173
            -> True
    _ -> False

testLookupsI lookup invalidate invalidateCache keysCached keysNotExpired = do
  b <- lookup ("file1" :: BS.ByteString)
  TestSeq.haveNumber b
  b <- lookup "file1"
  TestSeq.haveNumber b
  b <- lookup "file2"
  TestSeq.haveNumber b
  b <- lookup "file3"
  TestSeq.haveNumber b
  mb <- invalidate "file2"
  case mb of
    Just b  -> TestSeq.haveNumber b
    Nothing -> TestSeq.haveNumber 0
  b <- lookup "file2"
  TestSeq.haveNumber b
  mb <- invalidate "file2"
  case mb of
    Just b  -> TestSeq.haveNumber b
    Nothing -> TestSeq.haveNumber 0
  l <- keysCached
  TestSeq.haveNumber (length l)
  l <- keysNotExpired
  TestSeq.haveNumber (length l)
  b <- lookup "file2"
  TestSeq.haveNumber b
  b <- lookup "file2"
  TestSeq.haveNumber b
  b <- lookup "file1"
  TestSeq.haveNumber b
  b <- lookup "file2"
  TestSeq.haveNumber b
  b <- lookup "file3"
  TestSeq.haveNumber b
  mk <- invalidateCache
  case mk of
    Just (_, b) -> TestSeq.haveNumber b
    Nothing     -> TestSeq.haveNumber 0
  l <- keysCached
  TestSeq.haveNumber (length l)
  l <- keysNotExpired
  TestSeq.haveNumber (length l)
  b <- lookup "file1"
  TestSeq.haveNumber b
  b <- lookup "file2"
  TestSeq.haveNumber b
  b <- lookup "file3"
  TestSeq.haveNumber b
  b <- lookup "file1"
  TestSeq.haveNumber b
  b <- lookup "file2"
  TestSeq.haveNumber b
  b <- lookup "file3"
  TestSeq.haveNumber b
  l <- keysCached
  TestSeq.haveNumber (length l)
  l <- keysNotExpired
  TestSeq.haveNumber (length l)
  
  return b
