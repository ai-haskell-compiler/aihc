{-# LANGUAGE OverloadedStrings #-}

module TestECMWithTestSequenceCommon (
    someEventsOnly,
    numberEventsOnly,
    pattern',
    pattern'',
    pattern''',
    pattern'''',
    testLookups,
    printOutEvents,
    printOutFailedPattern
) where

import qualified Caching.ExpiringCacheMap.Utils.TestSequence as TestSeq

import qualified Data.ByteString.Char8 as BS

printOutFailedPattern from_where filt_events' filt_events'' filt_events''' filt_events'''' = do
  putStrLn $ "Failed sequence test in " ++ from_where ++ ":"
  if not (pattern' (filt_events'))
    then putStrLn $ "Failed: pattern 1: " ++ (show filt_events')
    else return ()
  if not (pattern'' (filt_events''))
    then putStrLn $ "Failed: pattern 2: " ++ (show filt_events'')
    else return ()
  if not (pattern''' (filt_events'''))
    then putStrLn $ "Failed: pattern 3: " ++ (show filt_events''')
    else return ()
  if not (pattern'''' (filt_events''''))
    then putStrLn $ "Failed: pattern 4: " ++ (show filt_events'''')
    else return ()
  return ()


printOutEvents events' events'' events''' events'''' = do
  (putStrLn . show . filter someEventsOnly . reverse) events'
  (putStrLn . show . filter someEventsOnly . reverse) events''
  (putStrLn . show . filter someEventsOnly . reverse) events'''
  (putStrLn . show . filter someEventsOnly . reverse) events''''
  return ()

someEventsOnly a =
  case a of
    TestSeq.GetTime _    -> True
    TestSeq.ReadNumber _ -> True
    TestSeq.HaveNumber _ -> True
    _ -> False

numberEventsOnly a =
  case a of
    TestSeq.ReadNumber _ -> True
    TestSeq.HaveNumber _ -> True
    _ -> False

pattern' c =
  case c of
    [ TestSeq.ReadNumber numr1,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh1,
      TestSeq.HaveNumber numh1',
      TestSeq.ReadNumber numr2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh2 ]
      | numr1 == numh1 && numr1 == numh1' && numr2 == numh2 -> True
    _ -> False
    
pattern'' c =
  case c of
    [ TestSeq.ReadNumber numr1,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh1,
      TestSeq.GetTime _,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh1',
      TestSeq.ReadNumber numr2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh2 ]
      | numr1 == numh1 && numr1 == numh1' && numr2 == numh2 -> True
    _ -> False

pattern''' c =
  case c of
    [ TestSeq.ReadNumber numr1,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh1,
      TestSeq.HaveNumber numh1',
      TestSeq.ReadNumber numr2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh2 ]
      | numr1 == numh1 && numr1 == numh1' && numr2 == numh2 -> True
    _ -> False

pattern'''' c =
  case c of
    [ TestSeq.ReadNumber numr1,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh1,
      TestSeq.GetTime _,
      TestSeq.ReadNumber numr2,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh2,
      TestSeq.ReadNumber numr3,
      TestSeq.GetTime _,
      TestSeq.HaveNumber numh3 ]
      | numr1 == numh1 && numr2 == numh2 && numr3 == numh3 -> True
    _ -> False

testLookups lookup = do
  b <- lookup ("file1" :: BS.ByteString)
  TestSeq.haveNumber b
  b <- lookup "file1"
  TestSeq.haveNumber b
  b <- lookup "file2"
  TestSeq.haveNumber b
  return b
