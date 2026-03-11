{-# LANGUAGE ScopedTypeVariables #-}

module Test.Expectations
    ( module Test.Expectations
    , module EX
    ) where

import Test.Hspec.Expectations as EX hiding (shouldBe, shouldContain,
                                      shouldNotBe, shouldReturn, shouldSatisfy,
                                      shouldThrow)

import qualified Test.Concurrent as T
import qualified Test.Util       as T

import qualified Test.HUnit as HU

import qualified Control.Concurrent.Async as AS
import qualified Control.Exception        as E
import qualified Control.Monad            as M

import qualified GHC.Conc as CC

import qualified Data.List     as L
import qualified Data.Maybe    as MB
import           Data.Monoid   ((<>))
import qualified Data.Typeable as TP

import qualified System.Mem      as Mem
import qualified System.Mem.Weak as Weak
import qualified System.Timeout  as ST

import Prelude hiding (and, fail, or)

assertFailure :: String -> IO x
assertFailure desc = do
    M.void $ HU.assertFailure desc
    error "dummy"

failWithException :: (String -> String) -> IO x -> IO y
failWithException descf wait = proc `E.catch` returnError
  where
    returnError (E.SomeException err) = do
        assertFailure . descf $ "but aborted with: " <> show err
    proc = do
        M.void wait
        assertFailure $ descf "died with no exception (test maybe wrong)"

--------------------
-- assertions

assertTrue :: Bool -> String -> IO ()
assertTrue True  _    = return ()
assertTrue False desc = HU.assertFailure desc

assertEqual :: (Show x, Eq x) => x -> x -> IO ()
assertEqual expected actual = assertTrue (expected == actual) desc
  where
    desc = mkDesc 80 "expected" expectedStr <> "\n" <>
           mkDesc 80 "actual  " actualStr
    expectedStr = show expected
    actualStr   = show actual

assertNotEqual :: (Show x, Eq x) => x -> x -> IO ()
assertNotEqual expected actual = assertTrue (expected /= actual) desc
  where
    desc = mkDesc 80 "expected not to be" expectedStr <> "\n" <>
           mkDesc 80 "actual" actualStr
    expectedStr = show expected
    actualStr   = show actual

shouldBe :: (Show x, Eq x) => x -> x -> IO ()
shouldBe = flip assertEqual

shouldNotBe :: (Show x, Eq x) => x -> x -> IO ()
shouldNotBe = flip assertNotEqual

shouldSatisfy :: Show x => x -> (x -> Bool) -> IO ()
shouldSatisfy x f = assertTrue (f x) desc
  where
    desc = "assertion failed on: " ++ show x

shouldContain :: (Show x, Eq x) => [x] -> [x] -> IO ()
shouldContain actual expected = assertTrue result $ desc
  where
    result = expected `L.isInfixOf` actual
    desc = mkDesc 80 "expected to contain" expectedStr <> "\n" <>
           mkDesc 80 "but actual" actualStr
    expectedStr = show expected
    actualStr   = show actual

shouldReturn :: (Show x, Eq x) => IO x -> x -> IO ()
shouldReturn act expected = do
    ex <- E.try act
    case ex of
        Right x                    -> x `shouldBe` expected
        Left (E.SomeException err) -> HU.assertFailure $ desc err
  where
    desc err = mkDesc 80 "expected" expectedStr <> "\n" <>
               mkDesc 80 "but exception throwed" (show err)
    expectedStr = show expected

shouldThrow :: (Show x, E.Exception e) => IO x -> Selector e -> IO ()
shouldThrow act selector = do
    ex <- E.try act
    case ex of
        Right x                       -> HU.assertFailure $ descX x
        Left (err :: E.SomeException) -> case E.fromException err of
            Just e | selector e -> return ()
                   | otherwise  -> HU.assertFailure $ descF err
            Nothing             -> HU.assertFailure $ descE err
  where
    descX x = expectedStr <> mkDesc 80 "but returned" (show x)
    descF e = expectedStr <> mkDesc 80 "but selector failed for" (show e)
    descE e = expectedStr <> mkDesc 80 "but exception throwed" (show e)
    expectedStr = mkDesc 80 "expected to throw" exceptedType <> "\n"
    exceptedType = (show . TP.typeOf . instanceOf) selector
    instanceOf :: Selector a -> a
    instanceOf _ = error "dummy data of shouldThrow"

-- gc expectations

shouldBeGarbageCollected :: Weak.Weak x -> IO ()
shouldBeGarbageCollected weak = do
    Mem.performGC
    mref <- Weak.deRefWeak weak
    case mref of
        Just _  -> assertFailure "expected to be garbage collected but does not"
        Nothing -> return ()

shouldNotBeGarbageCollected :: Weak.Weak x -> IO ()
shouldNotBeGarbageCollected weak = do
    Mem.performGC
    mv <- Weak.deRefWeak weak
    case mv of
        Just _  -> return ()
        Nothing -> assertFailure "expected not to be garbage collected but garbage collected"

-- concurrent expectations

shouldBlock :: IO x -> Int -> IO (AS.Async x)
shouldBlock act time = do
    async <- AS.async act
    mstatus <- ST.timeout time $ T.waitStop async
    case mstatus of
        Just CC.ThreadFinished    -> HU.assertFailure $ desc "but finished"
        Just CC.ThreadDied        -> failWithException desc $ AS.wait async
        Just (CC.ThreadBlocked _) -> return ()
        _                         -> HU.assertFailure $ desc "but still running"
    return async
  where
    desc str = "expected to block in " <> show time <> " nanosec\n" <> str

shouldStillBlock :: AS.Async x -> Int -> IO ()
shouldStillBlock async time = M.void $ AS.wait async `shouldBlock` time

shouldAwakeFinish :: AS.Async x -> Int -> IO x
shouldAwakeFinish async time = do
    mstatus <- ST.timeout time $ T.waitFinish async
    status <- MB.fromMaybe (T.threadStatus async) $ return <$> mstatus
    let wait = AS.wait async
    case status of
        CC.ThreadFinished       -> return ()
        CC.ThreadDied           -> failWithException desc $ AS.wait async
        CC.ThreadBlocked reason ->
            HU.assertFailure . desc $ "but still blocked with " <> show reason
        CC.ThreadRunning        ->
            HU.assertFailure . desc $ "but still running"
    wait
  where
    desc str = "expected to awake and finish in " <>
               show time <> " nanosec\n" <> str

onlyOneShouldAwakeFinish :: [AS.Async x] -> Int -> IO (x, [AS.Async x])
onlyOneShouldAwakeFinish asyncs0 time = do
    mret <- ST.timeout time $ T.waitFinishAny asyncs0
    (idx, status) <- case mret of
        Just [ret] -> return ret
        Just _     -> assertFailure . desc $ "but not only one finished"
        Nothing    -> assertFailure . desc $ "but still running"
    let (async, asyncs1) = T.pickUp idx asyncs0
        wait = AS.wait async
    x <- case status of
        CC.ThreadFinished -> wait
        CC.ThreadDied     -> failWithException desc wait
        _                 -> assertFailure . desc $ "unknown case"
    return (x, asyncs1)
  where
    desc str = "expected to awake only one and finish in " <>
               show time <> " nanosec\n" <> str

shouldNotBlock :: IO x -> Int -> IO x
shouldNotBlock act time = do
    async <- AS.async act
    mstatus <- ST.timeout time $ T.waitStop async
    let wait = AS.wait async
    case mstatus of
        Just CC.ThreadFinished         -> return ()
        Just CC.ThreadDied             -> failWithException desc wait
        Just (CC.ThreadBlocked reason) -> do
            HU.assertFailure . desc $ "but blocked with " <> show reason
        _ -> HU.assertFailure $ desc "but still running"
    wait
  where
    desc str = "expected not to block and finish in " <>
               show time <> " nanosec\n" <> str

shouldFinish :: IO x -> Int -> IO x
shouldFinish act time = do
    async <- AS.async act
    mstatus <- ST.timeout time $ T.waitFinish async
    let wait = AS.wait async
    case mstatus of
        Just CC.ThreadFinished         -> return ()
        Just CC.ThreadDied             -> failWithException desc wait
        Just (CC.ThreadBlocked reason) -> do
            HU.assertFailure . desc $ "but blocked with " <> show reason
        _ -> HU.assertFailure $ desc "but still running"
    wait
  where
    desc str = "expected to finish in " <> show time <>
               " nanosec\n" <> str

---------------------------
--- util

mkDesc :: Int -> String -> String -> String
mkDesc len s1 s2
    | length s1 + length str > len = s1 <> ":\n\t" <> str
    | otherwise                    = s1 <> ": "    <> str
  where
    str = truncateString 512 s2

truncateString :: Int -> String -> String
truncateString len str
    | null rest = res
    | otherwise = res <> "..."
  where
    (res, rest) = L.splitAt len str

and :: [a -> Bool] -> (a -> Bool)
and (c:cs) a | c a       = and cs a
             | otherwise = False
and []     _             = True

