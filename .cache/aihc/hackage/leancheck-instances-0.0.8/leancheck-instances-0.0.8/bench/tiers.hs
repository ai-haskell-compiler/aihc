-- tiers.hs -- prints tiers of values up to a certain point
--
-- Copyright (c) 2015-2022 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test.LeanCheck
import Test.LeanCheck.Instances
import Test.LeanCheck.Function
import Test.LeanCheck.Function.Eq
import Test.LeanCheck.Tiers (showTiers, finite)
import System.Environment
import qualified Data.ByteString as BS
import Data.List (intercalate, nub)
import Data.Array
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Text (Text)
import Data.Time
import Numeric.Natural

dropEmptyTiersTail :: [[a]] -> [[a]]
dropEmptyTiersTail ([]:[]:[]: []:[]:[]: _) = []
dropEmptyTiersTail (xs:xss) = xs:dropEmptyTiersTail xss
dropEmptyTiersTail []     = []

lengthT :: [[a]] -> Maybe Int
lengthT xss | finite xss' = Just . length $ concat xss'
            | otherwise   = Nothing
  where xss' = dropEmptyTiersTail xss

allUnique :: Eq a => [a] -> Bool
allUnique [] = True
allUnique (x:xs) = x `notElem` xs
                && allUnique (filter (/= x) xs)

countRepetitions :: Eq a => [a] -> Int
countRepetitions xs = length xs - length (nub xs)

ratioRepetitions :: Eq a => [a] -> Rational
ratioRepetitions [] = 0
ratioRepetitions xs = fromIntegral (countRepetitions xs) % fromIntegral (length xs)

showLengthT :: [[a]] -> String
showLengthT xss = case lengthT xss of
                    Nothing -> "Infinity"
                    Just x  -> show x

showDotsLongerThan :: Show a => Int -> [a] -> String
showDotsLongerThan n xs = "["
                       ++ intercalate "," (dotsLongerThan n $ map show xs)
                       ++ "]"
  where
  dotsLongerThan n xs = take n xs ++ ["..." | not . null $ drop n xs]

printTiers :: Show a => Int -> [[a]] -> IO ()
printTiers n = putStrLn . init . unlines . map ("  " ++) . lines . showTiers n

put :: (Show a, Eq a, Listable a) => String -> Int -> a -> IO ()
put t n a = do
  putStrLn $ "map length (tiers :: [[ " ++ t ++ " ]])  =  "
          ++ showDotsLongerThan n (map length $ tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "length (list :: [ " ++ t ++ " ])  =  "
          ++ showLengthT (tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "allUnique (list :: [ " ++ t ++ " ])  =  "
          ++ show (allUnique . concat . take n $ tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "ratioRepetitions (list :: [ " ++ t ++ " ])  =  "
          ++ show (ratioRepetitions . concat . take n $ tiers `asTypeOf` [[a]])
  putStrLn $ ""
  putStrLn $ "tiers :: [" ++ t ++ "]  ="
  printTiers n $ tiers `asTypeOf` [[a]]

u :: a
u = undefined

main :: IO ()
main = do
  as <- getArgs
  let (t,n) = case as of
                []    -> ("Int", 12)
                [t]   -> (t,     12)
                [t,n] -> (t, read n)
  case t of
    -- standard types
    "()"               -> put t n (u :: ()                   )
    "Int"              -> put t n (u :: Int                  )
    "Integer"          -> put t n (u :: Integer              )
    "Bool"             -> put t n (u :: Bool                 )
    "Char"             -> put t n (u :: Char                 )
    "Float"            -> put t n (u :: Float                )
    "Double"           -> put t n (u :: Double               )
    "Rational"         -> put t n (u :: Rational             )
    "[()]"             -> put t n (u :: [()]                 )
    "[Int]"            -> put t n (u :: [Int]                )
    "Array Int Int"    -> put t n (u :: Array Int Int        )
    -- other
    "Text"             -> put t n (u :: Text                 )
    "Natural"          -> put t n (u :: Natural              )
    "ByteString"       -> put t n (u :: BS.ByteString        )
    -- containers
    "Set Int"          -> put t n (u :: Set Int              )
    "Set Bool"         -> put t n (u :: Set Bool             )
    "Map Int Int"      -> put t n (u :: Map Int Int          )
    "Map Bool Bool"    -> put t n (u :: Map Bool Bool        )
    "Seq Int"          -> put t n (u :: Seq Int              )
    "Seq Bool"         -> put t n (u :: Seq Bool             )
    "IntMap Int"       -> put t n (u :: IntMap Int           )
    "IntSet"           -> put t n (u :: IntSet               )
    -- time
    "Day"              -> put t n (u :: Day                  )
    "DiffTime"         -> put t n (u :: DiffTime             )
    "UTCTime"          -> put t n (u :: UTCTime              )
    -- unhandled
    _                  -> putStrLn $ "unknown/unhandled type `" ++ t ++ "'"
