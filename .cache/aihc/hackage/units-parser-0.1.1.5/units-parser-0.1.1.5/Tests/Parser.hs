{- Test the unit parser
   Copyright (c) 2014 Richard Eisenberg
-}

{-# LANGUAGE TemplateHaskell, TypeOperators, CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

import Prelude hiding ( lex, exp )

import Text.Parse.Units

import Control.Monad (liftM)
import Control.Monad.Reader (runReader)
import qualified Data.Map.Strict as Map
import Text.Parsec
import Data.Generics
import Language.Haskell.TH

import Test.Tasty
import Test.Tasty.HUnit

leftOnly :: Either a b -> Maybe a
leftOnly (Left a) = Just a
leftOnly (Right _) = Nothing

----------------------------------------------------------------------
-- TH functions
----------------------------------------------------------------------

stripModules :: Data a => a -> a
stripModules = everywhere (mkT (mkName . nameBase))

pprintUnqualified :: (Ppr a, Data a) => a -> String
pprintUnqualified = pprint . stripModules

----------------------------------------------------------------------
-- Lexer
----------------------------------------------------------------------

lexTest :: String -> String
lexTest s =
  case lex s of
    Left _     -> "error"
    Right toks -> show toks

lexTestCases :: [(String, String)]
lexTestCases = [ ( "m", "[m]" )
               , ( "", "[]" )
               , ( "m s", "[m,s]" )
               , ( "   m   s ", "[m,s]" )
               , ( "m   ", "[m]" )
               , ( "   m", "[m]" )
               , ( "( m  /s", "[(,m,/,s]" )
               , ( "!", "error" )
               , ( "1 2 3", "[1,2,3]" )
               , ( "  ", "[]" )
               ]

lexTests :: TestTree
lexTests = testGroup "Lexer" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ lexTest str @?= out) lexTestCases

----------------------------------------------------------------------
-- Unit strings
----------------------------------------------------------------------

unitStringTestCases :: [(String, String)]
unitStringTestCases = [ ("m", "Meter")
                      , ("s", "Second")
                      , ("min", "Minute")
                      , ("km", "Kilo :@ Meter")
                      , ("mm", "Milli :@ Meter")
                      , ("kmin", "Kilo :@ Minute")
                      , ("dam", "error")   -- ambiguous!
                      , ("damin", "Deca :@ Minute")
                      , ("ms", "Milli :@ Second")
                      , ("mmin", "Milli :@ Minute")
                      , ("mmm", "error")
                      , ("mmmin", "error")
                      , ("sm", "error")
                      , ("", "error")
                      , ("dak", "error")
                      , ("das", "Deca :@ Second")
                      , ("ds", "Deci :@ Second")
                      , ("daam", "Deca :@ Ampere")
                      , ("kam", "Kilo :@ Ampere")
                      , ("dm", "Deci :@ Meter")
                      ]

parseUnitStringTest :: String -> String
parseUnitStringTest s =
  case flip runReader testSymbolTable $ runParserT unitStringParser () "" s of
    Left _ -> "error"
    Right exp -> show exp

unitStringTests :: TestTree
unitStringTests = testGroup "UnitStrings" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitStringTest str @?= out)
    unitStringTestCases

----------------------------------------------------------------------
-- Symbol tables
----------------------------------------------------------------------

mkSymbolTableTests :: TestTree
mkSymbolTableTests = testGroup "mkSymbolTable"
  [ testCase "Unambiguous1" (Map.keys (prefixTable testSymbolTable) @?= ["d","da","k","m"])
  -- , testCase "Unambiguous2" (Map.keys (unitTable testSymbolTable) @?= ["am","m","min","s"])
  , testCase "AmbigPrefix" (leftOnly (mkSymbolTable [("a",''Milli),("a",''Centi)] ([] :: [(String,Name)])) @?= Just "The label `a' is assigned to the following meanings:\n[\"Main.Milli\",\"Main.Centi\"]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "AmbigUnit" (leftOnly (mkSymbolTable ([] :: [(String,Name)]) [("m",''Meter),("m",''Minute)]) @?= Just "The label `m' is assigned to the following meanings:\n[\"Main.Meter\",\"Main.Minute\"]\nThis is ambiguous. Please fix before building a unit parser.")
  , testCase "MultiAmbig" (leftOnly (mkSymbolTable [("a",''Milli),("b",''Centi),("b",''Deci),("b",''Kilo),("c",''Atto),("c",''Deca)] [("m",''Meter),("m",''Minute),("s",''Second)]) @?= Just "The label `b' is assigned to the following meanings:\n[\"Main.Centi\",\"Main.Deci\",\"Main.Kilo\"]\nThe label `c' is assigned to the following meanings:\n[\"Main.Atto\",\"Main.Deca\"]\nThis is ambiguous. Please fix before building a unit parser.")
                                                                                                ]

testSymbolTable :: SymbolTable Name Name
Right testSymbolTable =
   mkSymbolTable (stripModules [ ("k", ''Kilo)
                               , ("da", ''Deca)
                               , ("m", ''Milli)
                               , ("d", ''Deci) ])
                 (stripModules [ ("m", ''Meter)
                               , ("s", ''Second)
                               , ("min", ''Minute)
                               , ("am", ''Ampere) ])

data Kilo   = Kilo
data Deca   = Deca
data Centi  = Centi
data Milli  = Milli
data Deci   = Deci
data Atto   = Atto

data Meter  = Meter
data Second = Second
data Minute = Minute
data Ampere = Ampere

----------------------------------------------------------------------
-- TH conversions, taken from the `units` package
----------------------------------------------------------------------

-- This is silly, but better than rewriting the tests.
-- Note that we can't depend on `units` package, because we want
-- `units` to depend on `units-parser`. Urgh.
data Number = Number
data a :@ b = a :@ b
data a :* b = a :* b
data a :/ b = a :/ b
data a :^ b = a :^ b

data Succ a
data Z = Zero

sPred, sSucc, sZero :: ()
sPred = ()
sSucc = ()
sZero = ()

parseUnitExp :: SymbolTable Name Name -> String -> Either String Exp
parseUnitExp tbl s = to_exp `liftM` parseUnit tbl s   -- the Either monad
  where
    to_exp Unity                  = ConE 'Number
    to_exp (Unit (Just pre) unit) = ConE '(:@) `AppE` of_type pre `AppE` of_type unit
    to_exp (Unit Nothing unit)    = of_type unit
    to_exp (Mult e1 e2)           = ConE '(:*) `AppE` to_exp e1 `AppE` to_exp e2
    to_exp (Div e1 e2)            = ConE '(:/) `AppE` to_exp e1 `AppE` to_exp e2
    to_exp (Pow e i)              = ConE '(:^) `AppE` to_exp e `AppE` mk_sing i

    of_type :: Name -> Exp
    of_type n = (VarE 'undefined) `SigE` (ConT n)

    mk_sing :: Integer -> Exp
    mk_sing n
      | n < 0     = VarE 'sPred `AppE` mk_sing (n + 1)
      | n > 0     = VarE 'sSucc `AppE` mk_sing (n - 1)
      | otherwise = VarE 'sZero

parseUnitType :: SymbolTable Name Name -> String -> Either String Type
parseUnitType tbl s = to_type `liftM` parseUnit tbl s   -- the Either monad
  where
    to_type Unity                  = ConT ''Number
    to_type (Unit (Just pre) unit) = ConT ''(:@) `AppT` ConT pre `AppT` ConT unit
    to_type (Unit Nothing unit)    = ConT unit
    to_type (Mult e1 e2)           = ConT ''(:*) `AppT` to_type e1 `AppT` to_type e2
    to_type (Div e1 e2)            = ConT ''(:/) `AppT` to_type e1 `AppT` to_type e2
    to_type (Pow e i)              = ConT ''(:^) `AppT` to_type e `AppT` mk_z i

    mk_z :: Integer -> Type
    mk_z n
      | n < 0     = ConT ''Pred `AppT` mk_z (n + 1)
      | n > 0     = ConT ''Succ `AppT` mk_z (n - 1)
      | otherwise = ConT 'Zero   -- single quote as it's a data constructor!

----------------------------------------------------------------------
-- Overall parser
----------------------------------------------------------------------

parseUnitTest :: String -> String
parseUnitTest s =
  case parseUnitExp testSymbolTable s of
    Left _    -> "error"
    Right exp -> pprintUnqualified exp

parseTestCases :: [(String, String)]
parseTestCases =
  [ ("m", "undefined :: Meter")
  , ("s", "undefined :: Second")
  , ("ms", "(:@) (undefined :: Milli) (undefined :: Second)")
  , ("mm", "(:@) (undefined :: Milli) (undefined :: Meter)")
  , ("mmm", "error")
  , ("km", "(:@) (undefined :: Kilo) (undefined :: Meter)")
  , ("m s", "(:*) (undefined :: Meter) (undefined :: Second)")
  , ("m/s", "(:/) (undefined :: Meter) (undefined :: Second)")
  , ("m/s^2", "(:/) (undefined :: Meter) ((:^) (undefined :: Second) (sSucc (sSucc sZero)))")
  , ("s/m m", "(:/) (undefined :: Second) ((:*) (undefined :: Meter) (undefined :: Meter))")
  , ("s s/m m", "(:/) ((:*) (undefined :: Second) (undefined :: Second)) ((:*) (undefined :: Meter) (undefined :: Meter))")
  , ("s*s/m*m", "(:*) ((:/) ((:*) (undefined :: Second) (undefined :: Second)) (undefined :: Meter)) (undefined :: Meter)")
  , ("s*s/(m*m)", "(:/) ((:*) (undefined :: Second) (undefined :: Second)) ((:*) (undefined :: Meter) (undefined :: Meter))")
  , ("m^-1", "(:^) (undefined :: Meter) (sPred sZero)")
  , ("m^(-1)", "(:^) (undefined :: Meter) (sPred sZero)")
  , ("m^(-(1))", "(:^) (undefined :: Meter) (sPred sZero)")
  , ("1", "Number")
  , ("1/s", "(:/) Number (undefined :: Second)")
  , ("m 1 m", "(:*) ((:*) (undefined :: Meter) Number) (undefined :: Meter)")
  , ("  ", "Number")
  , ("", "Number")
  ]

parseUnitTests :: TestTree
parseUnitTests = testGroup "ParseUnit" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitTest str @?= out)
    parseTestCases

parseUnitTestT :: String -> String
parseUnitTestT s =
  case parseUnitType testSymbolTable s of
    Left _    -> "error"
    Right exp -> pprintUnqualified exp

op :: String -> String
#if __GLASGOW_HASKELL__ > 802
op s = "(" ++ s ++ ")"
#else
op = id
#endif

opm, opd, ope, opa :: String
opm = op ":*"
opd = op ":/"
ope = op ":^"
opa = op ":@"

parseTestCasesT :: [(String, String)]
parseTestCasesT =
  [ ("m", "Meter")
  , ("s", "Second")
  , ("ms", opa ++ " Milli Second")
  , ("mm", opa ++ " Milli Meter")
  , ("mmm", "error")
  , ("km", opa ++ " Kilo Meter")
  , ("m s", opm ++ " Meter Second")
  , ("m/s", opd ++ " Meter Second")
  , ("m/s^2", opd ++ " Meter (" ++ ope ++ " Second (Succ (Succ Zero)))")
  , ("s/m m", opd ++ " Second (" ++ opm ++ " Meter Meter)")
  , ("s s/m m", opd ++ " (" ++ opm ++ " Second Second) (" ++ opm ++ " Meter Meter)")
  , ("s*s/m*m", opm ++ " (" ++ opd ++ " (" ++ opm ++ " Second Second) Meter) Meter")
  , ("s*s/(m*m)", opd ++ " (" ++ opm ++ " Second Second) (" ++ opm ++ " Meter Meter)")
  , ("m^-1", ope ++ " Meter (Pred Zero)")
  , ("m^(-1)", ope ++ " Meter (Pred Zero)")
  , ("m^(-(1))", ope ++ " Meter (Pred Zero)")
  , ("1", "Number")
  , ("1/s", opd ++ " Number Second")
  , ("1/s", opd ++ " Number Second")
  , ("m 1 m", opm ++ " (" ++ opm ++ " Meter Number) Meter")
  , ("  ", "Number")
  , ("", "Number")
  ]

parseUnitTestsT :: TestTree
parseUnitTestsT = testGroup "ParseUnitType" $
  map (\(str, out) -> testCase ("`" ++ str ++ "'") $ parseUnitTestT str @?= out)
    parseTestCasesT

----------------------------------------------------------------------
-- Conclusion
----------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Parser"
  [ lexTests
  , mkSymbolTableTests
  , unitStringTests
  , parseUnitTests
  , parseUnitTestsT
  ]

main :: IO ()
main = defaultMain tests
