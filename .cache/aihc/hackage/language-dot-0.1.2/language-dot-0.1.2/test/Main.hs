module Main (main) where

import Control.Monad (unless)
import Data.Char     (toLower, toUpper)
import System.Exit   (exitSuccess, exitFailure)

import Text.Parsec
import Text.Parsec.String

import Language.Dot.Parser
import Language.Dot.Pretty
import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = do
    let sumBoth (a,b) (c,d) = (a+c, b+d)
    (np,nf) <- foldr sumBoth (0,0) <$> sequence
               [
                 testParser "parsePort"      parsePort      parsePortTests
               , testParser "parseCompass"   parseCompass   parseCompassTests
               , testParser "parseAttribute" parseAttribute parseAttributeTests
               , testParser "parseId"        parseId        parseIdTests
               , testParser "parseGraphs"    parseGraph     parseGraphTests
               , testRoundTrip "RT1" "digraph T1 { n1 -> n2; n1 -> n3 -> n4; }"
               , testRoundTrip "RT2" "graph T2 { n1 [ shape=\"oval\" ]\n\
                                     \           n1 -- n2 [ style=\"dotted\"];\n\
                                     \ }"
               , testRoundTrip "RT3" "digraph T3 {\n\
                                     \  n1 [ shape=\"rectangle\" \n\
                                     \       label=\"This is a very long string that may interact with the pretting printing of the output\"\n\
                                     \       style=\"rounded\"\n\
                                     \     ];\n\
                                     \  n1 -> n2 -> n3 -> n4 -> n5 -> n6 [\n\
                                     \    style=\"dotted\"\n\
                                     \    label=\"This is another very long string whose purpose is to ensure that pretty printing is working correctly when there are very long strings in the output\"\n\
                                     \    samehead=\"Yes, use the same head if there are multiples\"\n\
                                     \    ];\n\
                                     \ }"
               ]
    unless (nf == 0) $ do
      putStrLn ("Final results: "
                <> show np <> " tests passed but "
                <> show nf <> " failed.")
      exitFailure
    putStrLn $ "All " <> show np <> " tests passed."
    exitSuccess


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parsePortTests :: [(String, Port)]
parsePortTests =
    [ ( ":\"x\""          , PortI (StringId  "x"       ) Nothing          )
    , ( ":\"\\t\\\"\":nw" , PortI (StringId  "\\t\""   ) (Just CompassNW) )
    , ( ":-.0004"         , PortI (FloatId   (-0.0004) ) Nothing          )
    , ( ":-1.23:sE"       , PortI (FloatId   (-1.23)   ) (Just CompassSE) )
    , ( ":123"            , PortI (IntegerId 123       ) Nothing          )
    , ( ":123:NE"         , PortI (IntegerId 123       ) (Just CompassNE) )
    , ( ":__2xYz"         , PortI (NameId    "__2xYz"  ) Nothing          )
    , ( ":__2xYz:S"       , PortI (NameId    "__2xYz"  ) (Just CompassS)  )
    , ( ":n"              , PortC CompassN  )
    , ( ":SE"             , PortC CompassSE )
    ]

parseCompassTests :: [(String, Compass)]
parseCompassTests =
    concat
      [ [ (t, CompassN)  | t <- allCaps "n"  ]
      , [ (t, CompassE)  | t <- allCaps "e"  ]
      , [ (t, CompassS)  | t <- allCaps "s"  ]
      , [ (t, CompassW)  | t <- allCaps "w"  ]
      , [ (t, CompassNE) | t <- allCaps "ne" ]
      , [ (t, CompassNW) | t <- allCaps "nw" ]
      , [ (t, CompassSE) | t <- allCaps "se" ]
      , [ (t, CompassSW) | t <- allCaps "sw" ]
      ]

parseAttributeTests :: [(String, Attribute)]
parseAttributeTests =
    [ ( "a"                      , AttributeSetTrue  (NameId "a")                           )
    , ( "a=b"                    , AttributeSetValue (NameId "a")       (NameId "b")        )
    , ( "-.003\t=\r\n  _xYz123_" , AttributeSetValue (FloatId (-0.003)) (NameId "_xYz123_") )
    , ( "\"\\t\\\"\"  =-123"     , AttributeSetValue (StringId "\\t\"") (IntegerId (-123))  )
    ]

parseIdTests :: [(String, Id)]
parseIdTests =
    [ ( "a"             , NameId    "a"         )
    , ( "A1"            , NameId    "A1"        )
    , ( "_2X"           , NameId    "_2X"       )
    , ( "\"\""          , StringId  ""          )
    , ( "\"\\t\\r\\n\"" , StringId  "\\t\\r\\n" )
    , ( ".0"            , FloatId   0.0         )
    , ( ".123"          , FloatId   0.123       )
    , ( "+.999"         , FloatId   0.999       )
    , ( "-.001"         , FloatId   (-0.001)    )
    , ( "+.001"         , FloatId   0.001       )
    , ( "0.0"           , FloatId   0.0         )
    , ( "1.2"           , FloatId   1.2         )
    , ( "123.456"       , FloatId   123.456     )
    , ( "0"             , IntegerId 0           )
    , ( "+0"            , IntegerId 0           )
    , ( "-0"            , IntegerId 0           )
    , ( "123"           , IntegerId 123         )
    , ( "-123"          , IntegerId (-123)      )
    ]

parseGraphTests :: [(String, Graph)]
parseGraphTests =
  [
    ( "digraph T1 { n1 -> n2; n1 -> n3 -> n4; }"
    , Graph UnstrictGraph DirectedGraph (Just $ NameId "T1")
      [
        EdgeStatement [ ENodeId NoEdge (NodeId (NameId "n1") Nothing)
                      , ENodeId DirectedEdge (NodeId (NameId "n2") Nothing)
                      ]
        []
      , EdgeStatement [ ENodeId NoEdge (NodeId (NameId "n1") Nothing)
                      , ENodeId DirectedEdge (NodeId (NameId "n3") Nothing)
                      , ENodeId DirectedEdge (NodeId (NameId "n4") Nothing)
                      ]
        []
      ]
    )

  , ( "graph T2 { n1 [ shape=\"oval\" ]; n1 -- n2 [ style=\"dotted\"]; }"
    , Graph UnstrictGraph UndirectedGraph (Just $ NameId "T2")
      [
        NodeStatement (NodeId (NameId "n1") Nothing)
        [ AttributeSetValue (NameId "shape") (StringId "oval") ]
      , EdgeStatement [ ENodeId NoEdge (NodeId (NameId "n1") Nothing)
                      , ENodeId UndirectedEdge (NodeId (NameId "n2") Nothing)
                      ]
        [ AttributeSetValue (NameId "style") (StringId "dotted") ]
      ]
    )
  ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

testParser :: (Eq a, Show a) => String -> Parser a -> [(String, a)] -> IO (Int,Int)
testParser name parser tests =
    help tests [] (0 :: Int) (0 :: Int)
  where
    help [] es np nf = do
        putStrLn $ name ++ ": " ++ show np ++ " passed, " ++ show nf ++ " failed"
        mapM_ (putStrLn . ("  "++)) (reverse es)
        unless (null es) (putStrLn "")
        return (np, nf)
    help ((i,o):ts) es np nf =
        case parse' parser i of
          Left  _ -> help ts (makeFailureMessage name i o : es) np (succ nf)
          Right v ->
            if v /= o
              then help ts (makeFailureMessage' name i o v : es) np (succ nf)
              else help ts es (succ np) nf

makeFailureMessage :: (Show a) => String -> String -> a -> String
makeFailureMessage name i o =
    "(" ++ name ++ " " ++ show i ++ ")" ++
    " should have returned " ++ "(" ++ show o ++ ")"

makeFailureMessage' :: (Show a) => String -> String -> a -> a -> String
makeFailureMessage' name i o v =
    "(" ++ name ++ " " ++ show i ++ ")" ++
    " returned "  ++ "(" ++ show v ++ ")" ++
    ", expected " ++ "(" ++ show o ++ ")"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

testRoundTrip :: String -> String -> IO (Int,Int)
testRoundTrip testName input =
  case parse' parseGraph input of
    Left e -> do putStrLn $ "Parse failed for test " <> testName
                 putStrLn $ "Input: " <> input
                 putStrLn $ "Error: " <> show e
                 return (0,1)
    Right v -> case parse' parseGraph $ renderDot v of
      Left _ -> do putStrLn $ "Parse failed for pretty form in test " <> testName
                   putStrLn $ "Pretty: " <> renderDot v
                   return (0,1)
      Right v' -> if v == v'
                  then do putStrLn $ "Parse success for " <> testName
                          -- putStrLn $ renderDot v
                          return (1,0)
                  else do putStrLn $ "Doubled round-trip failure for " <> testName <> ":"
                          putStrLn $ "First parse: " <> show v
                          putStrLn $ "Second parse: " <> show v'
                          return (0,1)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

allCaps :: String -> [String]
allCaps []     = [[]]
allCaps (c:cs) =
    concatMap (\t -> [l:t, u:t]) (allCaps cs)
  where
    l = toLower c
    u = toUpper c
