{-# LANGUAGE NamedFieldPuns #-}
-- | The script that generates the source code for "Data.Tuple.Constraint" and
-- "Data.Tuple.Constraint.ClassNewtype". See the @GenCTuple.sh@ script for how
-- to invoke this.
module Main (main) where

import Data.List.Compat (intercalate, intersperse)
import Prelude ()
import Prelude.Compat
import Options.Applicative

data Args = Args
  { output :: FilePath
  , mode   :: Mode
  } deriving (Eq, Ord, Show)

data Mode
  = Default
  | ClassNewtype
  | TypeFamily
  | TypeSynonym
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

argsParser :: Parser Args
argsParser = Args
  <$> strOption
      (  long "output"
      <> short 'o'
      <> metavar "PATH"
      <> help "The file to which to write the source code" )
  <*> option auto
      (  long "mode"
      <> short 'm'
      <> value Default
      <> help (unlines [ "Which version of Data.Tuple.Constraint should be generated?"
                       , "(" ++ intercalate ", " (map show [minBound..maxBound :: Mode]) ++ ")"
                       ]) )

main :: IO ()
main = execParser opts >>= generate
  where
    opts = info (argsParser <**> helper)
      ( fullDesc
     <> progDesc spiel
     <> header spiel )

    spiel = "Generate the source code for Data.Tuple.Constraint and friends"

generate :: Args -> IO ()
generate args@Args{output} =
  let sourceCode = unlines $ preamble args ++ decs args in
  writeFile output sourceCode

tupleName :: Int -> String
tupleName n
  | n == 0    = "CUnit"
  | n == 1    = "CSolo"
  | otherwise = "CTuple" ++ show n

genClassDef :: Bool -> Int -> String
genClassDef classNewtype n =
     parens (concat (intersperse ", " cNums))
  ++ " => " ++ tupleName n
  ++ [ ' ' | n > 0 ]
  ++ unwords cNums
  where
    parens :: String -> String
    parens s | n == 1    = s
             | otherwise = kindSig $ "(" ++ s ++ ")"

    kindSig :: String -> String
    kindSig s
      | classNewtype = "(" ++ s ++ " :: Constraint)"
      | otherwise    = s

    cNums :: [String]
    cNums = mkVars n

genTypeFamilyDecomposers :: Int -> [String]
genTypeFamilyDecomposers n =
  [ "type family   Decomposer" ++ show n ++ " (x :: Constraint) :: " ++ resKind n
  , "type instance Decomposer" ++ show n ++ " (f " ++ unwords (mkVars n) ++ ") = f"
  ]

genTypeSynonymDecomposer :: Int -> String
genTypeSynonymDecomposer n =
  "type Decomposer" ++ show n ++ " (x :: Proxy ((f :: " ++ resKind n ++ ") " ++ unwords (mkVars n) ++ ")) = f"

resKind :: Int -> String
resKind n = concat $ intersperse " -> " $ replicate (n+1) "Constraint"

genAlias :: Bool -- True for type families, False for type synonyms
         -> Int -> String
genAlias typeFams i =
     "type " ++ tupleName i ++ " = Decomposer" ++ show i ++ " (" ++ arg ++ ")"
  where
    arg :: String
    arg | typeFams  = dummyClass
        | otherwise = "'Proxy :: Proxy (" ++ dummyClass ++ ")"

    dummyClass :: String
    dummyClass = "("
              ++ concat (intersperse ", " $ replicate i "()")
              ++ ") :: Constraint"

mkVars :: Int -> [String]
mkVars n = ['c':show i | i <- [1..n]]

classDefHaddocks :: Int -> [String]
classDefHaddocks i =
  [ "-- | A constraint tuple class with " ++ show i ++
    " argument" ++ pluralSuffix i ++ "."
  ]

aliasHaddocks :: Int -> String
aliasHaddocks i =
  "-- | A type alias for a constraint tuple with " ++ show i ++
  " argument" ++ pluralSuffix i ++ "."

pluralSuffix :: Int -> String
pluralSuffix i | i == 1
               = ""
               | otherwise
               = "s"

preamble :: Args -> [String]
preamble Args{mode} =
  [ "{-# LANGUAGE ConstraintKinds #-}"
  , "{-# LANGUAGE CPP #-}"
  , "{-# LANGUAGE KindSignatures #-}"
  , "{-# LANGUAGE Safe #-}"
  ] ++ case mode of
         Default      -> classDefExts
         ClassNewtype -> classDefExts
         TypeFamily   -> [ "{-# LANGUAGE TypeFamilies #-}" ]
         TypeSynonym  -> [ "{-# LANGUAGE DataKinds #-}"
                         , "{-# LANGUAGE PolyKinds #-}"
                         , "#if __GLASGOW_HASKELL__ < 806"
                         , "{-# LANGUAGE TypeInType #-}"
                         , "#endif"
                         ]
    ++
  [ ""
  , "-- | This module provides " ++ things ++ " that emulate the behavior of GHC's constraint"
  , "-- tuple syntax. Unlike GHC's built-in constraint tuples, the " ++ things ++ " in this"
  , "-- library can be partially applied."
  ] ++ haddockNote ++
  [ "module " ++ modName
  ] ++ exports ++
  [ ""
  ] ++ imports
  where
    classDefExts :: [String]
    classDefExts =
      [ "{-# LANGUAGE FlexibleInstances #-}"
      , "{-# LANGUAGE MultiParamTypeClasses #-}"
      , "{-# LANGUAGE UndecidableInstances #-}"
      , "{-# LANGUAGE UndecidableSuperClasses #-}"
      ]

    things :: String
    things = case mode of
               Default      -> "classes"
               ClassNewtype -> "classes"
               TypeFamily   -> "type aliases"
               TypeSynonym  -> "type aliases"

    modName :: String
    modName = "Data.Tuple.Constraint" ++
              case mode of
                Default -> ""
                _       -> '.' : show mode

    exports :: [String]
    exports =
      [ "  ( -- * Constraint tuples"
      , "    CTuple0"
      , "  , CTuple1"
      ] ++ cTupleExports ++
      [ "  ) where" ]

    cTupleExports :: [String]
    cTupleExports =
      flip concatMap [0..maxTupleSize] $ \i ->
        [ "#if __GLASGOW_HASKELL__ >= 902" | largeTupleSize i ] ++
        [ "  , " ++ tupleName i ] ++
        [ "#endif" | largeTupleSize i ]

    imports :: [String]
    imports =
      case mode of
        Default      -> defaultImports
        ClassNewtype -> constraintImports
        TypeFamily   -> constraintImports
        TypeSynonym  -> [ dtcImports
                        , "import Data.Kind (Constraint)"
                        , "import Data.Proxy (Proxy(..))"
                        ]

    defaultImports :: [String]
    defaultImports =
      [ "#if __GLASGOW_HASKELL__ >= 910"
      , "import GHC.Classes"
      , "#else"
      , "import Data.Kind (Constraint)"
      , "#endif"
      ]

    constraintImports :: [String]
    constraintImports =
      [ dtcImports
      , "import Data.Kind (Constraint)"
      ]

    dtcImports :: String
    dtcImports = "import Data.Tuple.Constraint (CTuple0, CTuple1, CSolo)"

    haddockNote :: [String]
    haddockNote =
      case mode of
        Default ->
          [ "--"
          , "-- When building with GHC 9.10 or later, this will simply re-export the"
          , "-- constraint tuples offered by \"GHC.Classes\"."
          ]
        ClassNewtype ->
          [ "--"
          , "-- Unlike \"Data.Tuple.Constraint\", a @CTupleN@ class defined in this module"
          , "-- (where @N@ is greater than 1) compiles to a newtype around the corresponding"
          , "-- built-in constraint tuple type with @N@ arguments in Core. In contrast, a"
          , "-- @CTupleN@ class defined in \"Data.Tuple.Constraint\" compiles to a"
          , "-- dictionary data type with @N@ fields in Core."
          , "--"
          , "-- For most use cases, this distinction is of no practical consequence. One"
          , "-- scenario where you may benefit from using this module is when you are"
          , "-- interoperating with built-in constraint tuple syntax."
          , "-- For example, in this code:"
          , "--"
          , "-- @"
          , "-- data Dict :: Constraint -> Type where"
          , "--   Dict :: c => Dict c"
          , "--"
          , "-- foo :: CTuple2 a b => Dict (a, b)"
          , "-- foo = Dict"
          , "-- @"
          , "--"
          , "-- If you use the @CTuple2@ class from \"Data.Tuple.Constraint\" to define"
          , "-- @foo@, then in the Core for @foo@, the @a@ and @b@ must be extracted from"
          , "-- the @CTuple2@ dictionary before building the @Dict@ dictionary. On the other"
          , "-- hand, if you use the @CTuple@ class from this module, then no such"
          , "-- extraction is necessary, as the Core can simply cast the @CTuple2@"
          , "-- dictionary (which is a newtype) to the @(a, b)@ dictionary and use that to"
          , "-- construct a @Dict@ dictionary."
          ]
        TypeFamily  -> aliasNote True
        TypeSynonym -> aliasNote False

    aliasNote :: Bool -- True for type families, False for type synonyms
              -> [String]
    aliasNote typeFams =
      [ "--"
      , "-- The type aliases in this module are defined by way of "
          ++ if typeFams then "type families" else "type synonyms" ++ " that"
      , "-- decompose applications of constraint tuple type constructors to their arguments."
      ]

decs :: Args -> [String]
decs Args{mode} =
  concat
    [ concat
        [ [ ""
          , "#if __GLASGOW_HASKELL__ < 910"
          ]
        | mode == Default
        ]
    , [ "" ]
    , extraDefs
    , flip concatMap [0..maxTupleSize] (\i ->
        if mode /= Default && i == 1
        then [] -- CSolo is imported from Data.Tuple.Constraint
        else case mode of
               Default      -> genClassDefs False i
               ClassNewtype -> genClassDefs True  i
               TypeFamily   -> genAliasDefs True  i
               TypeSynonym  -> genAliasDefs False i
             ++ [ "" ])
    , [ "#endif" | mode == Default ]
    ]
  where
    extraDefs :: [String]
    extraDefs =
      concat
        [ [ "-- | An alias for a nullary constraint tuple."
          , "type CTuple0 = (() :: Constraint)"
          , ""
          , "-- | An alias for a unary constraint tuple."
          , "type CTuple1 = CSolo"
          , ""
          ]
        | mode == Default
        ]

    genClassDefs :: Bool -- Should the classes be newtypes?
                 -> Int -> [String]
    genClassDefs classNewtype i =
      let cTuple = genClassDef classNewtype i in
      concat
        [ [ "#if __GLASGOW_HASKELL__ >= 902" | largeTupleSize i ]
        , classDefHaddocks i
        , [ "class    " ++ cTuple
          , "instance " ++ cTuple
          ]
        , [ "#endif" | largeTupleSize i ]
        ]

    genAliasDefs :: Bool -- True for type families, False for type synonyms
                 -> Int -> [String]
    genAliasDefs typeFams i =
      concat
        [ [ "#if __GLASGOW_HASKELL__ >= 902" | largeTupleSize i ]
        , [ aliasHaddocks i
          , genAlias typeFams i
          ]
        , if typeFams
             then   genTypeFamilyDecomposers i
             else [ genTypeSynonymDecomposer i ]
        , [ "#endif" | largeTupleSize i ]
        ]

-- | The maximum tuple size to generate.
--
-- This should stay in sync with the value of @GHC.Exts.maxTupleSize@. We define
-- it here separately because different versions of GHC have different values
-- of @GHC.Exts.maxTupleSize@, and we want the generator script to produce the
-- same output regardless of which GHC version is used to build the script.
maxTupleSize :: Int
maxTupleSize = 64

-- GHC 8.0 through 9.0 impose a maximum tuple size of 62, so one cannot define
-- constraint tuples of larger sizes. GHC 9.2 and later, however, raise the
-- maximum tuple size to 64 (see 'maxTupleSize' above). We would still like to
-- define 63- and 64- tuples whenever possible, so we use this function to check
-- if a tuple is too large for old GHCs (and therefore needs to be guarded by
-- appropriate CPP).
largeTupleSize :: Int -> Bool
largeTupleSize i = i > 62
