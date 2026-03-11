{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}
module Dependencies where

import Control.Arrow
import Test.HUnit

import Debian.Control.String
import Debian.Apt.Dependencies hiding (packageVersionParagraph)
import Debian.Relation
import Debian.Version
import Debian.Apt.Package


packageA =
    [ ("Package", " a")
    , ("Version", " 1.0")
    , ("Depends", " b")
    ]

packageB =
    [ ("Package", " b")
    , ("Version", " 1.0")
    ]

packageC =
    [ ("Package", " c")
    , ("Version", " 1.0")
    , ("Depends", " doesNotExist")
    ]

packageD =
    [ ("Package", " d")
    , ("Version", " 1.0")
    , ("Depends", " e | f, g | h")
    ]

packageE =
    [ ("Package", " e")
    , ("Version", " 1.0")
    ]

packageF =
    [ ("Package", " f")
    , ("Version", " 1.0")
    ]

packageG =
    [ ("Package", " g")
    , ("Version", " 1.0")
    ]

packageH =
    [ ("Package", " h")
    , ("Version", " 1.0")
    ]

packageI =
    [ ("Package", " i")
    , ("Version", " 1.0")
    , ("Depends", " k")
    ]

packageJ =
    [ ("Package", " j")
    , ("Version", " 1.0")
    , ("Provides", " k")
    ]

packageK =
    [ ("Package", " k")
    , ("Version", " 1.0")
    ]



control 
    = [ packageA
      , packageB
      , packageC
      , packageD
      , packageE
      , packageF
      , packageG
      , packageH
      , packageI
      , packageJ
      , packageK
      ]

depends p =
    case lookup "Depends" p of
      Nothing -> []
      (Just v) -> either (error . show) id (parseRelations v)

mkCSP :: [[(String, String)]] -> String -> ([(String, String)] -> Relations) -> CSP [(String, String)]
mkCSP paragraphs relStr depF' =
    CSP { pnm = addProvides providesF paragraphs $ packageNameMap getName paragraphs
        , relations = either (error . show) id (parseRelations relStr)
        , depFunction = depF'
        , conflicts = conflicts'
        , packageVersion = packageVersionParagraph
        }
    where
      getName :: [(String, String)] -> BinPkgName
      getName p = case lookup "Package" p of Nothing -> error "Missing Package field" ; (Just n) -> BinPkgName (stripWS n)
      conflicts' :: [(String, String)] -> Relations
      conflicts' p = 
          case lookup "Conflicts" p of
            Nothing -> []
            (Just c) -> either (error . show) id (parseRelations c)

      providesF :: [(String, String)] -> [BinPkgName]
      providesF p =
          case lookup "Provides" p of
            Nothing -> []
            (Just v) ->
                 map BinPkgName $ parseCommaList v

parseCommaList :: String -> [String]
parseCommaList str =
    words $ map (\c -> if c == ',' then ' ' else c) str

packageVersionParagraph :: [(String, String)] -> (BinPkgName, DebianVersion) 
packageVersionParagraph p =
    case lookup "Package" p of
      Nothing -> error $ "Could not find Package in " ++ show p
      (Just n) -> 
          case lookup "Version" p of
            Nothing -> error $ "Could not find Package in " ++ show p
            (Just v) -> 
                (BinPkgName (stripWS n), parseDebianVersion' v)

mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd f = map (second f)

deriving instance Show Status
-- deriving instance Show Relation
-- deriving instance Show VersionReq
-- deriving instance Show ArchitectureReq

test1 =
    let csp = mkCSP control "a" depends
        expected = [ (Complete, [packageB, packageA])]
    in
      TestCase (assertEqual "test1" expected (search bt csp))

missing1 =
    let csp = mkCSP control "c" depends
        expected = []
    in
      TestCase (assertEqual "missing1" expected (search bt csp))

ors1 =
    let csp = mkCSP control "d" depends
        expected = [ (Complete, [packageG, packageE, packageD])
                   , (Complete, [packageH, packageE, packageD])
                   , (Complete, [packageG, packageF, packageD])
                   , (Complete, [packageH, packageF, packageD])
                   ]
    in
      TestCase (assertEqual "ors1" expected (search bt csp))

provides1 =
    let csp = mkCSP control "i" depends
        expected = [ (Complete, [packageK, packageI])
                   , (Complete, [packageJ, packageI])
                   ]
    in
      TestCase (assertEqual "provides1" expected (search bt csp))

provides2 =
    let csp = mkCSP control "k" depends
        expected = [ (Complete, [packageK])
                   , (Complete, [packageJ])
                   ]
    in
      TestCase (assertEqual "provides2" expected (search bt csp))

dependencyTests =
    [ test1
    , missing1
    , ors1
    , provides1
    , provides2
    ]
-- runTestText putTextToShowS test1  >>= \(c,st) -> putStrLn (st "")
