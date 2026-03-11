{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Elf.Constants.TH ( mkDeclarations
                             , BaseWord(..)
                             ) where

import Control.Monad
import Language.Haskell.TH
#if MIN_VERSION_template_haskell(2,18,0)
import Language.Haskell.TH.Syntax
#endif

data BaseWord = BaseWord8 | BaseWord16 | BaseWord32 | BaseWord64

newNamePE :: String -> Q (Q Pat, Q Exp)
newNamePE s = do
    n <- newName s
    return (varP n, varE n)

mkDeclarations :: BaseWord -> String -> String -> [(String, Integer, String)] -> Q [Dec]
mkDeclarations baseType typeNameString patternPrefixString enums = do

    let typeName = mkName typeNameString
    let patternName s = mkName (patternPrefixString ++ s)
    let
        baseTypeT :: Q Type
        baseTypeT =
            case baseType of
                BaseWord8  -> conT $ mkName "Word8"
                BaseWord16 -> conT $ mkName "Word16"
                BaseWord32 -> conT $ mkName "Word32"
                BaseWord64 -> conT $ mkName "Word64"

    let
        newTypeDef =
            newtypeD
                (cxt [])
                typeName
                []
                Nothing
                (normalC typeName [ bangType (bang noSourceUnpackedness noSourceStrictness) baseTypeT ])
                [ derivClause Nothing [ conT (mkName "Eq")
                                      , conT (mkName "Ord")
                                      , conT (mkName "Enum")
                                      , conT (mkName "Num")
                                      , conT (mkName "Real")
                                      , conT (mkName "Integral")
                                      , conT (mkName "Bits")
                                      , conT (mkName "FiniteBits")
                                      ]
                ]

    let
        mkShowClause (s, n, _) =
            clause
                [ conP typeName [litP $ IntegerL n] ]
                (normalB [| patternPrefixString ++ s |])
                []

    let
        showClauses :: [Q Clause]
        showClauses = map mkShowClause enums

    (nP, nE) <- newNamePE "n"
    let
        defaultShowClause =
            clause
                [ conP typeName [nP] ]
                (normalB [| typeNameString ++ " " ++ show $(nE) |])
                []

    let showInstanceFunctions = funD (mkName "show") (showClauses ++ [ defaultShowClause ])

    let showInstance = instanceD (cxt []) (appT (conT (mkName "Show")) (conT typeName)) [ showInstanceFunctions ]

    let
        mkBinaryInstance :: Q Type -> Q Pat -> Q Exp -> Q Exp -> Q Dec
        mkBinaryInstance typeT putP putE getE =
            instanceD
                (cxt [])
                (appT (conT (mkName "Binary")) typeT)
                [ binaryInstanceGet, binaryInstancePut ]
            where
                binaryInstancePut =
                    funD
                        (mkName "put")
                        [ clause
                            [putP]
                            (normalB putE)
                            []
                        ]
                binaryInstanceGet =
                    funD
                        (mkName "get")
                        [ clause
                            []
                            (normalB getE)
                            []
                        ]

    let
        binaryInstancesXe putLe getLe putBe getBe =
            [ do
                (n3P, n3E) <- newNamePE "n"
                mkBinaryInstance
                    (appT (conT $ mkName "Le") (conT typeName))
                    (conP (mkName "Le") [conP typeName [n3P]])
                    [| $putLe $n3E |]
                    [| $(conE $ mkName "Le") <$> ($(conE typeName) <$> $getLe) |]
            , do
                (n3P, n3E) <- newNamePE "n"
                mkBinaryInstance
                    (appT (conT $ mkName "Be") (conT typeName))
                    (conP (mkName "Be") [conP typeName [n3P]])
                    [| $putBe $n3E |]
                    [| $(conE $ mkName "Be") <$> ($(conE typeName) <$> $getBe) |]
            ]

    let
        binaryInstances =
            case baseType of
                BaseWord8 ->
                    [ do
                        (n3P, n3E) <- newNamePE "n"
                        mkBinaryInstance
                            (conT typeName)
                            (conP typeName [n3P])
                            [| putWord8 $n3E |]
                            [| $(conE typeName) <$> getWord8 |]
                    ]
                BaseWord16 -> binaryInstancesXe [| putWord16le |] [| getWord16le |] [| putWord16be |] [| getWord16be |]
                BaseWord32 -> binaryInstancesXe [| putWord32le |] [| getWord32le |] [| putWord32be |] [| getWord32be |]
                BaseWord64 -> binaryInstancesXe [| putWord64le |] [| getWord64le |] [| putWord64be |] [| getWord64be |]

    let
        mkPatterns (s, n, _) =
            [ patSynSigD
                (patternName s)
                (conT typeName)
            , patSynD
                (patternName s)
                (prefixPatSyn [])
                implBidir
                (conP typeName [litP $ IntegerL n])
            ]

    let patterns = join (map mkPatterns enums)

#if MIN_VERSION_template_haskell(2,18,0)
    let mkPatternDocs (s, _, doc) = putDoc (DeclDoc $ patternName s) doc

    mapM_ (addModFinalizer . mkPatternDocs) enums
#endif

    sequence $ newTypeDef : showInstance : patterns ++ binaryInstances
