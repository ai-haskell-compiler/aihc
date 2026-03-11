module Generate where

-- needs: haskell-src-exts, curl >= 1.3.8
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Data.Char (toUpper)

import Network.HTTP
import Data.Text (Text, pack, unpack, breakOn, splitOn)
import qualified Data.Text as T
import Data.Maybe
import Data.List

gen triples = prettyPrint $ 
    -- Module SrcLoc ModuleName [ModulePragma] (Maybe WarningText) (Maybe [ExportSpec]) [ImportDecl] [Decl] 
    Module (SrcLoc n 0 0) (ModuleName n) [] Nothing Nothing 
        [ImportDecl loc (ModuleName "Prelude") False False False Nothing Nothing
            (Just (True, [IThingAll (Ident "Ordering") ])) -- hidding Ordering because of the symbol LT
        ] 
        [iso639_1, toCharsFun, fromCharsFun, languageFun]
    where
        n = "Data.LanguageCodes"
        loc = SrcLoc n 0 0 
        iso639_1 = DataDecl loc DataType [] (Ident "ISO639_1") [] 
                   (codes languages_1) -- constructors
                   derivingClasses     -- deriving
        code (_,c) = QualConDecl loc [] [] 
                   $ ConDecl (Ident $ map toUpper c) []
        codes lst = map code lst
        languages_1 = catMaybes $ map (\(l,_,c) -> fmap (\a -> (unpack l, unpack a)) c) triples
        derivingClasses = map (\a -> (UnQual $ Ident a, []) ) 
                        ["Show", "Read", "Eq", "Enum", "Ord"]
        fromCharsFun = FunBind 
            [ Match loc (Ident "fromChars") 
                [PVar (Symbol "c1"), PVar (Symbol "c2")] 
                Nothing
                ( UnGuardedRhs 
                    (Case ( Tuple Boxed [ Var $ UnQual $ Symbol "c1"
                                        , Var $ UnQual $ Symbol "c2"
                                        ]
                          ) (reverse $ languageNoneAlt 
                                     : (map languageCodeAlt languages_1)
                            )
                    )
                )
                (BDecls [])
            ]
        languageCodeAlt (_,c) = 
            Alt loc (PTuple Boxed [ PLit Signless $ Char $ c !! 0
                                  , PLit Signless $ Char $ c !! 1
                                  ]) 
                    (UnGuardedRhs $ App (Con $ UnQual $ Symbol "Just")
                                        (Con $ UnQual $ Symbol $ map toUpper c))
                    (BDecls [])
        languageNoneAlt = 
            Alt loc PWildCard (UnGuardedRhs (Con $ UnQual $ Symbol "Nothing"))
                    (BDecls [])
        toCharsFun = FunBind 
            [ Match loc (Ident "toChars") 
                [PVar (Symbol "code")] 
                Nothing
                ( UnGuardedRhs (Case ( Var $ UnQual $ Symbol "code" )
                                     ( map languageAlt languages_1)
                               )
                )
                (BDecls [])
            ]
        languageAlt (_,c) = 
                    Alt loc (PApp (UnQual $ Symbol $ map toUpper c) [])
                            (UnGuardedRhs $ Tuple Boxed [ Lit (Char $ c !! 0)
                                                        , Lit (Char $ c !! 1)
                                                        ]
                            ) 
                    (BDecls [])
        languageFun = FunBind 
            [ Match loc (Ident "language") 
                [PVar (Symbol "code")] 
                Nothing
                ( UnGuardedRhs (Case ( Var $ UnQual $ Symbol "code" )
                                     ( map languageNameAlt languages_1 )
                               )
                )
                (BDecls [])
            ]
        languageNameAlt (n,c) = 
                Alt loc (PApp (UnQual $ Symbol $ map toUpper c) [])
                              (UnGuardedRhs $ Lit $ String n) 
                        (BDecls [])

-- fetch table from official site
fetchTable = do
    str <- simpleHTTP (getRequest url) >>= getResponseBody
    let site = pack str
    return site
    where
        url = "http://www.loc.gov/standards/iso639-2/php/English_list.php"

-- dirty table parsing
parseTable html = map (rowToTriple . T.lines) $ rows html 
    where 
        rows h = filter isValid $ map fixLine (tail $ T.splitOn (pack "</tr>") h)
        tagTxt = T.tail . T.takeWhile (/= '<') . T.dropWhile (/= '>')
        isValid t = T.take 3 t == T.pack "<tr"
        fixLine = T.replace (T.pack "><") (T.pack ">\n<") 
                . T.dropWhile (/= '<')
        rowToTriple l = (tagTxt (l !! 1), tagTxt (l !! 4)
                        , if tagTxt (l !! 5) == pack "&nbsp;" 
                            then Nothing
                            else Just (tagTxt (l !! 5))
                        )
        
doIt = do
    t <- fetchTable
    let uniqueLang f = nubBy (\(_,_,c1) (_,_,c2) -> c1 == c2) f
        sortedUnique = sortBy (\(_,_,c1) (_,_,c2) -> compare c1 c2) . uniqueLang
    writeFile "../Data/LanguageCodes.hs" $ gen (sortedUnique $ parseTable t)
