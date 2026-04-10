module ShrinkUtils
  ( candidateTransformsWith,
    removeModuleHead,
    shrinkModuleHeadNameWith,
    removeModuleHeadExports,
    shrinkModuleHeadExports,
    removeModuleImports,
    shrinkModuleImports,
    removeModuleDecls,
    shrinkModuleDecls,
    shrinkModuleNameWith,
    shrinkModuleNamePartsWith,
    dropSegmentShrinks,
    shrinkSegmentShrinksWith,
    replaceAt,
    unique,
    splitModuleName,
    joinModuleName,
    isValidModuleName,
    isValidModuleSegment,
    isSegmentRestChar,
  )
where

import Data.Char (isAlphaNum, isUpper)
import Data.List (intercalate)
import Language.Haskell.Exts qualified as HSE

candidateTransformsWith :: (String -> [String]) -> HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
candidateTransformsWith shrinkSegment modu =
  removeModuleHead modu
    <> shrinkModuleHeadNameWith shrinkSegment modu
    <> removeModuleHeadExports modu
    <> shrinkModuleHeadExports modu
    <> removeModuleImports modu
    <> shrinkModuleImports modu
    <> removeModuleDecls modu
    <> shrinkModuleDecls modu

removeModuleHead :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleHead modu =
  case modu of
    HSE.Module loc (Just _) pragmas imports decls ->
      [HSE.Module loc Nothing pragmas imports decls]
    _ -> []

shrinkModuleHeadNameWith :: (String -> [String]) -> HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleHeadNameWith shrinkSegment modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc (HSE.ModuleName nLoc name) warning exports)) pragmas imports decls ->
      [ HSE.Module
          loc
          (Just (HSE.ModuleHead hLoc (HSE.ModuleName nLoc name') warning exports))
          pragmas
          imports
          decls
      | name' <- shrinkModuleNameWith shrinkSegment name
      ]
    _ -> []

removeModuleHeadExports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleHeadExports modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning (Just _))) pragmas imports decls ->
      [HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning Nothing)) pragmas imports decls]
    _ -> []

shrinkModuleHeadExports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleHeadExports modu =
  case modu of
    HSE.Module loc (Just (HSE.ModuleHead hLoc modName warning (Just (HSE.ExportSpecList eLoc specs)))) pragmas imports decls ->
      let removeAt idx = take idx specs <> drop (idx + 1) specs
          shrunkLists = [removeAt idx | idx <- [0 .. length specs - 1]] <> [[], specs]
       in [ HSE.Module
              loc
              (Just (HSE.ModuleHead hLoc modName warning (Just (HSE.ExportSpecList eLoc specs'))))
              pragmas
              imports
              decls
          | specs' <- unique shrunkLists,
            specs' /= specs
          ]
    _ -> []

removeModuleImports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleImports modu =
  case modu of
    HSE.Module loc header pragmas (_ : _) decls ->
      [HSE.Module loc header pragmas [] decls]
    _ -> []

shrinkModuleImports :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleImports modu =
  case modu of
    HSE.Module loc header pragmas imports decls ->
      let removeAt idx = take idx imports <> drop (idx + 1) imports
          shrunkLists =
            [removeAt idx | idx <- [0 .. length imports - 1]]
              <> [replaceAt idx decl' imports | idx <- [0 .. length imports - 1], decl' <- shrinkImportDecl (imports !! idx)]
              <> [replaceAt idx (simplifyImportDecl (imports !! idx)) imports | idx <- [0 .. length imports - 1]]
              <> [[], imports]
       in [HSE.Module loc header pragmas imports' decls | imports' <- unique shrunkLists, imports' /= imports]
    _ -> []

removeModuleDecls :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
removeModuleDecls modu =
  case modu of
    HSE.Module loc header pragmas imports (_ : _) ->
      [HSE.Module loc header pragmas imports []]
    _ -> []

shrinkModuleDecls :: HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
shrinkModuleDecls modu =
  case modu of
    HSE.Module loc header pragmas imports decls ->
      let removeAt idx = take idx decls <> drop (idx + 1) decls
          shrunkLists =
            [removeAt idx | idx <- [0 .. length decls - 1]]
              <> [replaceAt idx decl' decls | idx <- [0 .. length decls - 1], decl' <- simplifyDecl (decls !! idx)]
              <> [[], decls]
       in [HSE.Module loc header pragmas imports decls' | decls' <- unique shrunkLists, decls' /= decls]
    _ -> []

simplifyImportDecl :: HSE.ImportDecl HSE.SrcSpanInfo -> HSE.ImportDecl HSE.SrcSpanInfo
simplifyImportDecl decl =
  decl
    { HSE.importQualified = False,
      HSE.importSrc = False,
      HSE.importSafe = False,
      HSE.importPkg = Nothing,
      HSE.importAs = Nothing,
      HSE.importSpecs = Nothing
    }

shrinkImportDecl :: HSE.ImportDecl HSE.SrcSpanInfo -> [HSE.ImportDecl HSE.SrcSpanInfo]
shrinkImportDecl decl =
  case HSE.importSpecs decl of
    Nothing -> []
    Just (HSE.ImportSpecList specLoc hidingFlag specs) ->
      let removeAt specIdx = take specIdx specs <> drop (specIdx + 1) specs
          shrinkSpecs =
            [ decl {HSE.importSpecs = Just (HSE.ImportSpecList specLoc hidingFlag specs')}
            | specs' <- unique ([removeAt idx | idx <- [0 .. length specs - 1]] <> [take 1 specs]),
              not (null specs'),
              specs' /= specs
            ]
       in shrinkSpecs <> [decl {HSE.importSpecs = Just (HSE.ImportSpecList specLoc hidingFlag [spec'])} | spec <- specs, spec' <- shrinkImportSpec spec]

shrinkImportSpec :: HSE.ImportSpec HSE.SrcSpanInfo -> [HSE.ImportSpec HSE.SrcSpanInfo]
shrinkImportSpec spec =
  case spec of
    HSE.IVar loc name -> [HSE.IVar loc name]
    HSE.IAbs loc ns name -> [HSE.IVar loc name, HSE.IAbs loc ns name]
    HSE.IThingAll loc name -> [HSE.IVar loc name, HSE.IThingAll loc name]
    HSE.IThingWith loc name cnames ->
      [HSE.IThingAll loc name, HSE.IThingWith loc name (take 1 cnames)]

simplifyDecl :: HSE.Decl HSE.SrcSpanInfo -> [HSE.Decl HSE.SrcSpanInfo]
simplifyDecl decl =
  case decl of
    HSE.TypeSig loc names _ty ->
      case names of
        [] -> []
        name : _ -> [HSE.TypeSig loc [name] (simpleType loc)]
    HSE.FunBind loc matches ->
      case matches of
        [] -> []
        match : _ ->
          case match of
            HSE.Match mLoc name _pats _rhs _binds ->
              [HSE.FunBind loc [simplifyMatch mLoc name]]
            HSE.InfixMatch mLoc lhs name _pats _rhs _binds ->
              [HSE.FunBind loc [simplifyMatch mLoc name, HSE.Match mLoc name [lhs] (simpleRhs mLoc name) Nothing]]
    HSE.PatBind loc pat _rhs _binds ->
      [HSE.PatBind loc (simplifyPat pat) (simpleRhs loc (nameFromPat pat)) Nothing]
    _ -> []

simplifyMatch :: HSE.SrcSpanInfo -> HSE.Name HSE.SrcSpanInfo -> HSE.Match HSE.SrcSpanInfo
simplifyMatch loc name =
  HSE.Match loc name [] (simpleRhs loc name) Nothing

simpleRhs :: HSE.SrcSpanInfo -> HSE.Name HSE.SrcSpanInfo -> HSE.Rhs HSE.SrcSpanInfo
simpleRhs loc name =
  HSE.UnGuardedRhs loc (HSE.Var loc (HSE.UnQual loc name))

simpleType :: HSE.SrcSpanInfo -> HSE.Type HSE.SrcSpanInfo
simpleType loc =
  HSE.TyVar loc (HSE.Ident loc "a")

simplifyPat :: HSE.Pat HSE.SrcSpanInfo -> HSE.Pat HSE.SrcSpanInfo
simplifyPat pat =
  case pat of
    HSE.PVar loc name -> HSE.PVar loc name
    HSE.PWildCard loc -> HSE.PWildCard loc
    _ -> HSE.PWildCard (HSE.ann pat)

nameFromPat :: HSE.Pat HSE.SrcSpanInfo -> HSE.Name HSE.SrcSpanInfo
nameFromPat pat =
  case pat of
    HSE.PVar _ name -> name
    _ -> HSE.Ident (HSE.ann pat) "x"

shrinkModuleNameWith :: (String -> [String]) -> String -> [String]
shrinkModuleNameWith shrinkSegment name =
  let parts = splitModuleName name
      joinedShrinks =
        [ joinModuleName parts'
        | parts' <- shrinkModuleNamePartsWith shrinkSegment parts,
          isValidModuleName parts'
        ]
   in unique joinedShrinks

shrinkModuleNamePartsWith :: (String -> [String]) -> [String] -> [[String]]
shrinkModuleNamePartsWith shrinkSegment parts =
  dropSegmentShrinks parts <> shrinkSegmentShrinksWith shrinkSegment parts

-- Remove one segment at a time (if at least one segment remains).
dropSegmentShrinks :: [String] -> [[String]]
dropSegmentShrinks parts =
  [ before <> after
  | idx <- [0 .. length parts - 1],
    let (before, rest) = splitAt idx parts,
    (_ : after) <- [rest],
    not (null (before <> after))
  ]

-- Shrink each segment independently.
shrinkSegmentShrinksWith :: (String -> [String]) -> [String] -> [[String]]
shrinkSegmentShrinksWith shrinkSegment parts =
  [ replaceAt idx segment' parts
  | (idx, segment) <- zip [0 ..] parts,
    segment' <- shrinkSegment segment,
    isValidModuleSegment segment'
  ]

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx value xs =
  let (before, rest) = splitAt idx xs
   in case rest of
        [] -> xs
        (_ : after) -> before <> (value : after)

unique :: (Eq a) => [a] -> [a]
unique = foldr keep []
  where
    keep x acc
      | x `elem` acc = acc
      | otherwise = x : acc

splitModuleName :: String -> [String]
splitModuleName raw =
  case raw of
    "" -> []
    _ -> splitOnDot raw

splitOnDot :: String -> [String]
splitOnDot s =
  case break (== '.') s of
    (prefix, []) -> [prefix]
    (prefix, _ : rest) -> prefix : splitOnDot rest

joinModuleName :: [String] -> String
joinModuleName = intercalate "."

isValidModuleName :: [String] -> Bool
isValidModuleName segments = not (null segments) && all isValidModuleSegment segments

isValidModuleSegment :: String -> Bool
isValidModuleSegment segment =
  case segment of
    first : rest -> isUpper first && all isSegmentRestChar rest
    [] -> False

isSegmentRestChar :: Char -> Bool
isSegmentRestChar ch = isAlphaNum ch || ch == '\'' || ch == '_'
