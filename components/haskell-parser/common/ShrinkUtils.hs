module ShrinkUtils
  ( candidateTransformsWith,
    removeModuleHead,
    shrinkModuleHeadNameWith,
    removeModuleHeadExports,
    shrinkModuleHeadExports,
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
import qualified Language.Haskell.Exts as HSE

candidateTransformsWith :: (String -> [String]) -> HSE.Module HSE.SrcSpanInfo -> [HSE.Module HSE.SrcSpanInfo]
candidateTransformsWith shrinkSegment modu =
  removeModuleHead modu
    <> shrinkModuleHeadNameWith shrinkSegment modu
    <> removeModuleHeadExports modu
    <> shrinkModuleHeadExports modu

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
