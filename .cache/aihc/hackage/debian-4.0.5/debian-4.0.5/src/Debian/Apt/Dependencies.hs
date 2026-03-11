{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-missing-signatures #-}
module Debian.Apt.Dependencies
{-
    ( solve
    , State
    , binaryDepends
    , search
    , bj'
    , bt
    , CSP(..)
    ) -} where

-- test gutsyPackages "libc6" (\csp -> bt csp)

import Control.Arrow (second)
import qualified Data.ByteString.Char8 as C
import Data.List as List (find, union)
import Data.Tree (Tree(rootLabel, Node))
import Debian.Apt.Package (PackageNameMap, packageNameMap, lookupPackageByRel)
import Debian.Control.ByteString (ControlFunctions(stripWS, lookupP, parseControlFromFile),
                                  Field'(Field, Comment), Control'(Control), Paragraph, Control)
import Debian.Relation (BinPkgName(..))
import Debian.Relation.ByteString (ParseRelations(..), Relation(..), OrRelation, AndRelation, Relations, checkVersionReq)
import Debian.Version (DebianVersion, parseDebianVersion, prettyDebianVersion)
import Debian.Version.ByteString ()
import Text.PrettyPrint (render)

-- * Basic CSP Types and Functions

data Status
    = Remaining AndRelation
    | MissingDep Relation
    | Complete
      deriving (Eq)

type State a = (Status, [a])

complete :: State a -> Bool
complete (Complete, _) = True
complete _ = False

data CSP a
    = CSP { pnm :: PackageNameMap a
          , relations :: Relations
          , depFunction :: (a -> Relations)
          , conflicts :: a -> Relations
          , packageVersion :: a -> (BinPkgName, DebianVersion)
          }

-- * Test CSP

-- |TODO addProvides -- see DQL.Exec
controlCSP :: Control -> Relations -> (Paragraph -> Relations) -> CSP Paragraph
controlCSP (Control paragraphs) rels depF' =
    CSP { pnm = packageNameMap getName paragraphs
        , relations = rels
        , depFunction = depF'
        , conflicts = conflicts'
        , packageVersion = packageVersionParagraph
        }
    where
      getName :: Paragraph -> BinPkgName
      getName p = case lookupP "Package" p of
                    Nothing -> error "Missing Package field"
                    Just (Field (_,n)) -> BinPkgName (C.unpack (stripWS n))
                    Just (Comment _) -> error "controlCSP"
      conflicts' :: Paragraph -> Relations
      conflicts' p =
          case lookupP "Conflicts" p of
            Nothing -> []
            Just (Field (_, c)) -> either (error . show) id (parseRelations c)
            Just (Comment _) -> error "controlCSP"

testCSP :: FilePath -> (Paragraph -> Relations) -> String -> (CSP Paragraph -> IO a) -> IO a
testCSP controlFile depf relationStr cspf =
    do c' <- parseControlFromFile controlFile
       case c' of
         Left e -> error (show e)
         Right control@(Control _) ->
             case parseRelations relationStr of
               Left e -> error (show e)
               Right r ->
                     cspf (controlCSP control r depf)

depF :: Paragraph -> Relations
depF p =
    let preDepends =
            case lookupP "Pre-Depends" p of
              Nothing -> []
              Just (Field (_,pd)) ->
                  either (error . show) id (parseRelations pd)
              Just (Comment _) -> error "depF"
        depends =
            case lookupP "Depends" p of
              Nothing -> []
              Just (Field (_,pd)) ->
                  either (error . show) id (parseRelations pd)
              Just (Comment _) -> error "depF"
    in
      preDepends ++ depends

sidPackages = "/var/lib/apt/lists/ftp.debian.org_debian_dists_unstable_main_binary-i386_Packages"
gutsyPackages = "/var/lib/apt/lists/mirror.anl.gov_pub_ubuntu_dists_gutsy_main_binary-i386_Packages"

test controlFP rel labeler =
    testCSP controlFP depF rel (mapM_ (\ (_,p) -> mapM_ (print . second (render . prettyDebianVersion) . packageVersionParagraph) p ) . take 1 . search labeler)

-- TODO: add better errors
packageVersionParagraph :: Paragraph -> (BinPkgName, DebianVersion)
packageVersionParagraph p =
    case lookupP "Package" p of
      Nothing -> error $ "Paragraph missing Package field"
      (Just (Field (_, name))) ->
          case lookupP "Version" p of
            Nothing -> error $ "Paragraph missing Version field"
            (Just (Field (_, str))) ->
                case parseDebianVersion str of
                  Right ver -> (BinPkgName (C.unpack (stripWS name)), ver)
                  Left e -> error $ "packageVersionParagraph: " ++ show e
            (Just (Comment _)) -> error "packageVersionParagraph"
      (Just (Comment _)) -> error "packageVersionParagraph"



conflict :: CSP p -> p -> p -> Bool
conflict csp p1 p2 =
    let (name1, version1) = (packageVersion csp) p1
        (name2, version2) = (packageVersion csp) p2
    in
      if name1 == name2
      then version1 /= version2
      else
        any (conflict' (name1, version1)) (concat $ (conflicts csp) p2) ||
        any (conflict' (name2, version2)) (concat $ (conflicts csp) p1)

-- |JAS: deal with 'Provides' (can a package provide more than one package?)
conflict' :: (BinPkgName, DebianVersion) -> Relation -> Bool
conflict' (pName, pVersion) (Rel pkgName mVersionReq _) =
    (pName == pkgName) && (checkVersionReq mVersionReq (Just pVersion))



-- * Tree Helper Functions

mkTree :: a -> [Tree a] -> Tree a
mkTree = Node

label :: Tree a -> a
label = rootLabel

initTree :: (a -> [a]) -> a -> Tree a
initTree f a = Node a (map (initTree f) (f a))

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = fmap

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (Node a ts) = f a (map (foldTree f) ts)

zipTreesWith :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
zipTreesWith f (Node a ts) (Node b us) =
    Node (f a b) (zipWith (zipTreesWith f) ts us)

prune :: (a -> Bool) -> Tree a -> Tree a
prune p = foldTree f
    where f a ts = Node a (filter (not . p . label) ts)

leaves :: Tree a -> [a]
leaves = foldTree f
    where f leaf [] = [leaf]
          f _ ts = concat ts

inhTree :: (b -> a -> b) -> b -> Tree a -> Tree b
inhTree f b (Node a ts) = Node b' (map (inhTree f b') ts)
    where b' = f b a

distrTree :: (a -> [b]) -> b -> Tree a -> Tree b
distrTree  f b (Node a ts) = Node b (zipWith (distrTree f) (f a) ts)

-- * mkSearchTree

-- TODO: might want to leave markers about what relation we are satisfying?
mkSearchTree :: forall a. CSP a -> Tree (State a)
mkSearchTree csp =
    Node (Remaining (relations csp),[]) (andRelation ([],[]) (relations csp))
    where
      andRelation :: ([a],AndRelation) -> AndRelation -> [Tree (State a)]
      andRelation (candidates,[]) [] = [Node (Complete, candidates) []]
      andRelation (candidates,remaining) [] = andRelation (candidates, []) remaining
      andRelation (candidates, remaining) (x:xs) =
          orRelation (candidates, xs ++ remaining) x
      orRelation :: ([a],AndRelation) -> OrRelation -> [Tree (State a)]
      orRelation acc x =
          concat (fmap (relation acc) x)
      relation :: ([a],AndRelation) -> Relation -> [Tree (State a)]
      relation acc@(candidates,_) rel =
          let packages = lookupPackageByRel (pnm csp) (packageVersion csp) rel in
          case packages of
            [] -> [Node (MissingDep rel, candidates) []]
            _ -> map (package acc) packages
      package :: ([a],AndRelation) -> a -> Tree (State a)
      package (candidates, remaining) p =
          if ((packageVersion csp) p) `elem` (map (packageVersion csp) candidates)
          then if null remaining
               then Node (Complete, candidates) []
               else Node (Remaining remaining, candidates) (andRelation (candidates, []) remaining)
          else Node (Remaining remaining, (p : candidates)) (andRelation ((p : candidates), remaining) ((depFunction csp) p))


-- |earliestInconsistency does what it sounds like
-- the 'reverse as' is because the vars are order high to low, but we
-- want to find the lowest numbered (aka, eariest) inconsistency ??
--
earliestInconsistency :: CSP a -> State a -> Maybe ((BinPkgName, DebianVersion), (BinPkgName, DebianVersion))
earliestInconsistency _ (_,[]) = Nothing
earliestInconsistency _ (_,[_p]) = Nothing
earliestInconsistency csp (_,(p:ps)) =
    case find ((conflict csp) p) (reverse ps) of
      Nothing -> Nothing
      (Just conflictingPackage) -> Just ((packageVersion csp) p, (packageVersion csp) conflictingPackage)

-- * Conflict Set

-- | conflicting packages and relations that require non-existant packages
type ConflictSet = ([(BinPkgName, DebianVersion)],[Relation])

isConflict :: ConflictSet -> Bool
isConflict ([],[]) = False
isConflict _ = True

solutions :: Tree (State a, ConflictSet) -> [State a]
solutions = filter complete . map fst . leaves . prune (isConflict . snd)

type Labeler a = CSP a -> Tree (State a) -> Tree (State a, ConflictSet)

search :: Labeler a -> CSP a -> [State a]
search labeler csp = (solutions . (labeler csp) . mkSearchTree) csp

-- * Backtracking Labeler

bt :: Labeler a
bt csp = mapTree f
    where
      f s@(status,_) =
              case status of
                (MissingDep rel) -> (s, ([], [rel]))
                _ ->
                    (s,
                      case (earliestInconsistency csp) s of
                        Nothing -> ([],[])
                        Just (a,b) -> ([a,b], []))

-- * BackJumping Solver

{-|bj - backjumping labeler

If the node already has a conflict set, then leave it alone.

Otherwise, the conflictset for the node is the combination of the
conflict sets of its direct children.
-}
bj :: CSP p -> Tree (State p, ConflictSet) -> Tree (State p, ConflictSet)
bj csp = foldTree f
    where f (s, cs) ts
            | isConflict cs  = mkTree (s, cs) ts
--            | isConflict cs' = mkTree (s, cs') [] -- prevent space leak
            | otherwise = mkTree (s, cs') ts
            where cs' =
                      let set = combine csp (map label ts) [] in
                      set `seq` set -- prevent space leak

unionCS :: [ConflictSet] -> ConflictSet
unionCS css = foldr (\(c1, m1) (c2, m2) -> ((c1 `union` c2), (m1 `union` m2))) ([],[]) css

combine :: CSP p -> [(State p, ConflictSet)] -> [ConflictSet] -> ConflictSet
combine _ [] acc = unionCS acc
combine csp ((s,cs@(c,m)):ns) acc
    | (not (lastvar `elem` c)) && null m = cs
    | null c && null m = ([],[]) -- is this case ever used?
    | otherwise = combine csp ns ((c, m):acc)
    where lastvar =
              let (_,(p:_)) = s in (packageVersion csp) p


