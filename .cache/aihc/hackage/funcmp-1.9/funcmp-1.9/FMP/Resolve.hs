{- |
   Module      :  FMP.Resolve
   Copyright   :  (c) 2003-2010 Peter Simons
                  (c) 2002-2003 Ferenc Wágner
                  (c) 2002-2003 Meik Hellmund
                  (c) 1998-2002 Ralf Hinze
                  (c) 1998-2002 Joachim Korittky
                  (c) 1998-2002 Marco Kuhlmann
   License     :  GPLv3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable
 -}
{-
  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <http://www.gnu.org/licenses/>.
 -}

module FMP.Resolve (
      resolvePoint, resolvePath, resolveNumeric, resolveEquation,
      maybes2List,
      symEquations,
      insertNumeric
      ) where

import FMP.Types
import FMP.Picture
import FMP.Syntax
import FMP.Symbols
import Control.Monad ( mplus )

maybes2List                   :: [Maybe a] -> [a]
maybes2List []                =  []
maybes2List (Just a :ls)      =  a:maybes2List ls
maybes2List (Nothing:ls)      =    maybes2List ls

-- L"ose die Symbole in Ausdr"ucken auf.

flattenName                   :: Name -> Name
flattenName (Hier a b)        =  flattenName' (Hier a b) Nothing
flattenName (Global (Hier a b) )
                              =  flattenName' (Hier a b) Nothing
flattenName a                 =  a

flattenName'                  :: Name -> Maybe Name -> Name
flattenName' (Hier (Hier a b) c) Nothing
                              =  flattenName' a (Just (flattenName' b (Just c)))
flattenName' (Hier (Hier a b) c) (Just e)
                              =  flattenName' a (Just (flattenName' b (Just (c <+ e))))
flattenName' a Nothing        =  a
flattenName' a (Just e)       =  a <+ e

resolvePoint                  :: (Int, Symbols) -> Point -> Maybe Point
resolvePoint ns (PointVar (Global name))
                              =  resolvePoint ns (PointVar name)
resolvePoint (n, _) (PointVar  (NameInt m))
                              =  Just (PointVarArray' n m)
resolvePoint (n, _) (PointVar  (NameDir d))
                              =  Just (PointPic' n d)
resolvePoint (n, _) (PointVar  (Hier (NameStr "last") (NameDir d)))
                              =  Just (PointPic' (n+1) d)
resolvePoint (_, s) (PointVar name)
                              =  resolvePointName (symPnts s) (flattenName name)
resolvePoint ns (PointTrans' p ts)
                              =  maybe' (\p->PointTrans' p ts) (resolvePoint ns p)
resolvePoint ns (PointVec (nx, ny))
                              =  maybe2 (\a b -> PointVec (a, b))
                                      (resolveNumeric ns nx, resolveNumeric ns ny)
resolvePoint ns (PointPPP c p1 p2)
                              =  maybe2 (PointPPP c)
                                      (resolvePoint ns p1, resolvePoint ns p2)
resolvePoint ns (PointMediate a p1 p2)
                              =  maybe3 PointMediate (resolveNumeric ns a,
                                                      resolvePoint ns p1,
                                                      resolvePoint ns p2)
resolvePoint ns (PointNMul a p )
                              =  maybe2 PointNMul (resolveNumeric ns a, resolvePoint ns p)
resolvePoint ns (PointNeg p)  =  maybe' PointNeg (resolvePoint ns p)
resolvePoint ns (PointDirection p)
                              =  maybe' PointDirection (resolveNumeric ns p)
resolvePoint ns (PointCond b t e)
                              =  maybe3 PointCond (   resolveBoolean ns b,
                                                      resolvePoint ns t,
                                                      resolvePoint ns e)
resolvePoint _ p              =  Just p

resolvePointName              :: SymPoint -> Name -> Maybe Point
resolvePointName (SymPUnion d1 d2) name
                              =  resolvePointName d1 name
                              `mplus` resolvePointName d2 name
resolvePointName (SymPUnion3 d1 d2 d3) name
                              =  resolvePointName d1 name
                              `mplus` resolvePointName d2 name
                              `mplus` resolvePointName d3 name
resolvePointName (SymPHier name' n _) (Hier name (NameInt m))
                              =  if name == name'
                                      then Just (PointVarArray' n m)
                                      else Nothing
resolvePointName (SymPName name' n _) (Hier name (NameDir d)) -- SymPHier
                              =  if name == name'
                                      then Just (PointPic' n d)
                                      else Nothing
resolvePointName (SymPHier name' _ ds) (Hier name p)
                              =  if name == name'
                                      then resolvePointName ds p
                                      else Nothing
resolvePointName (SymPName name' n m) name
                              =  if name == name'
                                      then Just (PointVar' n m)
                                      else Nothing
resolvePointName (SymPTrans d k) name
                              =  maybe' (f k) (resolvePointName d name)
      where
      f k (PointTrans' p2 ks) =  PointTrans' p2 (k:ks)
      f k p                   =  PointTrans' p [k]
resolvePointName _ _          =  Nothing


-------------------------------------------------------


resolveNumeric2               :: SymNum -> Name -> Maybe Numeric
resolveNumeric2 (SymNUnion n1 n2) name
                              =  resolveNumeric2 n1 name
                              `mplus` resolveNumeric2 n2 name
resolveNumeric2 (SymNUnion3 n1 n2 n3) name
                              =  resolveNumeric2 n1 name
                              `mplus` resolveNumeric2 n2 name
                              `mplus` resolveNumeric2 n3 name
resolveNumeric2 (SymNHier name' n _) (Hier name (NameInt m))
                              =  if name == name'
                                      then Just (NumericArray' n m)
                                      else Nothing
resolveNumeric2 (SymNHier name' _ ds) (Hier name n)
                              =  if name == name'
                                      then resolveNumeric2 ds n
                                      else Nothing
resolveNumeric2 (SymNName name' n m) name
                              =  if name == name'
                                      then Just (NumericVar' n m)
                                      else Nothing
resolveNumeric2 _ _           =  Nothing

maybe'                        :: (a -> b) -> Maybe a -> Maybe b
maybe' f (Just a)             =  Just (f a)
maybe' _ _                    =  Nothing

maybe2                        :: (a -> b -> c) -> (Maybe a,Maybe b) -> Maybe c
maybe2 f (Just a, Just b)     =  Just (f a b)
maybe2 _ (_, _)               =  Nothing

maybe3                        :: (a -> b -> c -> d) -> (Maybe a,Maybe b,Maybe c) -> Maybe d
maybe3 f (Just a, Just b, Just c)
                              =  Just (f a b c)
maybe3 _ (_, _, _)            =  Nothing

resolveNumeric                :: (Int, Symbols) -> Numeric -> Maybe Numeric
resolveNumeric (n,_) (NumericVar (NameInt m))
                              =  Just (NumericArray' n m)
resolveNumeric (_,s) (NumericVar name)
                              =  resolveNumeric2 (symNums s) (flattenName name)
resolveNumeric ns (NumericNNN c n1 n2)
                              =  maybe2 (NumericNNN c)
                                      (resolveNumeric ns n1,
                                      resolveNumeric ns n2)
resolveNumeric ns (NumericPN c p)
                              =  maybe' (NumericPN c) (resolvePoint ns p)
resolveNumeric ns (NumericNN c a)
                              =  maybe' (NumericNN c) (resolveNumeric ns a)
resolveNumeric ns (NumericNsN c as)
                              =  if elem Nothing as'
                                      then Nothing
                                      else Just (NumericNsN c (maybes2List as'))
                      where
                      as'     =  map (resolveNumeric ns) as
resolveNumeric ns (NumericDist p1 p2)
                              =  maybe2 NumericDist ( resolvePoint ns p1,
                                                      resolvePoint ns p2)
resolveNumeric ns (NumericMediate n1 n2 n3)
                              =  maybe3 NumericMediate (      resolveNumeric ns n1,
                                                              resolveNumeric ns n2,
                                                              resolveNumeric ns n3)
resolveNumeric ns (NumericCond b t e)
                              =  maybe3 NumericCond ( resolveBoolean ns b,
                                                      resolveNumeric ns t,
                                                      resolveNumeric ns e)
resolveNumeric _ a            =  Just a

---------------------------------------------------------------------


resolvePath                   :: (Int, Int, Symbols) -> Path -> Maybe (Int, Path)
resolvePath (_, m, _) PathCycle
                              =  Just (m, PathCycle)
resolvePath (n, m, s) (PathPoint p)
                              =  maybe' (\a->(m, PathPoint a)) (resolvePoint (n, s) p)
resolvePath (n, m, s) (PathEndDir p d)
                              =  maybe' (\a->(m, PathEndDir a (resolveDir (n, s) d)))
                                      (resolvePoint (n, s) p)
resolvePath (n, m, s) (PathJoin p1 ped p2)
                              =  case resolvePath (n, m, s) p1 of
                                      Just (_, p1')
                                              -> case resolvePath (n, m, s) p2 of
                                              Just (m2', p2')
                                                      -> Just (m2',PathJoin p1'
                                                      (resolvePathElemDescr (n, s) ped)
                                                       p2')
                                              _       -> Nothing
                                      _       -> Nothing
resolvePath (n, m, s) (PathBuildCycle p1 p2)
                              =  case resolvePath (n, m, s) p1 of
                                      Just (_, p1')
                                              -> case resolvePath (n, m, s) p2 of
                                              Just (m2', p2')
                                                      -> Just (m2', PathBuildCycle p1' p2')
                                              _       -> Nothing
                                      _       -> Nothing
resolvePath (n, m, s) (PathDefine eqs p)
                              =  case resolvePath (n, m', s1 & s) p of
                                      Just (m',p')
                                              -> Just (m', PathDefine eqs' p')
                                      _       -> Nothing
              where
              eqs'            =  maybes2List (map (resolveEquation (n, s1 & s)) eqs)
              (m',  s1)       =  symEquations (n, m, relax)  eqs
resolvePath nms (PathTransform t p)
                              =  case resolvePath nms p of
                                      Just (m',p')
                                              -> Just (m', PathTransform t p')
                                      _       -> Nothing



resolvePathElemDescr          :: (Int, Symbols) -> PathElemDescr -> PathElemDescr
resolvePathElemDescr ns@(n, s) ped
                              =  ped {peStartCut      = getCut (peStartCut ped),
                                      peEndCut        = getCut (peEndCut ped),
                                      pePen           = resolvePen ns (pePen ped),
                                      peStartDir      = resolveDir ns (peStartDir ped),
                                      peEndDir        = resolveDir ns (peEndDir ped)}
      where
      getCut Nothing          =  Nothing
      getCut (Just cut)       =  resolveCut (n,symPnts s) cut

resolveDir                    :: (Int, Symbols) -> Dir' -> Dir'
resolveDir ns (DirCurl a)     =  maybe DirEmpty DirCurl (resolveNumeric ns a)
resolveDir ns (DirDir a)      =  maybe DirEmpty DirDir (resolveNumeric ns a)
resolveDir ns (DirVector a)   =  maybe DirEmpty DirVector (resolvePoint ns a)
resolveDir _ a                =  a

resolveCut                    :: (Int, SymPoint) -> CutPic -> Maybe CutPic
resolveCut (n,_) (CutPic (NameDir _))
                              =  return (CutPic' (suff n))
resolveCut (n,SymPUnion p1 p2) c
                              =  resolveCut (n, p1) c
                              `mplus` resolveCut (n, p2) c
resolveCut (n,SymPUnion3 p1 p2 p3 ) c
                              =  resolveCut (n, p1) c
                              `mplus` resolveCut (n, p2) c
                              `mplus` resolveCut (n, p3) c
resolveCut (n,SymPHier name' _ ds) (CutPic (Hier name name2))
                              =  if name == name'
                                      then resolveCut (n, ds) (CutPic name2)
                                      else Nothing
resolveCut (_,SymPName name' n _) (CutPic name)
                              =  if name == name'
                                      then return (CutPic' (suff n))
                                      else Nothing
resolveCut (n,SymPTrans d k) c=  maybe' (f k) (resolveCut (n, d) c)
      where
      f k (CutPicTrans p2 ks) =  CutPicTrans p2 (k:ks)
      f k p                   =  CutPicTrans p [k]
resolveCut _ _                =  Nothing


resolveJoin                   :: (Int, Symbols) -> BasicJoin -> BasicJoin
resolveJoin ns (BJTension a)  =  maybe BJBounded BJTension (resolveTension ns a)
resolveJoin ns (BJTension2 a b )
                              =  case (resolveTension ns a, resolveTension ns b) of
                                      (Nothing, Nothing)      -> BJBounded
                                      (Just a', Nothing)      -> BJTension a'
                                      (Nothing, Just a')      -> BJTension a'
                                      (Just a', Just b')      -> BJTension2 a' b'
resolveJoin ns (BJControls a) =  maybe BJBounded BJControls (resolvePoint ns a)
resolveJoin ns (BJControls2 a b)
                              =  case (resolvePoint ns a, resolvePoint ns b) of
                                      (Nothing, Nothing)      -> BJBounded
                                      (Just a', Nothing)      -> BJControls a'
                                      (Nothing, Just a')      -> BJControls a'
                                      (Just a', Just b')      -> BJControls2 a' b'
resolveJoin _ a               =  a


resolveTension                :: (Int, Symbols) -> Tension -> Maybe Tension
resolveTension ns (Tension a) =  maybe' Tension (resolveNumeric ns a)
resolveTension ns (TensionAtLeast a)
                              =  maybe' TensionAtLeast (resolveNumeric ns a)

resolvePen                    :: (Int, Symbols) -> Pen -> Pen
resolvePen ns (PenCircle (x, y ) a)
                              =  PenCircle (getDefault (resolveNumeric ns x) 1,
                                      getDefault (resolveNumeric ns y) 1)
                                      (getDefault (resolveNumeric ns a) 0)
resolvePen ns (PenSquare (x, y ) a)
                              =  PenSquare (getDefault (resolveNumeric ns x) 1,
                                      getDefault (resolveNumeric ns y) 1)
                                      (getDefault (resolveNumeric ns a) 0)
resolvePen _ a                =  a

-------------------------------------------------------------------

resolveEquation               :: (Int, Symbols) -> Equation -> Maybe Equation
resolveEquation ns (PEquations ps)
                              =  case ps' of
                                      []      -> Nothing
                                      [_]     -> Nothing
                                      a       -> Just (PEquations a)
                      where
                      ps'     =  maybes2List (map (resolvePoint ns) ps)
resolveEquation ns (NEquations nums)
                              =  case nums'' of
                                      []      -> Nothing
                                      [_]     -> Nothing
                                      a       -> Just (NEquations a)
                      where
                      nums'   =  map (resolveNumeric ns) nums
                      nums''  =  maybes2List nums'
resolveEquation ns (EquationCond b e1 e2)
                              =  maybe3 EquationCond (resolveBoolean ns b,
                                                      resolveEquation ns e1,
                                                      resolveEquation ns e2)
resolveEquation ns (Equations es)
                              =  if null es'
                                      then Nothing
                                      else Just (Equations es')
      where
      es'                     =  maybes2List (map (resolveEquation ns) es)


-------------------------------------------------------------------

resolveBoolean                :: (Int, Symbols) -> Boolean -> Maybe Boolean
resolveBoolean _ (Boolean a)  =  Just (Boolean a)
resolveBoolean ns (BoolNum a c b)
                              =  maybe2 (\a b -> BoolNum a c b)
                                      (resolveNumeric ns a, resolveNumeric ns b)
resolveBoolean ns (BoolPnt a c b)
                              =  maybe2 (\a b -> BoolPnt a c b)
                                      (resolvePoint ns a, resolvePoint ns b)
resolveBoolean ns (BoolOr a b)=  maybe2 BoolOr (resolveBoolean ns a, resolveBoolean ns b)
resolveBoolean ns (BoolAnd a b )
                              =  maybe2 BoolAnd (resolveBoolean ns a, resolveBoolean ns b)
resolveBoolean ns (BoolNot a) =  maybe' BoolNot (resolveBoolean ns a)

-- Suche neue Symbole in Ausdr"ucken und f"uge sie zu den bisherigen hinzu.

symEquations                  :: (Int, Int, Symbols) -> [Equation] -> (Int, Symbols)
symEquations (_, m, s) []     =  (m, s)
symEquations (n, m, s) (eq : eqs)
                              =  symEquations (n, m', s') eqs
              where
              (m',  s')       =  symEquation  (n, m,  s) eq

symEquation                   :: (Int, Int, Symbols) -> Equation -> (Int, Symbols)
symEquation (n, m, s) (PEquations ps)
                              =  symPoints (n, m, s) ps
      where
      symPoints (_, m, s) []  =  (m, s)
      symPoints (n, m, s) (p : ps)
                              =  symPoints (n, m', s') ps
              where
              (m',  s')       =  symPoint (n, m,  s)  p
symEquation (n, m, s) (NEquations ns)
                              =  symNumerics (n, m, s) ns
      where
      symNumerics (_, m, s) []
                              =  (m, s)
      symNumerics (n, m, s) (num:ns)
                              =  symNumerics (n, m', s') ns
              where
              (m',  s')       =  symNumeric (n, m,  s) num
symEquation (n, m, s) (EquationCond b e1 e2)
                              =  symEquation (n, m'', s'') e2
              where
              (m',   s')      =  symBoolean (n, m, s) b
              (m'',  s'')     =  symEquation (n, m',  s') e1
symEquation (n, m, s) (Equations es)
                              =  symEquations (n, m,  s) es

symPoint                      :: (Int, Int, Symbols) -> Point -> (Int, Symbols)
symPoint (_, m, s) (PointVar (Global _))
                              =  (m, s)
symPoint nms (PointVar name)
                              =  insertPntName nms name
symPoint (n, m, s) (PointPPP _ a b)
                              =  symPoint (n, m', s') b
      where   (m',  s')       =  symPoint (n, m,  s)  a
symPoint (n, m, s) (PointVec (a, b))
                              =  symNumeric (n, m', s') b
      where   (m',  s')       =  symNumeric (n, m,  s)  a
symPoint (n, m, s) (PointMediate a b c)
                              =  symPoint   (n, m'', s'') c
      where   (m',   s')      =  symNumeric (n, m,   s)  a
              (m'',  s'')     =  symPoint   (n, m',  s') b
symPoint (n, m, s) (PointNMul a b)
                              =  symPoint   (n, m',  s') b
      where   (m',   s')      =  symNumeric (n, m,   s)  a
symPoint nms(PointDirection a)
                              =  symNumeric nms a
symPoint nms (PointNeg a)     =  symPoint nms a
symPoint (n, m, s) (PointCond b t e)
                              =  symPoint   (n, m'', s'') e
      where   (m',   s')      =  symBoolean (n, m,   s)  b
              (m'',  s'')     =  symPoint   (n, m',  s') t
symPoint (_, m, s) _          =  (m, s)


symNumeric                    :: (Int,Int,Symbols) -> Numeric -> (Int,Symbols)
symNumeric (_, m, s) (NumericVar (Global _))
                              =  (m, s)
symNumeric (n, m, s) (NumericVar name)
                              =  insertNumeric (n, m, s) name
symNumeric (n, m, s) (NumericNN _ a)
                              =  symNumeric (n, m, s) a
symNumeric (n, m, s) (NumericPN _ a)
                              =  symPoint (n, m, s) a
symNumeric (n, m, s) (NumericNNN _ a b)
                              =  symNumeric (n, m', s') b
              where
              (m',  s')       =  symNumeric (n, m,  s) a
symNumeric (n, m, s) (NumericDist a b)
                              =  symPoint (n, m', s') b
              where
              (m',  s')       =  symPoint (n, m,  s) a
symNumeric (n, m, s) (NumericMediate a b c)
                              =  symNumeric (n, m'', s'') c
              where
              (m',   s')      =  symNumeric (n, m,   s) a
              (m'',  s'')     =  symNumeric (n, m',  s') b
symNumeric (n, m, s) (NumericNsN _ as)
                              =  symNumerics (n, m, s) as
      where
      symNumerics (_,m ,s) [] =  (m, s)
      symNumerics (n,m ,s) (a:as)
                              =  symNumerics (n, m', s') as
              where
              (m',s')         =  symNumeric (n, m, s) a
symNumeric (n, m, s) (NumericCond b t e)
                              =  symNumeric (n, m'', s'') e
              where
              (m',   s')      =  symBoolean (n, m,   s)  b
              (m'',  s'')     =  symNumeric (n, m',  s') t
symNumeric (_, m, s) _= (m, s)


symBoolean                    :: (Int,Int,Symbols) -> Boolean -> (Int,Symbols)
symBoolean (n, m, s) (BoolNum a _ b)
                              =  symNumeric (n, m', s') b
              where
              (m',  s')       =  symNumeric (n, m, s) a
symBoolean (n, m, s) (BoolPnt a _ b)
                              =  symPoint (n, m', s') b
              where
              (m',  s')       =  symPoint (n, m, s) a
symBoolean (n, m, s) (BoolOr a b)
                              =  symBoolean (n, m', s') b
              where
              (m',  s')       =  symBoolean (n, m, s) a
symBoolean (n, m, s) (BoolAnd a b)
                              =  symBoolean (n, m', s') b
              where
              (m',  s')       =  symBoolean (n, m, s) a
symBoolean (n, m, s) (BoolNot a)
                              =  symBoolean (n, m, s) a
symBoolean (_, m, _) _        =  (m, relax)


insertNumeric                 :: (Int,Int,Symbols) -> Name -> (Int,Symbols)
insertNumeric (n, m, s) name  =  if resolveNumeric (n,s) (NumericVar name)
                                      == Nothing
                                      then (m+1, addNDef (SymNName name n m) s)
                                      else (m, s)

insertPntName                 :: (Int,Int,Symbols) -> Name -> (Int,Symbols)
insertPntName (n, m, s) name  =  if not (lastNameIsDir name)
                                 && resolvePoint (n,s) (PointVar name) == Nothing
                                      then (m+1, addPDef (SymPName name n m) s)
                                      else (m, s)
