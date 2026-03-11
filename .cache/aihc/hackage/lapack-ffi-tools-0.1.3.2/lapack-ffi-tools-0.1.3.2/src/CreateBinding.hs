module CreateBinding where

import qualified Parser.Signature as Parser
import qualified Type
import Parser.Signature (Identifier)

import qualified Text.ParserCombinators.Parsec as Parsec

import qualified Control.Monad.Exception.Synchronous as ME
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Functor.HT as Func
import Control.Monad (when)

import qualified Data.Foldable as Fold
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Traversable (forM)
import Data.Tuple.HT (mapPair, mapFst, mapSnd, mapSnd3, fst3, snd3, thd3)
import Data.Map (Map)
import Data.Set (Set)
import Data.Char (toLower, isSpace, isAlpha, isAlphaNum)
import Data.Maybe (fromMaybe, mapMaybe)

import Text.Printf (printf)


type MonoMapping = Type.Mapping String Type.Mono
data Mapping = Mapping (Maybe [String]) Bool
type RawParameter =
      (String, (Type.Mono, Mapping),
       Maybe (Parser.Direction, Type.Mono, Maybe MonoMapping))
type Parameter = (String, (Type.Mono, MonoMapping, Parser.Direction))

examineParamInfo ::
   ((Identifier, [String], Maybe Type.Mono), [Parser.ParamDecl],
    Map String (Parser.Direction, Type.Mono, Maybe MonoMapping)) ->
   ME.Exceptional String (Identifier, [RawParameter], Maybe Type.Mono)
examineParamInfo ((name, paramNames, returnType), paramDecls, paramDirs) =
   let (paramTypes,externals) =
         ListHT.unzipEithers $
         map
            (\decl ->
               case decl of
                  Parser.Type typ ps -> Left (typ,ps)
                  Parser.External ps -> Right ps)
            paramDecls
       externalSet :: Set String
       externalSet = Fold.foldMap Set.fromList externals
       typeMap =
         Map.fromList $
         concatMap
            (\(typ,params) -> map (\(param,dims) -> (param,(typ,dims))) params)
            paramTypes
       mapp pm param = Mapping pm $ Set.member param externalSet
   in  fmap (\params -> (name, params, returnType)) $
       forM paramNames $ \param ->
         ME.fromMaybe ("no type declaration found for " ++ param) $
         fmap
            (\(typ,pm) ->
               (param, (typ, mapp pm param), Map.lookup param paramDirs)) $
         Map.lookup param typeMap

fallbackMapping :: Mapping -> MonoMapping
fallbackMapping pm =
   case pm of
      Mapping Nothing False -> Type.Scalar
      Mapping (Just dims) False -> Type.Array dims
      Mapping _ True -> Type.Function 0 Type.Integer

parameterFromRaw :: RawParameter -> Parameter
parameterFromRaw (name, (typ,dims), mdir) =
   (name,
    case mdir of
      Nothing -> (typ, fallbackMapping dims, Parser.Input)
      Just (dir, _typ, cmtDims) ->
         (typ, fromMaybe (fallbackMapping dims) cmtDims, dir))

compatibleDimension :: String -> String -> Bool
compatibleDimension x y  =  x == ("0:"++y++"-1")  ||  x==y

compatibleDimensions :: [String] -> [String] -> Bool
compatibleDimensions =
   let go [] [] = True
       go ["*"] [_] = True
       go ["0:*"] [_] = True
       go (x:xs) (y:ys) = compatibleDimension x y && go xs ys
       go [] _ = False
       go _ [] = False
       trim = filter (not . isSpace)
   in  \xs ys -> go (map trim xs) (map trim ys)

compatibleMapping :: Mapping -> MonoMapping -> Bool
compatibleMapping (Mapping Nothing False) Type.Scalar = True
compatibleMapping (Mapping (Just dims0) False) (Type.Array dims1) =
   compatibleDimensions dims0 dims1
compatibleMapping (Mapping Nothing True) (Type.Function _ _) = True
compatibleMapping _ _ = False

combineParamInfo :: [RawParameter] -> MW.Writer [String] [Parameter]
combineParamInfo params = do
   let (completeParams, incompleteParams) =
         ListHT.partitionMaybe (Func.mapThd3 id) params
   when (not $ null incompleteParams) $ MW.tell $ (:[]) $
      "parameters without or with ill-formatted details: " ++
      List.intercalate ", " (map fst3 incompleteParams)
   let inconsistentTypes =
         filter (\(_name, typ, cmt) -> fst typ /= snd3 cmt) completeParams
   when (not $ null inconsistentTypes) $ MW.tell $ (:[]) $
      "parameters with inconsistent type definitions: " ++
      List.intercalate ", " (map fst3 inconsistentTypes)
   let inconsistentDims =
         filter
            (\(_name, typ, cmt) ->
               Fold.any
                  (\cmtDims -> not $ compatibleMapping (snd typ) cmtDims)
                  (thd3 cmt))
            completeParams
   when (not $ null inconsistentDims) $ MW.tell $ (:[]) $
      "parameters with inconsistent dimension lists: " ++
      List.intercalate ", " (map fst3 inconsistentDims)
   return $ map parameterFromRaw params

parseHeader ::
   Parsec.Parser (MW.Writer [String] (Identifier, [Parameter], Maybe Type.Mono))
parseHeader =
   fmap (Func.mapSnd3 combineParamInfo) $
   ME.switch fail return . examineParamInfo =<< Parser.header3_4


foreignFromParameters ::
   [(name, (Type.Mono, Type.Mapping dims Type.Mono, dir))] ->
   Maybe Type.Mono -> Type.Foreign Type.Mono
foreignFromParameters params returnType =
   Type.Foreign
      (map
         (\(_name, (typ, mapping, _dir)) ->
            case mapping of
               Type.Scalar -> Type.Ptr typ
               Type.Array _ -> Type.Ptr typ
               Type.Function n param -> Type.FunPtr n param typ)
         params)
      returnType

formatForeignCall :: ((String, String), Type.Foreign Type.Mono) -> String
formatForeignCall ((haskellName, fortranName), foreignSig) =
   printf "foreign import ccall \"%s_\"\n" (map toLower fortranName) ++
   printf "   %s :: %s\n" haskellName (Type.formatForeign foreignSig)


contFromType :: Type.Mono -> String
contFromType typ =
   case typ of
      Type.Logical -> "Call.bool"
      Type.Character -> "Call.char"
      Type.Integer -> "Call.cint"
      Type.RealSingle -> "Call.float"
      Type.RealDouble -> "Call.double"
      Type.ComplexSingle -> "Call.complexFloat"
      Type.ComplexDouble -> "Call.complexDouble"

isIOArray :: Parser.Direction -> Bool
isIOArray dir =
   case dir of
      Parser.InputOutput -> True
      Parser.InputWorkspace -> True
      _ -> False

isTmpIOArray :: Parser.Direction -> Bool
isTmpIOArray dir =
   case dir of
      Parser.Input -> False
      _ -> True

contFromArrayType :: Parameter -> String
contFromArrayType (name, (typ, dims, dir)) =
   case dims of
      Type.Scalar ->
         if isInputDir dir
            then contFromType typ ++ " " ++ name
            else "Call.alloca"
      Type.Array _ ->
         (if isTmpIOArray dir then "Call.ioarray " else "Call.array ") ++ name
      Type.Function _ _ -> "pure " ++ name

parens :: String -> String
parens expr = "("++expr++")"

parensIfNeeded :: String -> String
parensIfNeeded expr = if all isAlphaNum expr then expr else parens expr

isInputDir :: Parser.Direction -> Bool
isInputDir dir =
   case dir of
      Parser.Input -> True
      Parser.InputOutput -> True
      Parser.InputWorkspace -> True
      Parser.Output -> False
      Parser.Workspace -> False

isOutput :: (Type.Mono, Type.Mapping dim Type.Mono, Parser.Direction) -> Bool
isOutput (_, dims, dir) =
   case dir of
      Parser.Input -> False
      Parser.InputOutput -> Type.isScalar dims
      Parser.InputWorkspace -> False
      Parser.Output -> True
      Parser.Workspace -> False


parameterName :: String -> String
parameterName fortranName =
   case map toLower fortranName of
      "id" -> "id_"
      "in" -> "in_"
      "sin" -> "sin_"
      "type" -> "type_"
      haskellName -> haskellName

spanIdentifier :: String -> (String, String)
spanIdentifier str =
   case str of
      [] -> ([], str)
      c:cs ->
         if isAlpha c
           then mapFst (c:) $ span isAlphaNum cs
           else ([], str)

isIdentifier :: String -> Bool
isIdentifier str =
   case str of
      [] -> False
      c:cs -> isAlpha c && all isAlphaNum cs

sizeExpression :: String -> String -> (String, [String])
sizeExpression name =
   let c <: ~(strRem,vars) = (c:strRem, vars)
       str <++ ~(strRem,vars) = (str++strRem, vars)
       addVar var str = mapSnd (var:) $ var <++ str
       go paren str =
         case spanIdentifier str of
            ("", cs) ->
               case cs of
                  "" ->
                     (if null paren then "" else "{- too many closing parens -}",
                      [])
                  ' ':cs1 -> go paren cs1
                  '/':cs1 -> "`div`" <++ go paren cs1
                  '*':'*':cs1 -> "^!" <++ go paren cs1
                  '(':cs1 -> '(' <: go (')':paren) cs1
                  ')':cs1 ->
                     case paren of
                        p:ps -> p <: go ps cs1
                        [] -> " {- missing closing -} )" <++ go paren cs1
                  c:cs1 -> c <: go paren cs1
            ("MAX", '(':cs) -> "maximum[" <++ go (']':paren) cs
            ("max", '(':cs) -> "maximum[" <++ go (']':paren) cs
            ("MIN", '(':cs) -> "minimum[" <++ go (']':paren) cs
            ("min", '(':cs) -> "minimum[" <++ go (']':paren) cs
            ("ABS", '(':cs) -> "abs(" <++ go (')':paren) cs
            ("abs", '(':cs) -> "abs(" <++ go (')':paren) cs
            ("N", ' ':'l':'g':' ':'N':cs) -> addVar "nlgn" (go paren cs)
            (ident, cs) -> addVar (parameterName ident) (go paren cs)
       sizeName = name ++ "Size"
   in  \str -> if str=="*" then (sizeName, [sizeName]) else go [] str

-- like nubBy but needs (n * log n) operations instead of (n^2)
ordnub :: (Ord a) => Set a -> [(a,b)] -> [(a,b)]
ordnub initSet xs =
   map fst $ filter snd $ zip xs $ zipWith (Set.notMember . fst) xs $
   scanl (flip Set.insert) initSet $ map fst xs


peekOut :: Parameter -> String
peekOut (name, (typ, dims, dir)) =
   case dims of
      Type.Scalar ->
         case typ of
            Type.Character -> printf "fmap castCCharToChar (peek %sPtr)" name
            Type.Integer -> printf "fmap fromIntegral (peek %sPtr)" name
            _ -> printf "peek %sPtr" name
      Type.Array _ ->
         if isIOArray dir
            then "pure " ++ name
            else "Call.freezeArray " ++ name
      Type.Function _ _ ->
         printf "error \"function %s cannot be an output\"" name

formatOutputTuple ::
   (Type.Array array, Type.Format typ) =>
   array -> Type.Wrapper array typ -> String
formatOutputTuple array (Type.Wrapper _inputs returnType outputs) =
   let outputTuple =
         CreateBinding.parens $ List.intercalate ", " $
         map (uncurry $ Type.formatArray array False) outputs
   in case (returnType, outputs) of
         (Nothing, _) -> outputTuple
         (Just t, []) -> Type.format t
         -- I have not encountered this case so far
         (Just t, _) -> printf "(%s, %s)" (Type.format t) outputTuple


formatWrapper ::
   (Type.Array array) =>
   array ->
   (String, [Parameter], Maybe Type.Mono) ->
   (String, Type.Wrapper array Type.Mono)
formatWrapper array (name, params0, returnType) =
   let prepare (param,info) =
         (param,
          mapSnd3
            (Type.mapMappings (map (sizeExpression param) . reverse)) info)
       pparams = map (prepare . mapFst parameterName) params0
       params = map (mapSnd (mapSnd3 (Type.mapMapping fst))) pparams
       (inputArrays,allocArrays) =
         ListHT.partition (isInputDir . snd . snd) $
         mapMaybe
            (\(param,(_typ,mapping,dir)) -> do
               dimVars <- Type.maybeArray mapping
               return (param,(dimVars,dir)))
            pparams
       dimNames param = map (printf "%sDim%d" param) [(0::Int)..]
       allDimVars =
         concatMap
            (\(param,(dimVars,_dir)) -> zip dimVars $ dimNames param)
            inputArrays
       inputsDup =
         concatMap
            (\(param, (typ,mapping,dir)) ->
               let (dims,vars) =
                     case mapping of
                        Type.Scalar -> (Type.Scalar, [])
                        Type.Function n paramType ->
                           (Type.Function n paramType, [])
                        Type.Array dimVars ->
                           mapPair (Type.Array, concat) $ Func.unzip dimVars
               in  if isInputDir dir
                     then [(param, (typ,dims,dir))]
                     else map (flip (,) (Type.Integer,Type.Scalar,Parser.Input)) vars)
            pparams
       inputVars =
         Set.fromList $
            map fst inputsDup ++
            filter isIdentifier (map (fst . fst) allDimVars)
       (varSet, (assigns,checks)) =
         mapSnd (mapPair (map snd, map snd) . ListHT.partition fst) $
         List.mapAccumL
            (\set rel@((dim,_vars),_dimId) ->
               let isIdent = isIdentifier dim
               in  (if isIdent then Set.insert dim set else set,
                    (isIdent && Set.notMember dim set, rel)))
            Set.empty
            allDimVars
       inputs = ordnub varSet inputsDup
       usedVars =
         Set.fromList $
            concatMap
               (\((dim,vars),_dimId) ->
                  if isIdentifier dim then dim:vars else vars)
               checks
            ++
            map fst params
            ++
            concatMap
               (\(_param,(dimVars,_dir)) -> concatMap snd dimVars)
               allocArrays
       outputs = filter (isOutput . snd) params
       wrapper =
         Type.Wrapper
            (map
               (\(_param,(typ,dims,dir)) -> (typ, dims, isIOArray dir))
               inputs)
            returnType
            (map (\(_param,(typ,dims,_dir)) -> (typ,dims)) outputs)
   in flip (,) wrapper $
      unlines $
      printf "%s ::" name :
      map
         (\(param,(typ,dims,dir)) ->
            printf "   %s {- ^ %s -} ->"
               (Type.formatArray array (isIOArray dir) typ dims) param)
         inputs ++
      ("   IO " ++ formatOutputTuple array wrapper) :
      (name ++ concatMap ((' ':) . fst) inputs ++ " = do") :
      map
         (\(param,(dimVars,dir)) ->
            let format =
                  Type.caseComfortCArray array
                     (if isIOArray dir
                        then "   let %s = Call.sizes%d $ MutArray.shape %s"
                        else "   let %s = Call.sizes%d $ Array.shape %s")
                     (if isIOArray dir
                        then "   %s <- Call.sizes%d <$> getBounds %s"
                        else "   let %s = Call.sizes%d $ bounds %s")
                dimIdents = Match.take dimVars $ dimNames param
            in  printf format
                  (Type.formatTuple dimIdents) (length dimIdents) param)
         inputArrays ++
      map
         (\((dim,_),dimId) ->
            let underscore = if Set.member dim usedVars then "" else "_"
            in  printf "   let %s%s = %s" underscore dim dimId)
         assigns ++
      map
         (\((dim,vars),dimId) ->
            if Set.isSubsetOf (Set.fromList vars) inputVars
               then
                  printf "   Call.assert \"%s: %s == %s\" (%s == %s)"
                     name dim dimId dim dimId
               else
                  printf "   Call.ignore \"%s: %s == %s\" %s"
                     name dim dimId (dimId::String))
         checks ++
      map
         (\(param,(dims,_dir)) ->
            printf "   %s <- Call.newArray%d%s"
               param (length dims)
               (concatMap ((' ':) . parensIfNeeded . fst) dims))
         allocArrays ++
      "   evalContT $ do" :
      map
         (\param ->
            printf "      %sPtr <- %s" (fst param) (contFromArrayType param))
         params ++
      ("      liftIO $ FFI." ++ name ++
         concatMap (printf " %sPtr" . fst) params) :
      (case outputs of
         [] -> []
         [param] -> ["      liftIO $ " ++ peekOut param]
         _:os ->
            printf "      liftIO $ pure (%s)" (Match.replicate os ',')
            :
            map (("         <*> " ++) . peekOut) outputs) ++
      []
