{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Yesod.Routes.Flow.Generator
  ( genFlowRoutes
  , genFlowRoutesPrefix
  , genFlowSource
  , genFlowClasses
  , classesToFlow
  , Class(..)
  , ClassMember(..)
  , RenderedPiece(..)
  , PieceType(..)
  ) where

import ClassyPrelude hiding (FilePath)
import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as Map
import Data.Text (dropWhileEnd)
import qualified Data.Text as T
import Filesystem (createTree, writeTextFile)
import Filesystem.Path (FilePath, directory)
import Yesod.Routes.TH.Types

-- An override map from Haskell type name to Flow type name
type Overrides = Map.Map String PieceType

genFlowRoutes :: [ResourceTree String] -> FilePath -> IO ()
genFlowRoutes ra fp = genFlowRoutesPrefix Map.empty [] [] ra fp "''"

genFlowRoutesPrefix :: Overrides -> [String] -> [String] -> [ResourceTree String] -> FilePath -> Text -> IO ()
genFlowRoutesPrefix overrides routePrefixes elidedPrefixes fullTree fp prefix = do
    createTree $ directory fp
    writeTextFile fp $ genFlowSource overrides routePrefixes elidedPrefixes prefix fullTree

genFlowSource :: Overrides -> [String] -> [String] -> Text -> [ResourceTree String] -> Text
genFlowSource overrides routePrefixes elidedPrefixes prefix fullTree =
  mconcat
    [ "/* @flow */\n\n"
    , classesToFlow $ genFlowClasses overrides routePrefixes elidedPrefixes fullTree
    , "\n\nvar PATHS: PATHS_TYPE_paths = new PATHS_TYPE_paths(" <> prefix <> ");\n"
    ]

genFlowClasses :: Overrides -> [String] -> [String] -> [ResourceTree String] -> [Class]
genFlowClasses overrides routePrefixes elidedPrefixes fullTree =
  map disambiguateFields $
  resourceTreeToClasses overrides elidedPrefixes $
  ResourceParent "paths" False [] hackedTree
 where
  -- Route hackery.
  landingRoutes = flip filter fullTree $ \case
      ResourceParent {} -> False
      ResourceLeaf res  -> notElem (resourceName res) ["AuthR", "StaticR"]
  parents =
      -- if routePrefixes is empty, include all routes
      filter (\n -> null routePrefixes || any (parentName n) routePrefixes) fullTree
  hackedTree = ResourceParent "staticPages" False [] landingRoutes : parents


parentName :: ResourceTree String -> String -> Bool
parentName (ResourceParent n _ _ _) name = n == name
parentName _ _                           = False

----------------------------------------------------------------------

data RenderedPiece
  = Path Text
  | Dyn PieceType
    deriving (Eq, Show)

data PieceType
  = NumberT
  | StringT
  | NonEmptyT PieceType
    deriving (Eq, Show)

isVariable :: RenderedPiece -> Bool
isVariable (Path _) = False
isVariable (Dyn _)  = True

renderRoutePieces :: Overrides -> [Piece String] -> [RenderedPiece]
renderRoutePieces overrides = map renderRoutePiece
  where
    renderRoutePiece (Static st)   = Path $ T.dropAround (== '/') $ pack st
    renderRoutePiece (Dynamic typ) = Dyn $ parseType typ

    parseType type_ =
      fromMaybe
        (maybe
          (parseSimpleType type_)
          (NonEmptyT . parseType)
          (L.stripPrefix "NonEmpty" type_)) -- NonEmptyUserId ~ NonEmpty UserId
        $ Map.lookup type_ overrides

    parseSimpleType "Int" = NumberT
    parseSimpleType type_
      | "Id" `isSuffixOf` type_ = NumberT -- UserId, PageId, PostId, etc.
      | otherwise = StringT

----------------------------------------------------------------------

-- | A Flow class that will be generated.
data Class =
  Class
    { className    :: Text
    , classMembers :: [ClassMember]
    }
  deriving (Eq, Show)

data ClassMember =
    -- | A 'ResourceParent' inside the 'ResourceParent'
    -- that generated this class.
    ChildClass
      { cmField     :: Text            -- ^ Field name used to refer to the child class.
      , cmClassName :: Text            -- ^ Class name of the child class.
      }
    -- | A callable method.
  | Method
      { cmField  :: Text            -- ^ Field name used to refer to the method.
      , cmPieces :: [RenderedPiece] -- ^ Pieces to render the route.
      }
    deriving (Eq, Show)

variableCount :: ClassMember -> Int
variableCount ChildClass {} = 0
variableCount Method {..}   = length (filter isVariable cmPieces)

variableNames :: [Text]
variableNames = T.cons <$> ['a'..'z'] <*> ("" : variableNames)

----------------------------------------------------------------------

-- | Create a list of 'Class'es from a 'ResourceTree'.
resourceTreeToClasses :: Overrides -> [String] -> ResourceTree String -> [Class]
resourceTreeToClasses overrides elidedPrefixes = finish . go Nothing []
  where
    finish (Right (_, classes)) = classes
    finish (Left _)             = []

    go :: Maybe Text -> [RenderedPiece] -> ResourceTree String -> Either (Maybe ClassMember) ([ClassMember], [Class])
    go _parent routePrefix (ResourceLeaf res) =
      Left $ do
        Methods _ methods <- pure $ resourceDispatch res -- Ignore subsites.
        guard (not $ null methods) -- Silently ignore routes without methods.
        let resName  = T.replace "." "" $ T.replace "-" "_" fullName
            fullName = intercalate "_" [pack st :: Text | Static st <- resourcePieces res]
        pure Method
          { cmField       = if null fullName then "_" else resName
          , cmPieces      = routePrefix <> renderRoutePieces overrides (resourcePieces res) }
    go parent routePrefix (ResourceParent name _ pieces children) =
      let elideThisPrefix = name `elem` elidedPrefixes
          pref            = cleanName $ pack name
          jsName          = maybe "" (<> "_") parent <> pref
          newParent       = if elideThisPrefix then parent else Just jsName
          newRoutePrefix  = routePrefix <> renderRoutePieces overrides pieces
          membersMethods  = catMaybes childrenMethods
          (childrenMethods, childrenClasses) = partitionEithers $ map (go newParent newRoutePrefix) children
          (membersClasses, moreClasses)      = concat *** concat $ unzip childrenClasses
      in Right $
           if elideThisPrefix
           then (membersClasses, moreClasses)
           else
             let ourClass =
                   Class
                     { className    = "PATHS_TYPE_" <> jsName
                     , classMembers = membersClasses ++ membersMethods }
                 ourReference =
                   ChildClass
                     { cmClassName  = className ourClass
                     , cmField      = pref }
             in ([ourReference], ourClass : moreClasses)

cleanName :: Text -> Text
cleanName = underscorize . uncapitalize . dropWhileEnd C.isUpper
  where uncapitalize t = toLower (take 1 t) <> drop 1 t
        underscorize = T.pack . go . T.unpack
          where go (c:cs) | C.isUpper c = '_' : C.toLower c : go cs
                          | otherwise   =  c                : go cs
                go [] = []

----------------------------------------------------------------------

-- | Disambiguate fields by appending suffixes.
disambiguateFields :: Class -> Class
disambiguateFields klass = klass { classMembers = processMembers $ classMembers klass }
  where
    processMembers = fromMap . disambiguate viaLetters . disambiguate viaArgCount . toMap
    fromMap  = concat . Map.elems
    toMap    = Map.fromListWith (++) . labelled
    labelled = map (cmField &&& pure)
    append t cm = cm { cmField = cmField cm <> t cm }

    disambiguate :: ([ClassMember] -> [ClassMember]) -> Map.Map Text [ClassMember] -> Map.Map Text [ClassMember]
    disambiguate inner = Map.fromListWith (++) . concatMap f . Map.toList
      where
        f :: (Text, [ClassMember]) -> [(Text, [ClassMember])]
        f y@(_, [ ]) = [y]
        f y@(_, [_]) = [y]
        f   (_, xs ) = labelled $ inner xs

    -- Append the number of arguments.
    viaArgCount = map $ append (T.pack . show . variableCount)

    -- Append arbitrary letters as a last resort.
    viaLetters  = zipWith (append . const) variableNames

----------------------------------------------------------------------

classMemberToFlowDef :: ClassMember -> Text
classMemberToFlowDef ChildClass {..} = "  " <> cmField <> " : " <> cmClassName <> ";\n"
classMemberToFlowDef Method {..}     = "  " <> cmField <> "(" <> args <> "): string { " <> body <> "; }\n"
  where
    args = intercalate ", " $ zipWith render variableNames $ mapMaybe getType cmPieces
      where
        render name typ = name <> ": " <> argType typ

        getType (Path _) = Nothing
        getType (Dyn t)  = Just t

        argType NumberT       = "number"
        argType StringT       = "string"
        argType (NonEmptyT t) = "Array<" <> argType t <> ">"

    body = "return this.root + '" <> routeStr variableNames cmPieces <> "'"
      where
        routeStr vars     (Path p:rest) = (if null p then "" else "/" <> p) <> routeStr vars rest
        routeStr (v:vars) (Dyn t:rest)  = "/' + " <> convert v 0 t <> " + '" <> routeStr vars rest
        routeStr _         _            = ""

        convert v i StringT = name v i
        convert v i NumberT = name v i <> ".toString()"
        convert v i (NonEmptyT t) =
          T.concat
            [ name v i
            , ".map(function("
            , name v (i + 1)
            , ") { return "
            , convert v (i + 1) t
            , " }).join(',')"
            ]

        name :: Text -> Int -> Text
        name v 0 = v
        name v i = v <> pack (show i)


classMemberToFlowInit :: ClassMember -> Text
classMemberToFlowInit ChildClass {..} = "    this." <> cmField <> " = new " <> cmClassName <> "(root);\n"
classMemberToFlowInit Method {}       = ""

classToFlow :: Class -> Text
classToFlow Class {..} =
    "class " <> className <> " {\n"
  <> concat (classMemberToFlowDef <$> classMembers)
  <> "\n"
  <> "  root: string;\n"
  <> "  constructor(root: string) {\n"
  <> "    this.root = root;\n"
  <> concat (classMemberToFlowInit <$> classMembers)
  <> "  }\n"
  <> "}\n"

classesToFlow :: [Class] -> Text
classesToFlow = intercalate "\n" . map classToFlow

#if !MIN_VERSION_yesod_core(1, 6, 2)
deriving instance (Show a) => Show (ResourceTree a)
deriving instance (Show a) => Show (FlatResource a)
#endif
