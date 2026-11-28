{-# LANGUAGE OverloadedStrings #-}

-- | The requests that a splice sends to the compiler.
--
-- A splice runs in a WebAssembly instance. For an operation that needs the
-- compiler, the module @Aihc.TH.Splice@ of the @template-haskell@ package
-- sends a request and waits for the reply. A request is a tree of
-- "Aihc.Th.Wire" with the constructor names of the request type of that
-- module.
module Aihc.Th.Request
  ( Request (..),
    RequestName (..),
    requestFromTree,
    lookupNameReply,
    extensionsReply,
    reportedFailure,
  )
where

import Aihc.Parser.Syntax (Extension (..), Name, extensionName)
import Aihc.Resolve.Types (GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Th.Convert (nameFromTree)
import Aihc.Th.Wire (Tree (..))
import Data.Char (isUpper)
import Data.Text (Text)
import Data.Text qualified as T

-- | What a splice asks the compiler to do.
data Request
  = -- | Report an error ('True') or a warning ('False') with a message.
    RequestReport !Bool !Text
  | -- | Give the facts of a name. The text is the name as the splice has
    -- it.
    RequestReify !Text !RequestName
  | -- | Give the roles of the parameters of a type constructor, with the
    -- name as for 'RequestReify'.
    RequestReifyRoles !Text !RequestName
  | -- | Give the decided strictness of each field of a data constructor,
    -- with the name as for 'RequestReify'.
    RequestReifyConStrictness !Text !RequestName
  | -- | Give the fixity that a declaration gives to a name, with the name
    -- as for 'RequestReify'.
    RequestReifyFixity !Text !RequestName
  | -- | Give the source location of the splice.
    RequestLocation
  | -- | Find a name in the scope of the splice, for @lookupTypeName@
    -- ('True') or @lookupValueName@ ('False'). The name has no package,
    -- and it can have a module qualifier.
    RequestLookupName !Bool !Name
  | -- | Give each extension that is on in the module, for @extsEnabled@
    -- and @isExtEnabled@.
    RequestExtensions
  deriving (Eq, Show)

-- | The name of a request for facts.
data RequestName
  = -- | A name that is bound to a package and a module, with its top-level
    -- entity.
    RequestGlobal !GlobalName
  | -- | A name with no package, from @mkName@. It can have a module
    -- qualifier. The compiler finds the name in the scope of the splice,
    -- as GHC does.
    RequestScoped !Name
  | -- | A different name, such as a name from @newName@. The compiler does
    -- not know its facts.
    RequestUnknown
  deriving (Eq, Show)

-- | The request of a tree.
requestFromTree :: Tree -> Either Text Request
requestFromTree tree =
  case tree of
    TreeNode "Report" [TreeNode "True" [], TreeString message] -> Right (RequestReport True message)
    TreeNode "Report" [TreeNode "False" [], TreeString message] -> Right (RequestReport False message)
    TreeNode "Reify" [TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]] ->
      Right (RequestReify occurrence (requestName occurrence flavour))
    TreeNode "ReifyRoles" [TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]] ->
      Right (RequestReifyRoles occurrence (requestName occurrence flavour))
    TreeNode "ReifyConStrictness" [TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]] ->
      Right (RequestReifyConStrictness occurrence (requestName occurrence flavour))
    TreeNode "ReifyFixity" [TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]] ->
      Right (RequestReifyFixity occurrence (requestName occurrence flavour))
    TreeNode "Location" [] -> Right RequestLocation
    TreeNode "LookupName" [TreeNode "True" [], name] -> RequestLookupName True <$> nameFromTree name
    TreeNode "LookupName" [TreeNode "False" [], name] -> RequestLookupName False <$> nameFromTree name
    TreeNode "Extensions" [] -> Right RequestExtensions
    TreeNode name _ -> Left ("The compiler does not know the splice request " <> name <> ".")
    _ -> Left "A splice request is not a constructor."

-- | The name of a request from the occurrence and the flavour of a name.
requestName :: Text -> Tree -> RequestName
requestName occurrence flavour =
  case flavour of
    TreeNode "NameG" [TreeNode space _, TreeNode "PkgName" [TreeString package], TreeNode "ModName" [TreeString home]] ->
      maybe RequestUnknown (RequestGlobal . GlobalName occurrence (PackageId package) home) (namespace space)
    TreeNode flavourName _
      | flavourName `elem` ["NameS", "NameQ"],
        Right name <- nameFromTree (TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]) ->
          RequestScoped name
    _ -> RequestUnknown
  where
    namespace space
      | space `elem` ["VarName", "DataName", "FldName"] = Just ResolutionNamespaceTerm
      | space == "TcClsName" = Just ResolutionNamespaceType
      | otherwise = Nothing

-- | The @Maybe Name@ reply to 'RequestLookupName': the top-level entity
-- that the compiler found in the scope of the splice, bound to its package
-- and its module, or 'Nothing'. A term with a name that starts with an
-- upper-case letter or a colon is a data constructor.
lookupNameReply :: Maybe GlobalName -> Tree
lookupNameReply found =
  case found of
    Nothing -> TreeNode "Nothing" []
    Just global ->
      TreeNode
        "Just"
        [ TreeNode
            "Name"
            [ TreeNode "OccName" [TreeString (globalNameText global)],
              TreeNode
                "NameG"
                [ TreeNode (space global) [],
                  TreeNode "PkgName" [TreeString (packageIdText (globalNamePackage global))],
                  TreeNode "ModName" [TreeString (globalNameModule global)]
                ]
            ]
        ]
  where
    space global
      | globalNameNamespace global == ResolutionNamespaceType = "TcClsName"
      | Just (first, _) <- T.uncons (globalNameText global), isUpper first || first == ':' = "DataName"
      | otherwise = "VarName"

-- | The @[String]@ reply to 'RequestExtensions': the name of each
-- extension that is on, as the constructor of the Template Haskell type
-- @Extension@ has it. The guest ignores a name that its type does not
-- have. Some extensions have an old name that is the same as a newer
-- extension. Such an extension gets the newer name, as with GHC.
extensionsReply :: [Extension] -> Tree
extensionsReply extensions =
  foldr (\name rest -> TreeNode "(:)" [TreeString name, rest]) (TreeNode "[]" []) (map templateName extensions)
  where
    templateName extension =
      case extension of
        CPP -> "Cpp"
        Rank2Types -> "RankNTypes"
        PolymorphicComponents -> "RankNTypes"
        RecordPuns -> "NamedFieldPuns"
        PatternSignatures -> "ScopedTypeVariables"
        DoRec -> "RecursiveDo"
        _ -> extensionName extension

-- | The errors that a splice reported, as one message. A splice that
-- reported an error fails, also when it gives a result.
reportedFailure :: [Request] -> Maybe Text
reportedFailure requests =
  case [message | RequestReport True message <- requests] of
    [] -> Nothing
    messages -> Just (T.intercalate "\n" messages)
