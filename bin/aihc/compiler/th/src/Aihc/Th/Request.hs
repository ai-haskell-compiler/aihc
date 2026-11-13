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
    requestFromTree,
    reportedFailure,
  )
where

import Aihc.Resolve.Types (GlobalName (..), PackageId (..), ResolutionNamespace (..))
import Aihc.Th.Wire (Tree (..))
import Data.Text (Text)
import Data.Text qualified as T

-- | What a splice asks the compiler to do.
data Request
  = -- | Report an error ('True') or a warning ('False') with a message.
    RequestReport !Bool !Text
  | -- | Give the facts of a name. The text is the name as the splice has
    -- it. A name that is bound to a package and a module also has its
    -- top-level entity. The compiler knows only such names.
    RequestReify !Text !(Maybe GlobalName)
  | -- | Give the roles of the parameters of a type constructor, with the
    -- name as for 'RequestReify'.
    RequestReifyRoles !Text !(Maybe GlobalName)
  deriving (Eq, Show)

-- | The request of a tree.
requestFromTree :: Tree -> Either Text Request
requestFromTree tree =
  case tree of
    TreeNode "Report" [TreeNode "True" [], TreeString message] -> Right (RequestReport True message)
    TreeNode "Report" [TreeNode "False" [], TreeString message] -> Right (RequestReport False message)
    TreeNode "Reify" [TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]] ->
      Right (RequestReify occurrence (globalName occurrence flavour))
    TreeNode "ReifyRoles" [TreeNode "Name" [TreeNode "OccName" [TreeString occurrence], flavour]] ->
      Right (RequestReifyRoles occurrence (globalName occurrence flavour))
    TreeNode name _ -> Left ("The compiler does not know the splice request " <> name <> ".")
    _ -> Left "A splice request is not a constructor."

-- | The top-level entity of a name that is bound to a package and a module.
globalName :: Text -> Tree -> Maybe GlobalName
globalName occurrence flavour =
  case flavour of
    TreeNode "NameG" [TreeNode space _, TreeNode "PkgName" [TreeString package], TreeNode "ModName" [TreeString home]] ->
      GlobalName occurrence (PackageId package) home <$> namespace space
    _ -> Nothing
  where
    namespace space
      | space `elem` ["VarName", "DataName", "FldName"] = Just ResolutionNamespaceTerm
      | space == "TcClsName" = Just ResolutionNamespaceType
      | otherwise = Nothing

-- | The errors that a splice reported, as one message. A splice that
-- reported an error fails, also when it gives a result.
reportedFailure :: [Request] -> Maybe Text
reportedFailure requests =
  case [message | RequestReport True message <- requests] of
    [] -> Nothing
    messages -> Just (T.intercalate "\n" messages)
