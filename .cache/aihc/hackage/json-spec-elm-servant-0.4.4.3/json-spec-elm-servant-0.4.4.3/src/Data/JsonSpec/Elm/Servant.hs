{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Data.JsonSpec.Elm.Servant (
  -- * Generating Elm Clients
  servantDefs,
  generateElm,
  generateElmExtra,

  -- * Extensions
  {-|
    The symbols in this section are mainly exposed in case you are using
    some extensions to the standard servant types and need to build some
    companion extensions to generate proper Elm types for them. For most
    normal usage you will probably just use 'generateElm' (or possibly
    'servantDefs').
  -}
  Elmable(..),
  IsParam(..),
  Param(..),
  PathParam(..),
  HeaderParam(..),
  QP(..),
) where


import Bound (Var(B, F), Scope, abstract1, closed, toScope)
import Control.Monad.Writer (MonadTrans(lift), MonadWriter(tell), execWriter)
import Data.Foldable (Foldable(fold), foldl', traverse_)
import Data.HashMap.Strict (HashMap)
import Data.JsonSpec
  ( HasJsonDecodingSpec(DecodingSpec), HasJsonEncodingSpec(EncodingSpec)
  )
import Data.JsonSpec.Elm (HasType(decoderOf, encoderOf, typeOf), Definitions)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.Proxy (Proxy(Proxy))
import Data.Set (Set)
import Data.String (IsString(fromString))
import Data.Text (Text)
import Data.Void (Void, absurd)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Elm.Definition (Definition)
import Language.Elm.Expression (Expression)
import Language.Elm.Name (Module)
import Language.Elm.Pretty (modules)
import Language.Elm.Type (Type)
import Network.HTTP.Types (Method)
import Prelude
  ( Applicative(pure), Bool(False, True), Eq((==))
  , Foldable(foldr, length), Functor(fmap), Maybe(Just, Nothing)
  , Monad((>>=)), Monoid(mconcat, mempty), Semigroup((<>)), Show(show)
  , Traversable(sequence, traverse), ($), (.), (<$>), IO, Int, String, drop
  , error, init, putStrLn, reverse, unlines
  )
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Servant.API
  ( ReflectMethod(reflectMethod), (:<|>), (:>), Capture, Header', Headers, JSON
  , NamedRoutes, NoContent, NoContentVerb, Optional, QueryParam', ReqBody'
  , Required, ToServantApi, Verb
  )
import System.Directory.OsPath
  ( createDirectoryIfMissing, doesDirectoryExist, listDirectory
  )
import System.OsPath ((</>), OsPath, OsString, osp, splitExtension)
import System.Process (readProcess)
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Language.Elm.Definition as Def
import qualified Language.Elm.Expression as Expr
import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pattern as Pat
import qualified Language.Elm.Type as Type
import qualified System.OsPath as OsPath


{-|
  This function will traverse the @api@ type, generating elm definitions for:

  * Http requests for each endpoint, including encoders and decoders for
    anonymous elm types.

  * Named Elm types (i.e. Any 'Specification' that is bound to a name using
    'JsonLet'

  * Decoders and Encoders for named elm types.

  You can consume the resulting 'Definition's using the
  [elm-syntax:Language.Elm.Pretty](https://hackage.haskell.org/package/elm-syntax-0.3.3.0/docs/Language-Elm-Pretty.html)
  module.
-}
servantDefs :: forall api. (Elmable api) => Proxy api -> Set Definition
servantDefs _ =
  builtins
  <> execWriter (endpoints @api [])


builtins :: Set Definition
builtins =
  Set.fromList
    [ Def.Alias
        "Api.Req.Request"
        1
        (
          toScope $
            Type.Record
              [ (Name.Field "method", "Basics.String")
              , (Name.Field "headers", "Basics.List" `Type.App` "Http.Header")
              , (Name.Field "url", "Basics.String")
              , (Name.Field "body", "Http.Body")
              , ( Name.Field "decoder"
                , "Api.Req.Either"
                    `Type.App` Type.Var (B 0)
                    `Type.App` ("Json.Decode.Decoder" `Type.App` Type.Var (B 0))
                )
              ]
        )
    , Def.Type
        "Api.Req.Either"
        2
        [ ( Name.Constructor "Left"
          , [toScope (Type.Var (B 0))]
          )
        , ( Name.Constructor "Right"
          , [toScope (Type.Var (B 1))]
          )
        ]
    , Def.Constant
        "Api.Req.task"
        1
        (
          toScope $
            let
              var :: Type (Bound.Var Int void)
              var = Type.Var (B 0)
            in
              Type.Fun
                ("Api.Req.Request" `Type.App` var)
                (Type.apps "Task.Task" ["Http.Error", var])
        )
        (
          Expr.Lam . toScope $
            let
              req :: Expression (Bound.Var () a)
              req = Expr.Var (B ())

              f :: Text -> b -> (Name.Field, b)
              f name expr = (Name.Field name, expr)

              p :: Expression v -> Text -> Expression v
              p v name = Expr.Proj (Name.Field name) `Expr.App` v
            in
              "Http.task" <|
                Expr.Record
                  [ f "method"   $ p req "method"
                  , f "headers"  $ p req "headers"
                  , f "url"      $ p req "url"
                  , f "body"     $ p req "body"
                  , f "timeout"    "Maybe.Nothing"
                  , f "resolver" $
                      "Http.stringResolver" `Expr.App`
                        (
                          Expr.Lam . toScope $
                            let
                              var :: Expression (Bound.Var () a)
                              var = Expr.Var (B ())

                              pat
                                :: Name.Qualified
                                -> [Pat.Pattern v]
                                -> Expression (Bound.Var b a)
                                -> (Pat.Pattern v, Scope b Expression a)
                              pat con vars expr =
                                (Pat.Con con vars, toScope expr)

                              patVar :: Int -> Expression (Bound.Var Int a)
                              patVar n = Expr.Var (B n)
                            in
                              Expr.Case
                                var
                                [ pat "Http.BadUrl_" [Pat.Var 0] $
                                    "Result.Err" `Expr.App`
                                      ("Http.BadUrl" `Expr.App` patVar 0)
                                , pat "Http.Timeout_" [] $
                                    "Result.Err" `Expr.App` "Http.Timeout"
                                , pat "Http.NetworkError_" [] $
                                    "Result.Err" `Expr.App` "Http.NetworkError"
                                , pat "Http.BadStatus_" [Pat.Var 0, Pat.Var 1] $
                                    "Result.Err" `Expr.App`
                                      (
                                        "Http.BadStatus" `Expr.App`
                                          p (patVar 0) "statusCode"
                                      )
                                , pat
                                    "Http.GoodStatus_"
                                    [Pat.Var 0, Pat.Var 1]
                                    (
                                      Expr.Case
                                        ( F . F <$> p req "decoder")
                                        [ pat "Api.Req.Left" [Pat.Var 0] $
                                            "Result.Ok" `Expr.App` patVar 0
                                        , pat "Api.Req.Right" [Pat.Var 0] $
                                            Expr.Case
                                              (
                                                Expr.apps
                                                  "Json.Decode.decodeString"
                                                  [ patVar 0
                                                  , F <$> patVar 1
                                                  ]
                                              )
                                              [ pat "Result.Err" [Pat.Var 0] $
                                                  "Result.Err"
                                                    <| "Http.BadBody"
                                                    <| "Json.Decode.errorToString"
                                                    <| patVar 0
                                              , pat "Result.Ok" [Pat.Var 0] $
                                                  "Result.Ok" <| patVar 0
                                              ]
                                        ]
                                    )
                                ]
                        )
                  ]
        )
    , Def.Constant
        "Api.Req.task_"
        2
        (
          toScope $
            let
              returnTypeVar :: Type (Bound.Var Int void)
              returnTypeVar = Type.Var (B 0)

              errTypeVar :: Type (Bound.Var Int void)
              errTypeVar =
                "Api.Req.Either"
                `Type.App` "Http.Error"
                `Type.App` Type.Var (B 1)

              errorParserType :: Type (Bound.Var Int void)
              errorParserType =
                Type.Fun "Basics.Int"
                . Type.Fun "Basics.String"
                $ "Basics.Maybe" `Type.App` Type.Var (B 1)
            in
              Type.Fun
                ("Api.Req.Request" `Type.App` returnTypeVar)
                (
                  Type.Fun
                    errorParserType
                    (Type.apps "Task.Task" [errTypeVar, returnTypeVar])
                )
        )
        (
          lam (\_req -> lam (\_errorParser ->
            let
              req :: Expression (Var () (Var () Void))
              req = Expr.Var (F (B ()))

              f :: Text -> b -> (Name.Field, b)
              f name expr = (Name.Field name, expr)

              p :: Expression v -> Text -> Expression v
              p v name = Expr.Proj (Name.Field name) `Expr.App` v
            in
              "Http.task" <|
                Expr.Record
                  [ f "method"   $ p req "method"
                  , f "headers"  $ p req "headers"
                  , f "url"      $ p req "url"
                  , f "body"     $ p req "body"
                  , f "timeout"    "Maybe.Nothing"
                  , f "resolver" $
                      "Http.stringResolver" <|
                        (
                          lam (\var ->
                            let
                              pat
                                :: Name.Qualified
                                -> [Pat.Pattern v]
                                -> Expression (Bound.Var b a)
                                -> (Pat.Pattern v, Scope b Expression a)
                              pat con vars expr =
                                (Pat.Con con vars, toScope expr)

                              patVar :: Int -> Expression (Bound.Var Int a)
                              patVar n = Expr.Var (B n)

                              httpErr :: Expression v -> Expression v
                              httpErr a =
                                  "Result.Err" <| ("Api.Req.Left" <| a)
                            in
                              Expr.Case
                                var
                                [ pat "Http.BadUrl_" [Pat.Var 0] $
                                    httpErr $
                                      "Http.BadUrl" <| patVar 0
                                , pat "Http.Timeout_" [] $
                                    httpErr "Http.Timeout"
                                , pat "Http.NetworkError_" [] $
                                    httpErr "Http.NetworkError"
                                , pat "Http.BadStatus_" [Pat.Var 0, Pat.Var 1] $
                                    "Result.Err"
                                    <| (
                                         "Maybe.withDefault"
                                         <| "Api.Req.Left"
                                         <| "Http.BadStatus"
                                         <| p (patVar 0) "statusCode"
                                       )
                                    <| ("Maybe.map" <| "Api.Req.Right")
                                    <| (
                                         Expr.Var (F . F $ B ())
                                         <| p (patVar 0) "statusCode"
                                       )
                                    <| patVar 1
                                , pat
                                    "Http.GoodStatus_"
                                    [Pat.Var 0, Pat.Var 1]
                                    (
                                      Expr.Case
                                        ( F . F <$> p req "decoder")
                                        [ pat "Api.Req.Left" [Pat.Var 0] $
                                            "Result.Ok" <| patVar 0
                                        , pat "Api.Req.Right" [Pat.Var 0] $
                                            Expr.Case
                                              (
                                                Expr.apps
                                                  "Json.Decode.decodeString"
                                                  [ patVar 0
                                                  , F <$> patVar 1
                                                  ]
                                              )
                                              [ pat "Result.Err" [Pat.Var 0] $
                                                  httpErr $
                                                    "Http.BadBody"
                                                    <| "Json.Decode.errorToString"
                                                    <| patVar 0
                                              , pat "Result.Ok" [Pat.Var 0] $
                                                  "Result.Ok" <| patVar 0
                                              ]
                                        ]
                                    )
                                ]
                          )
                        )
                  ]
          ))
        )
    ]


{-| Class of servant APIs for which Elm client code can be generated. -}
class Elmable e where
  {-|
    Collect all the Elm definitions needed to implement a client for
    the API.  This is called recursively on our walk down the API tree,
    and the @['Param']@ argument contains all the request parameters
    (like 'Servant.API.Capture', 'Servant.API.ReqBody'', etc) that have been encountered so far on
    whatever particular branch . It will start out empty at the API root.
  -}
  endpoints :: [Param] -> Definitions ()
instance (Elmable a, Elmable b) => Elmable (a :<|> b) where
  endpoints params = do
    endpoints @a params
    endpoints @b params
instance (Elmable (ToServantApi api)) => Elmable (NamedRoutes api) where
  endpoints = endpoints @(ToServantApi api)
instance (IsParam a, Elmable b) => Elmable (a :> b) where
  endpoints params = do
    p <- param @a
    endpoints @b (p : params)
instance (Elmable (Verb m c t r)) => Elmable (Verb m c t (Headers h r)) where
  endpoints = endpoints @(Verb m c t r)
instance {- Elmable (Verb m c t NoContent) -}
    (Elmable (NoContentVerb m))
  =>
    Elmable (Verb m c t NoContent)
  where
    endpoints = endpoints @(NoContentVerb m)
instance {- Elmable (Verb method code types response) -}
    {-# overlaps #-}
    ( HasType (EncodingSpec response)
    , ReflectMethod method
    )
  =>
    Elmable (Verb method code types response)
  where
    endpoints (reverse -> params) = do
      responseType <- typeOf @(EncodingSpec response)
      decoder <- decoderOf @(EncodingSpec response)
      tell . Set.singleton $
        Def.Constant
          (requestFunctionName @method params)
          (length params)
          (requestFunctionType params responseType)
          (
            requestFunctionBody
              params
              (reflectMethod (Proxy @method))
              ("Api.Req.Right" `Expr.App` decoder)
          )
      pure ()
instance (ReflectMethod method) => Elmable (NoContentVerb method) where
  endpoints (reverse -> params) = do
    tell . Set.singleton $
      Def.Constant
        (requestFunctionName @method params)
        (length params)
        (requestFunctionType params "Basics.()")
        (
          requestFunctionBody
            params
            (reflectMethod (Proxy @method))
            ("Api.Req.Left" `Expr.App` "Basics.()")
        )
    pure ()


{-|
  Obtain a value-level request parameter type from the type-level servant
  parameter type.
-}
class IsParam a where
  param :: Definitions Param
{-|
  The default instance assumes that the API combinator does not contribute
  to the parameter list or the "name of the endpoint" in any way. This
  covers things like 'Summary' and 'Description', or potentially custom
  combinators created by the user. In the case of custom combinators,
  you may need to create an overlapping instance if you wish it to affect
  the elm function parameters or function name.
-}
instance {-# OVERLAPPABLE #-} IsParam a where
  param = pure Ignore
instance (KnownSymbol name) => IsParam (Capture name tpy) where
  param = pure $ PathParam (Capture (sym @name))
instance (KnownSymbol name) => IsParam (Header' (Optional : mods) name a) where
  param = pure $ HeaderParam (OptionalHeader (sym @name))
instance (KnownSymbol name) => IsParam (Header' (Required : mods) name a) where
  param = pure $ HeaderParam (RequiredHeader (sym @name))
instance {- IsParam (Header' (other : mods) name a) -}
    {-# OVERLAPS #-} (IsParam (Header' mods name a))
  =>
    IsParam (Header' (other : mods) name a)
  where
    param = param @(Header' mods name a)
instance {- IsParam (ReqBody' (Required : mods) (JSON : accept) a) -}
    (HasType (DecodingSpec a))
  =>
    IsParam (ReqBody' (Required : mods) (JSON : accept) a)
  where
    param = do
      elmType <- typeOf @(DecodingSpec a)
      encoder <- encoderOf @(DecodingSpec a)
      pure $ BodyEncoder {elmType, encoder}
instance {- IsParam (ReqBody' (other : mods) (JSON : accept) a) -}
    {-# overlaps #-} (IsParam (ReqBody' mods '[JSON] a))
  =>
    IsParam (ReqBody' (other : mods) (JSON : accept) a)
  where
    param = param @(ReqBody' mods '[JSON] a)
instance {- IsParam (ReqBody' mods (other : accept) a) -}
    {-# overlaps #-} (IsParam (ReqBody' mods accept a))
  =>
    IsParam (ReqBody' mods (other : accept) a)
  where
    param = param @(ReqBody' mods accept a)
instance (KnownSymbol segment) => IsParam (segment :: Symbol) where
  param = pure $ PathParam (Static (sym @segment))
instance {- IsParam (QueryParam' (Optional : more) name typ) -}
    (KnownSymbol name)
  =>
    IsParam (QueryParam' (Optional : more) name typ)
  where
    param = pure $ QueryParam (OptionalQP (sym @name))
instance {- IsParam (QueryParam' (Required : more) name typ) -}
    (KnownSymbol name)
  =>
    IsParam (QueryParam' (Required : more) name typ)
  where
    param = pure $ QueryParam (RequiredQP (sym @name))
instance {- IsParam (QueryParam' (other : more) name typ) -}
    {-# overlaps #-} (IsParam (QueryParam' more name typ))
  =>
    IsParam (QueryParam' (other : more) name typ)
  where
    param = param @(QueryParam' more name typ)


requestFunctionName
  :: forall method. (ReflectMethod method)
  => [Param]
  -> Name.Qualified
requestFunctionName params =
    Name.Qualified
      ["Api", "Req"]
      (fold (methodName : pathParts))
  where
    methodName :: Text
    methodName =
      Text.toLower
      . TE.decodeUtf8
      . reflectMethod
      $ Proxy @method

    pathParts :: [Text]
    pathParts =
      Text.toTitle <$>
        mapMaybe
          (\case
            PathParam (Static segment) -> Just (munge segment)
            PathParam (Capture name) -> Just (munge name)
            _ -> Nothing
          )
          params

    {-
      Try to generate valid names in the face of common api path
      idioms. It isn't really worth it for this to be complete, but we
      at least want to cover the basics
    -}
    munge :: Text -> Text
    munge = Text.replace "-" "_"


requestFunctionType
  :: [Param]
  -> Type Void
  -> Scope Int Type Void
requestFunctionType params responseType =
    lift funType
  where
    funType :: Type Void
    funType =
      foldr
        Type.Fun
        ("Api.Req.Request" `Type.App` responseType)
        (
          mapMaybe
            (\case
              PathParam (Capture _) -> Just "Basics.String"
              PathParam (Static _) -> Nothing
              QueryParam (RequiredQP _) -> Just "Basics.String"
              QueryParam (OptionalQP _) ->
                Just ("Basics.Maybe" `Type.App` "Basics.String")
              HeaderParam (RequiredHeader _) -> Just "Basics.String"
              HeaderParam (OptionalHeader _) ->
                Just ("Basics.Maybe" `Type.App` "Basics.String")
              BodyEncoder typ _ -> Just typ
              Ignore -> Nothing
            )
            params
        )


requestFunctionBody
  :: [Param]
  -> Method
  -> Expression Void
  -> Expression Void
requestFunctionBody params method decoder =
    buildLambda
      (reverse params)
      (
        Expr.Record
          [ (Name.Field "method", Expr.String (TE.decodeUtf8 method))
          , (Name.Field "headers", headers)
          , (Name.Field "url", url)
          , (Name.Field "body", body)
          , (Name.Field "decoder", Expr.bind g absurd decoder)
          ]
      )
  where
    headers :: Expression Param
    headers =
        Expr.apps
          "List.filterMap"
          [ "Basics.identity"
          , Expr.List
              [ headerExpr header
              | HeaderParam header <- params
              ]
          ]
      where
        headerExpr :: HeaderParam -> Expression Param
        headerExpr header =
            Expr.apps
              "Maybe.map"
              [ "Http.header" `Expr.App` Expr.String name
              , case header of
                  RequiredHeader _ ->
                    "Maybe.Just" `Expr.App` Expr.Var (HeaderParam header)
                  OptionalHeader _ -> Expr.Var (HeaderParam header)
              ]
          where
            name :: Text
            name =
              case header of
                  RequiredHeader n -> n
                  OptionalHeader n -> n

    url :: Expression Param
    url =
      Expr.apps
        "Url.Builder.absolute"
        [
          Expr.List
            [ case pp of
                Static part -> Expr.String part
                Capture _ -> Expr.Var param_
            | param_@(PathParam pp) <- params
            ]
        , Expr.apps
            "List.filterMap"
            [ "Basics.identity"
            , Expr.List
                [ let
                    name :: Text
                    name  = case qp of { RequiredQP n -> n; OptionalQP n -> n}

                    queryExpr :: Expression Param
                    queryExpr =
                      Expr.apps
                        "Maybe.map"
                        [ "Url.Builder.string" `Expr.App` Expr.String name
                        , case qp of
                            RequiredQP _ ->
                              "Maybe.Just" `Expr.App` Expr.Var param_
                            OptionalQP _ -> Expr.Var param_
                        ]
                  in
                    queryExpr
                | param_@(QueryParam qp) <- params
                ]
            ]
        ]

    body :: Expression Param
    body =
      case
        [ g "Http.jsonBody" `Expr.App`
            ((absurd <$> encoder) `Expr.App` Expr.Var param_)
        | param_@(BodyEncoder _ encoder) <- params
        ]
      of
        [] -> g "Http.emptyBody"
        (encoder : _) -> encoder

    buildLambda :: [Param] -> Expression Param -> Expression Void
    buildLambda = \cases
      [] e ->
        fromMaybe
          (error "Paramaters in expression to not match the parameter list.")
          (Bound.closed e)
      (PathParam (Static _) : more) e ->
        buildLambda more e
      (Ignore : more) e ->
        buildLambda more e
      (p : more) e ->
        buildLambda
          more
          (Expr.Lam (abstract1 p e))


data Param
  = PathParam PathParam
  | HeaderParam HeaderParam
  | QueryParam QP
  | BodyEncoder
      { elmType :: Type Void
      , encoder :: Expression Void
      }
  | Ignore {-^ This is for things like `Summary` -}
  deriving stock (Eq)


data QP
  = RequiredQP Text
  | OptionalQP Text
  deriving stock (Eq)


data PathParam
  = Static Text
  | Capture Text
  deriving stock (Eq)


data HeaderParam
  = RequiredHeader Text
  | OptionalHeader Text
  deriving stock (Eq)


g :: Name.Qualified -> Expression any
g = Expr.Global


sym
  :: forall a b.
     ( IsString b
     , KnownSymbol a
     )
  => b
sym = fromString $ symbolVal (Proxy @a)


{-|
  "Batteries included" way to generate some Elm code on disk in a given
  a directory. The directory should be dedicated to the generated Elm
  code and you shouldn't try to store anything else in that directory,
  Elm files or otherwise.

  This function will succeed without error if (and only if) the directory
  already exists /and/ its contents exactly match what would be generated
  anyway.

  If the files on disk are wrong, then an error is thrown, and the files
  are unmodified.

  If the directory does not exist, then it is created and the Elm code
  is generated inside the directory, /then an error is thrown/.

  The intent is that you can use this function (thinly wrapped with
  the appropriate directory and api spec) as the main function for a
  test suite, where the "test" is that the files on disk are /already/
  correct. We throw an error even in the case where we generate files
  for you because, for instance, you wouldn't want CI to be generating
  these files when you forgot to check them in in the first place.
-}
generateElm
  :: forall api. (Elmable api)
  => OsPath {-^ The directory in which to deposit Elm code. -}
  -> Proxy api
  -> IO ()
generateElm dir proxy =
  generateElmExtra dir proxy mempty


{-|
  Like 'generateElm', but allow for some extra definitions to be
  added. This might be useful for piling on elm definitions that need
  to be decoded by the front end but which are not directly referenced
  in your servant API (e.g. a common response body for error responses).
-}
generateElmExtra
  :: forall api. (Elmable api)
  => OsPath {-^ The directory in which to deposit Elm code. -}
  -> Proxy api
  -> Set Definition {-^ Any extra definitions you want added.  -}
  -> IO ()
generateElmExtra dir Proxy extra = do
    definitions :: HashMap Module Text
      <-
        traverse
          (
            elmFormat
            . (<> "\n")
            . renderStrict
            . layoutPretty defaultLayoutOptions
          )
        . modules
        . Set.toList
        $ servantDefs (Proxy @api) <> extra

    doesDirectoryExist dir >>= \case
      False -> do
        traverse_ writeModule (HM.toList definitions)
        error $
          unlines
            [ ""
            , "   We successfully generated the elm code, but we are going to"
            , "   fail the test anyway because the the success criteria for"
            , "   the test is that the generated files on disk are _already_"
            , "   correct. You wouldn't want CI to pass in this case,"
            , "   for instance."
            ]
      True -> do
        checkModules definitions
        putStrLn "Test passes. Generated files are up to date."
  where
    elmFormat :: Text -> IO Text
    elmFormat elmCode = do
      putStrLn $ "Formatting: " <> show elmCode
      result <-
        Text.pack <$>
          readProcess
            "elm-format"
            ["--stdin"]
            (Text.unpack elmCode)
      putStrLn $ "Result: " <> show result
      pure result


    checkModules :: HashMap Module Text -> IO ()
    checkModules generatedModules = do
        modulesOnDisk <- getFiles dir
        if modulesOnDisk == generatedModules
          then pure ()
          else do
            putStrLn $
              unlines
                [ "expected: " <> show generatedModules
                , "actual:   " <> show modulesOnDisk
                , ""
                ]
            error $
              "Please regenerate modules by completely deleting the `"
              <> show dir <> "` directory and then running the test again."
      where
        getFiles :: OsPath -> IO (HashMap Module Text)
        getFiles path =
          case splitExtension path of
            (pToStr -> mod, pToStr -> ".elm") -> do
              content <- TIO.readFile (pToStr path)
              pure $
                HM.singleton
                  (
                    drop 1
                    . Text.split (== '/')
                    . Text.pack
                    $ mod
                  )
                  content
            _ -> do
              children <- listDirectory path
              fmap mconcat . sequence $
                [ getFiles (path </> child)
                | child <- children
                ]


    writeModule :: (Module, Text) -> IO ()
    writeModule (module_, content) = do
        createDirectoryIfMissing True dirname
        path <- OsPath.decodeUtf filename
        TIO.writeFile path content
      where
        pathName :: [Text] -> OsPath
        pathName =
          foldl' (</>) dir
          . fmap (fromJust . OsPath.encodeUtf . Text.unpack)

        filename :: OsPath
        filename = pathName module_ <> [osp|.elm|]

        dirname :: OsPath
        dirname = pathName (init module_)

    pToStr :: OsString -> String
    pToStr = fmap OsPath.toChar . OsPath.unpack


{-|
  Produce lambda in Elm out of a haskell function.

  > lam (\var ->
  >   "elmFunction" `a` var
  > )

  produces an Elm lambda expression of the form

  > (\var -> elmFunction var)
-}
lam
  :: (Expression (Var () a) -> Expression (Var () v))
  -> Expression v
lam f =
  Expr.Lam . toScope $ f (Expr.Var (B ()))


infixr 5 <|
(<|) :: Expression v -> Expression v -> Expression v
f <| v = Expr.App f v


