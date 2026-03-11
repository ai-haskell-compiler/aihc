{-# options_haddock prune #-}

-- | Description: Internal
module Exon.Quote where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH (Exp (AppE, InfixE, ListE), Q, extsEnabled, runQ)
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter))
import Language.Haskell.TH.Syntax (Quasi)

import Exon.Class.Exon (exonProcess, exonProcessWith)
import Exon.Class.ToSegment (toSegment)
import Exon.Data.RawSegment (RawSegment (AutoExpSegment, ExpSegment, StringSegment, WsSegment))
import qualified Exon.Data.Segment as Segment
import Exon.Haskell.Parse (parseExpWithExts)
import Exon.Parse (parse, parseWs)

exonError ::
  ToString e =>
  MonadFail m =>
  e ->
  m a
exonError err =
  fail ("Exon: " <> toString err)

segmentsQ ::
  QOrIO m =>
  Bool ->
  String ->
  m (NonEmpty RawSegment)
segmentsQ _ "" =
  exonError ("empty quasiquote" :: String)
segmentsQ whitespace s =
  (if whitespace then parseWs else parse) s & fmap nonEmpty & \case
    Right (Just segs) -> pure segs
    Right Nothing -> pure (pure (StringSegment ""))
    Left err -> exonError err

class Quasi m => QOrIO (m :: Type -> Type) where
  fileExtensions :: m [TH.Extension]

instance QOrIO IO where
  fileExtensions =
    pure []

instance QOrIO Q where
  fileExtensions =
    extsEnabled

reifyExp ::
  QOrIO m =>
  String ->
  m Exp
reifyExp "" = exonError ("Empty interpolation" :: String)
reifyExp s = do
  exts <- fileExtensions
  case parseExpWithExts exts s of
    Left (_, _, err) -> exonError err
    Right e -> pure e

reifySegments ::
  QOrIO m =>
  Bool ->
  NonEmpty RawSegment ->
  m (NonEmpty Exp)
reifySegments unsafe segs = do
  expCon <- runQ [e|Segment.Expression|]
  expToSegment <- runQ [e|toSegment|]
  for segs \case
    StringSegment s ->
      runQ [e|Segment.String s|]
    ExpSegment s | unsafe -> do
      e <- reifyExp s
      pure (AppE expCon (AppE expToSegment e))
    ExpSegment s -> do
      e <- reifyExp s
      pure (AppE expCon e)
    AutoExpSegment s -> do
      e <- reifyExp s
      pure (AppE expCon (AppE expToSegment e))
    WsSegment s ->
     runQ [e|Segment.Whitespace s|]

quoteExpWith ::
  QOrIO m =>
  Maybe (Q TH.Exp, Q TH.Exp) ->
  Bool ->
  Bool ->
  String ->
  m Exp
quoteExpWith wrapper whitespace unsafe code = do
  raw <- segmentsQ whitespace code
  hseg :| segs <- reifySegments unsafe raw
  conc <- runQ $ maybe [e|exonProcess|] wrapped wrapper
  consE <- runQ [e|(:|)|]
  pure (AppE conc (InfixE (Just hseg) consE (Just (ListE segs))))
  where
    wrapped (wrap, unwrap) = do
      [e|exonProcessWith ($wrap) $(unwrap)|]

quoteExp ::
  QOrIO m =>
  Bool ->
  Bool ->
  String ->
  m Exp
quoteExp =
  quoteExpWith Nothing

quoteSegments ::
  QOrIO m =>
  String ->
  m Exp
quoteSegments code = do
  raw <- segmentsQ True code
  hseg :| segs <- reifySegments False raw
  consE <- runQ [e|(:|)|]
  pure (InfixE (Just hseg) consE (Just (ListE segs)))

-- | Constructor for a quasiquoter that wraps all segments with the first expression and unwraps the result with the
-- second.
--
-- This can be used to define quoters with custom logic by providing instances of any of the classes in
-- "Exon.Class.Exon" with the result type argument set to the wrapper type:
--
-- >>> import Exon.Class.Exon (ExonString (..))
-- >>> import Exon.Data.Segment (Segment(String))
-- >>> import qualified Data.Text.Lazy.Builder as Text
-- >>> newtype Nl = Nl Text deriving (Generic)
-- >>> getNl (Nl t) = t
-- >>> instance ExonString Nl Text.Builder where exonWhitespace _ = exonString @Nl "\n"
-- >>> exonnl = exonWith (Just ([e|Nl|], [e|getNl|])) True False
-- >>> [exonnl|one   two     three|]
-- "one\ntwo\nthree"
--
-- @since 0.2.0.0
exonWith ::
  Maybe (Q TH.Exp, Q TH.Exp) ->
  Bool ->
  Bool ->
  QuasiQuoter
exonWith wrapper whitespace unsafe =
  QuasiQuoter (quoteExpWith wrapper whitespace unsafe) (err "pattern") (err "type") (err "decl")
  where
    err :: String -> String -> Q a
    err tpe _ =
      exonError ("Cannot quote " <> tpe)

-- | A quasiquoter that allows interpolation, concatenating the resulting segments with '(<>)' or a an arbitrary
-- user-defined implementation.
-- See the [introduction]("Exon") for details.
--
-- >>> [exon|write #{show (5 :: Int)} lines of ##{"code" :: ByteString}|] :: Text
-- "write 5 lines of code"
exon :: QuasiQuoter
exon =
  exonWith Nothing False False

-- | Unsafe version of 'exon', allowing automatic conversion with the same splice brackets as matching types.
--
-- @since 1.0.0.0
exun :: QuasiQuoter
exun =
  exonWith Nothing False True

-- | A variant of 'exon' that creates segments for each sequence of whitespace characters that can be processed
-- differently by 'Exon.ExonAppend', 'Exon.ExonSegment' or 'Exon.ExonString'.
--
-- @since 1.0.0.0
exonws :: QuasiQuoter
exonws =
  exonWith Nothing True False

-- | Internal debugging quoter that produces the raw segments.
--
-- @since 1.0.0.0
exonSegments :: QuasiQuoter
exonSegments =
  QuasiQuoter quoteSegments (err "pattern") (err "type") (err "decl")
  where
    err :: String -> String -> Q a
    err tpe _ =
      exonError ("Cannot quote " <> tpe)
