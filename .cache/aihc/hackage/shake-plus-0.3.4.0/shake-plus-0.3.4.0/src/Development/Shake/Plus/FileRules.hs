{- |
   Module     : Development.Shake.Plus.FileRules
   License    : MIT
   Stability  : experimental

Filerules in "Development.Shake" lifted to `MonadAction` and well-typed `Path`s.
-}
module Development.Shake.Plus.FileRules (
  need
, want
, needP
, wantP
, needIn
, wantIn
, (%>)
, (|%>)
, phony
) where

import           Control.Exception.Extra
import qualified Development.Shake
import           Development.Shake.Plus.Core
import           Path
import           RIO                         as R

-- | Lifted version of `Development.Shake.need`, This still uses `String`s
-- because it may refer to a phony rule. For the `Path` specific version
-- use `needP`
need :: (Partial, MonadAction m, Foldable t) => t String -> m ()
need = liftAction . Development.Shake.need . toList

-- | Lifted version of `Development.Shake.want`. This still uses `String`s
-- because it may refer to a phony rule. For the `Path` specific version
-- use wantP.
want :: (Partial, MonadRules m, Foldable t) => t String -> m ()
want = liftRules . Development.Shake.want . toList

-- | Lifted version of `Development.Shake.need` using well-typed `Path`s
needP :: (Partial, MonadAction m, Traversable t) => t (Path b File) -> m ()
needP = need . fmap toFilePath

-- | Lifted version of `Development.Shake.want` using well-typed `Path`s
wantP :: (Partial, MonadRules m, Traversable t) => t (Path b File) -> m ()
wantP = want . fmap toFilePath

-- | Like `needP`, but accepts `Path`s relative to the first argument.
needIn :: (Partial, MonadAction m, Traversable t) => Path b Dir -> t (Path Rel File) -> m ()
needIn x = needP . fmap (x </>)

-- | Like `wantP`, but accepts `Path`s relative to the first argument.
wantIn :: (Partial, MonadRules m, Traversable t) => Path b Dir -> t (Path Rel File) -> m ()
wantIn x = wantP . fmap (x </>)

-- | Lifted version of `Development.Shake.%>` using well-typed `Path`s
(%>) :: (Partial, MonadReader r m, MonadRules m) => FilePattern -> (Path Rel File -> RAction r ()) -> m ()
(%>) x ract = R.ask >>= \r -> liftRules $ x Development.Shake.%> (runRAction r . (ract <=< parseRelFile))

-- | Lifted version of `Development.Shake.|%>` using well-typed `Path`s
(|%>) :: (Partial, MonadReader r m, MonadRules m) => [FilePattern] -> (Path Rel File -> RAction r ()) -> m ()
(|%>) x ract = R.ask >>= \r -> liftRules $ x Development.Shake.|%> (runRAction r . (ract <=< parseRelFile))

-- | Lifted version of `Development.Shake.phony` using `RAction`
phony :: (MonadReader r m, MonadRules m) => String -> RAction r () -> m ()
phony x ract = R.ask >>= \r -> liftRules $ Development.Shake.phony x $ runRAction r ract
