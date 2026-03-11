-- |
-- Module: WildBind.Exec
-- Description: Functions to create executable actions.
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
module WildBind.Exec
    ( -- * Functions to build executable action
      wildBind
    , wildBind'
      -- * Option for executable
    , Option
    , defOption
      -- ** Accessor functions for 'Option'
    , optBindingHook
    , optCatch
    ) where

import           Control.Applicative        ((<$>))
import           Control.Exception          (SomeException, catch)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State  as State
import           Data.List                  ((\\))
import           System.IO                  (hPutStrLn, stderr)

import           WildBind.Binding           (Action (actDescription, actDo), Binding, boundAction,
                                             boundActions, boundInputs)
import           WildBind.Description       (ActionDescription)
import           WildBind.FrontEnd          (FrontEnd (frontNextEvent, frontSetGrab, frontUnsetGrab),
                                             FrontEvent (FEChange, FEInput))

type GrabSet i = [i]

updateGrab :: (Eq i) => FrontEnd s i -> GrabSet i -> GrabSet i -> IO ()
updateGrab f before after = do
  mapM_ (frontUnsetGrab f) (before \\ after)
  mapM_ (frontSetGrab f) (after \\ before)

-- | Combines the 'FrontEnd' and the 'Binding' and returns the executable.
wildBind :: (Ord i) => Binding s i -> FrontEnd s i -> IO ()
wildBind = wildBind' defOption

-- | Build the executable with 'Option'.
wildBind' :: (Ord i) => Option s i -> Binding s i -> FrontEnd s i -> IO ()
wildBind' opt binding front =
  flip Reader.runReaderT opt $ flip State.evalStateT (binding, Nothing) $ wildBindInContext front

-- | WildBind configuration options.
--
-- You can get the default value of 'Option' by 'defOption' funcion,
-- and modify its members via accessor functions listed below.
data Option s i
  = Option
      { optBindingHook :: [(i, ActionDescription)] -> IO ()
        -- ^ An action executed when current binding may be
        -- changed. Default: do nothing.
      , optCatch       :: s -> i -> SomeException -> IO ()
        -- ^ the handler for exceptions thrown from bound
        -- actions. Default: just print the 'SomeException' to
        -- 'stderr' and ignore it.
      }

defOption :: Option s i
defOption = Option { optBindingHook = const $ return (),
                     optCatch = \_ _ exception -> hPutStrLn stderr ("Exception from WildBind action: " ++ show exception)
                   }

-- | Internal state. fst is the current Binding, snd is the current front-end state.
type WBState s i = (Binding s i, Maybe s)

-- | A monad keeping WildBind context.
type WBContext s i = State.StateT (WBState s i) (Reader.ReaderT (Option s i) IO)

askOption :: WBContext s i (Option s i)
askOption = lift $ Reader.ask

boundDescriptions :: Binding s i -> s -> [(i, ActionDescription)]
boundDescriptions b s = fmap (\(i, act) -> (i, actDescription act)) $ boundActions b s

updateWBState :: (Eq i) => FrontEnd s i -> Binding s i -> s -> WBContext s i ()
updateWBState front after_binding after_state = do
  (before_binding, before_mstate) <- State.get
  let before_grabset = maybe [] (boundInputs before_binding) before_mstate
  State.put $ (after_binding, Just after_state)
  liftIO $ updateGrab front before_grabset (boundInputs after_binding after_state)
  hook <- optBindingHook <$> askOption
  liftIO $ hook $ boundDescriptions after_binding after_state

updateFrontState :: (Eq i) => FrontEnd s i -> s -> WBContext s i ()
updateFrontState front after_state = do
  (cur_binding, _) <- State.get
  updateWBState front cur_binding after_state

updateBinding :: (Eq i) => FrontEnd s i -> Binding s i -> WBContext s i ()
updateBinding front after_binding = do
  (_, mstate) <- State.get
  case mstate of
    Nothing    -> return ()
    Just state -> updateWBState front after_binding state

wildBindInContext :: (Ord i) => FrontEnd s i -> WBContext s i ()
wildBindInContext front = impl where
  impl = do
    event <- liftIO $ frontNextEvent front
    case event of
      FEChange state ->
        updateFrontState front state
      FEInput input -> do
        (cur_binding, mcur_state) <- State.get
        case stateAndAction cur_binding mcur_state input of
          Nothing -> return ()
          Just (cur_state, action) -> do
            handler <- getExceptionHandler cur_binding cur_state input
            next_binding <- liftIO $ actDo action `catch` handler
            updateBinding front next_binding
    wildBindInContext front
  stateAndAction binding mstate input = do
    state <- mstate
    action <- boundAction binding state input
    return (state, action)
  getExceptionHandler binding state input = do
    opt_catch <- optCatch <$> askOption
    return $ \e -> do
      opt_catch state input e
      return binding
