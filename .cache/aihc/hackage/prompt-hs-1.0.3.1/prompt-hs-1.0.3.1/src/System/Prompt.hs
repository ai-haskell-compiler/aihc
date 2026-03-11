{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module : System.Prompt
-- Description : A user-friendly, dependently-typed library for asking your users questions
-- License : BSD-3-Clause
--
-- @
-- data Colour = Red | Green | Blue deriving (Bounded, Enum, Eq, Show)
--
-- instance Chooseable Colour where
--  showChooseable = pack . show
--
-- main :: IO ()
-- main = do
--  name <- promptText Required RequireConfirmation $ pack "What is your name?"
--  favouriteColour <- promptChoice Optional DontConfirm (Proxy :: Proxy Colour) $ pack "And what is your favourite colour?"
--  putStrLn $
--    "Your name is " <> unpack name <> " and " <> case favouriteColour of
--      Just c -> "your favourite colour is " <> show c
--      Nothing -> "you didn't tell me your favourite colour."
-- @
module System.Prompt
  ( -- * Actions
    promptText,
    promptYesNo,
    promptYesNoWithDefault,
    promptChoice,
    promptChoiceFromSet,
    promptMultipleChoice,
    promptMultipleChoiceFromSet,

    -- * Data
    Chooseable (..),
    ChooseableItem (..),
    Requirement (..),
    PerRequirement,
    Confirmation (..),
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Char as C
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Lens.Micro (Lens', lens, (%~), (&), (.~))
import System.Exit (exitFailure)
import System.Prompt.Requirement
  ( Requirement (Optional, Required),
    SRequirement (SOptional, SRequired),
  )
import System.Terminal
  ( Direction (Downwards, Upwards),
    Event (KeyEvent),
    Interrupt (Interrupt),
    Key (ArrowKey, BackspaceKey, CharKey, EnterKey, EscapeKey),
    MonadColorPrinter (blue, cyan, foreground, magenta, red, yellow),
    MonadFormattingPrinter (bold, italic),
    MonadInput,
    MonadMarkupPrinter (Attribute, resetAttributes, setAttribute),
    MonadPrinter (flush, putText, putTextLn),
    MonadScreen (deleteLines, moveCursorUp),
    awaitEvent,
    ctrlKey,
    runTerminalT,
    withTerminal,
  )

-- | Things which can be chosen, following a prompt.
--
-- Most methods are automatically implemented for types with instances
-- of `Bounded` and `Enum`.
class Chooseable a where
  -- | Display the option in a user-friendly manner
  showChooseable :: a -> Text

  -- | Get the initially-selected value of `a`
  initialSelectionChooseable :: a
  default initialSelectionChooseable :: (Bounded a) => a
  initialSelectionChooseable = minBound

  -- | Get all values of `a`
  universeChooseableNE :: NonEmpty a
  default universeChooseableNE :: (Bounded a, Enum a) => NonEmpty a
  universeChooseableNE = minBound :| drop 1 [minBound .. maxBound]

instance (Chooseable a) => Chooseable (Maybe a) where
  showChooseable :: Maybe a -> Text
  showChooseable (Just a) = showChooseable a
  showChooseable Nothing = "Skip"

  initialSelectionChooseable :: Maybe a
  initialSelectionChooseable = Nothing

  universeChooseableNE :: NonEmpty (Maybe a)
  universeChooseableNE = Nothing <| (Just <$> universeChooseableNE)

-- | Things which can be chosen, but which are not part of a sum type.
class ChooseableItem a where
  -- | Display the option in a user-friendly manner
  chooseableItemText :: a -> Text

  -- | Get the initially-selected value of `a`
  initialSelectionChooseableItem :: a

instance (Chooseable a) => ChooseableItem a where
  chooseableItemText :: a -> Text
  chooseableItemText = showChooseable

  initialSelectionChooseableItem :: a
  initialSelectionChooseableItem = initialSelectionChooseable

-- | Returns `a` if the question is required, or `Maybe` `a` otherwise.
--
-- Represents what you'd expect to be the response to the given question, wrapped in `Maybe` for `Optional` questions.
type family PerRequirement (requirement :: SRequirement) (a :: Type) where
  PerRequirement 'SRequired a = a
  PerRequirement 'SOptional a = Maybe a

-- | Whether to get confirmation from the user before accepting their answer.
data Confirmation
  = -- | Get confirmation
    RequireConfirmation
  | -- | Don't get confirmation
    DontConfirm

-- Represents instructions shown to the user while prompting them to make a choice
data ChoiceInstruction
  = ChoiceInstructionNormal
  | ChoiceInstructionNoOptionSelected

-- | Gets the text which will be displayed to the user for a given instruction.
instructionText :: ChoiceInstruction -> Text
instructionText = \case
  ChoiceInstructionNormal -> "You can type to search, or use the arrow keys. Press enter to select."
  ChoiceInstructionNoOptionSelected -> "No option selected. Try expanding your filter with backspace to see more options."

-- | Gets the text which will be displayed to the user for a given instruction.
instructionTextMulti :: ChoiceInstruction -> Text
instructionTextMulti = \case
  ChoiceInstructionNormal -> "You can type to search, or use the arrow keys. Press enter to select/deselect, and ctrl+enter to confirm."
  ChoiceInstructionNoOptionSelected -> "No options match this search. Try expanding your filter with backspace to see more options."

class IsPromptChoiceState a where
  ipcsGetNumberOfOptions :: a -> Int
  ipcsGetNumberOfFilteredOptions :: a -> Int

data PromptChoiceState (requirement :: SRequirement) (a :: Type) = PromptChoiceState
  { pcsConfirmation :: Confirmation,
    pcsFilter :: Text,
    pcsInstruction :: ChoiceInstruction,
    pcsOptions :: NonEmpty (PerRequirement requirement a),
    pcsFilteredOptions :: [PerRequirement requirement a],
    pcsHoveredOption :: Maybe (PerRequirement requirement a)
  }

instance IsPromptChoiceState (PromptChoiceState requirement a) where
  ipcsGetNumberOfOptions = length . pcsOptions
  ipcsGetNumberOfFilteredOptions = length . pcsFilteredOptions

data PromptMultipleChoiceState (a :: Type) = PromptMultipleChoiceState
  { pmcsFilter :: Text,
    pmcsInstruction :: ChoiceInstruction,
    pmcsOptions :: NonEmpty a,
    pmcsFilteredOptions :: [a],
    pmcsHoveredOption :: Maybe a,
    pmcsSelectedOptions :: [a]
  }

instance IsPromptChoiceState (PromptMultipleChoiceState a) where
  ipcsGetNumberOfOptions = length . pmcsOptions
  ipcsGetNumberOfFilteredOptions = length . pmcsFilteredOptions

_pcsFilter :: Lens' (PromptChoiceState requirement a) Text
_pcsFilter = lens pcsFilter \s x -> s {pcsFilter = x}

_pmcsFilter :: Lens' (PromptMultipleChoiceState a) Text
_pmcsFilter = lens pmcsFilter \s x -> s {pmcsFilter = x}

_pcsInstruction :: Lens' (PromptChoiceState requirement a) ChoiceInstruction
_pcsInstruction = lens pcsInstruction \s x -> s {pcsInstruction = x}

_pmcsInstruction :: Lens' (PromptMultipleChoiceState a) ChoiceInstruction
_pmcsInstruction = lens pmcsInstruction \s x -> s {pmcsInstruction = x}

_pcsFilteredOptions :: Lens' (PromptChoiceState requirement a) [PerRequirement requirement a]
_pcsFilteredOptions = lens pcsFilteredOptions \s x -> s {pcsFilteredOptions = x}

_pmcsFilteredOptions :: Lens' (PromptMultipleChoiceState a) [a]
_pmcsFilteredOptions = lens pmcsFilteredOptions \s x -> s {pmcsFilteredOptions = x}

_pcsHoveredOption :: Lens' (PromptChoiceState requirement a) (Maybe (PerRequirement requirement a))
_pcsHoveredOption = lens pcsHoveredOption \s x -> s {pcsHoveredOption = x}

_pmcsHoveredOption :: Lens' (PromptMultipleChoiceState a) (Maybe a)
_pmcsHoveredOption = lens pmcsHoveredOption \s x -> s {pmcsHoveredOption = x}

_pmcsSelectedOptions :: Lens' (PromptMultipleChoiceState a) [a]
_pmcsSelectedOptions = lens pmcsSelectedOptions \s x -> s {pmcsSelectedOptions = x}

data PromptTextState (requirement :: SRequirement) = PromptTextState
  { ptsConfirmation :: Confirmation,
    ptsCurrentInput :: Text,
    ptsPrompt :: Text,
    ptsRequirement :: Requirement requirement
  }

_ptsCurrentInput :: Lens' (PromptTextState requirement) Text
_ptsCurrentInput = lens ptsCurrentInput \s x -> s {ptsCurrentInput = x}

type Renderable m requirement a =
  ( ChooseableItem (PerRequirement requirement a),
    Eq (PerRequirement requirement a),
    MonadColorPrinter m,
    MonadFormattingPrinter m,
    MonadMarkupPrinter m,
    MonadScreen m
  )

-- | Ask the user to choose between the constructors of a sum type.
--
-- @
-- data Colour = Red | Green | Blue deriving (Bounded, Enum, Eq, Show)
--
-- instance Chooseable Colour where
--   showChooseable = Data.Text.pack . show
--
-- main :: IO ()
-- main = do
--   favouriteColour <- promptChoice Optional DontConfirm (Proxy :: Proxy Colour) $ "What is your favourite colour?"
--   print favouriteColour
-- @
promptChoice ::
  (result ~ PerRequirement requirement a, Chooseable result, Eq result, MonadIO m) =>
  -- | May they skip the question?
  Requirement requirement ->
  -- | Should they be asked to confirm their answer after selecting it?
  Confirmation ->
  -- | Proxy of type you want to get back
  Proxy a ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m result
promptChoice requirement confirmation pxy prompt =
  liftIO . withTerminal . runTerminalT $
    promptChoiceInternal pxy prompt (initialPromptChoiceState requirement confirmation)

-- | Ask the user to choose between arbitrary options.
--
-- Note: duplicate options will be removed.
promptChoiceFromSet ::
  (ChooseableItem result, Eq result, MonadIO m) =>
  -- | Should they be asked to confirm their answer after selecting it?
  Confirmation ->
  -- | The items from which they can choose (if you don't require an answer, add a "Skip" option)
  NonEmpty result ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m result
promptChoiceFromSet confirmation set prompt =
  liftIO . withTerminal . runTerminalT $
    promptChoiceInternal Proxy prompt (initialPromptChoiceStateFromSet confirmation $ NE.nub set)

-- | Ask the user to choose zero-to-many of the constructors of a sum type.
--
-- @
-- data Colour = Red | Green | Blue deriving (Bounded, Enum, Eq, Show)
--
-- instance Chooseable Colour where
--   showChooseable = Data.Text.pack . show
--
-- main :: IO ()
-- main = do
--   favouriteColours <- promptMultipleChoice (Proxy :: Proxy Colour) $ "What are your favourite colours?"
--   print favouriteColours
-- @
promptMultipleChoice ::
  (Chooseable a, Eq a, MonadIO m) =>
  -- | Proxy of type you want to get back
  Proxy a ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m [a]
promptMultipleChoice pxy prompt =
  liftIO . withTerminal . runTerminalT $
    promptMultipleChoiceInternal pxy prompt initialPromptMultipleChoiceState

-- | Ask the user to choose zero-to-many of some arbitrary options.
--
-- Note: duplicate options will be removed.
promptMultipleChoiceFromSet ::
  (ChooseableItem a, Eq a, MonadIO m) =>
  -- | The items from which they can choose
  NonEmpty a ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m [a]
promptMultipleChoiceFromSet options prompt =
  liftIO
    . withTerminal
    . runTerminalT
    . promptMultipleChoiceInternal Proxy prompt
    $ initialPromptMultipleChoiceStateFromSet options

-- | Ask the user to enter text.
--
-- The answer is stripped of leading and trailing whitespace.
--
-- > promptText Required DontConfirm "What is your name?"
--
-- > promptText Optional RequireConfirmation "What is your greatest fear?"
promptText ::
  (result ~ PerRequirement requirement Text, MonadIO m) =>
  -- | May they skip the question?
  Requirement requirement ->
  -- | Should they be asked to confirm their answer after entering it?
  Confirmation ->
  -- | What should the prompt say? (You need not add a space to the end of this text; one will be added)
  Text ->
  m result
promptText requirement confirmation prompt =
  liftIO . withTerminal . runTerminalT $
    promptTextInternal
      PromptTextState
        { ptsConfirmation = confirmation,
          ptsCurrentInput = "",
          ptsPrompt = prompt,
          ptsRequirement = requirement
        }

-- | Ask the user to enter yes or no.
--
-- The answer is given as a Bool - True for yes, False for no.
--
-- > promptYesNo Required DontConfirm "Do you see light at the end of the road?"
promptYesNo ::
  (result ~ PerRequirement requirement Bool, MonadIO m) =>
  -- | May they skip the question?
  Requirement requirement ->
  -- | Should they be asked to confirm their answer after entering it?
  Confirmation ->
  -- | What should the prompt say? (appends [y/n]: automatically)
  Text ->
  m result
promptYesNo requirement confirmation prompt = promptYesNo' requirement confirmation (prompt <> " [y/n]:")

-- | Ask the user to enter yes or no. If they choose not to answer, use the default instead.
--
-- The answer is given as a Bool - True for yes, False for no.
--
-- > promptYesNo DontConfirm False "Do you see light at the end of the road?"
promptYesNoWithDefault ::
  (MonadIO m) =>
  -- | Should they be asked to confirm their answer after entering it?
  Confirmation ->
  -- | The default response
  Bool ->
  -- | What should the prompt say? (appends [y/n]: with capitalisation of the default automatically)
  Text ->
  m Bool
promptYesNoWithDefault confirmation def prompt = do
  let promptSuffix = if def then " [Y/n]:" else " [y/N]:"
  promptYesNo' Optional confirmation (prompt <> promptSuffix) >>= \case
    Just yesNo -> pure yesNo
    Nothing -> pure def

initialPromptChoiceState ::
  (Chooseable (PerRequirement requirement a)) =>
  Requirement requirement ->
  Confirmation ->
  PromptChoiceState requirement a
initialPromptChoiceState _ confirmation =
  PromptChoiceState
    { pcsConfirmation = confirmation,
      pcsFilter = "",
      pcsInstruction = ChoiceInstructionNormal,
      pcsOptions = universeChooseableNE,
      pcsFilteredOptions = NE.toList universeChooseableNE,
      pcsHoveredOption = Just initialSelectionChooseable
    }

initialPromptChoiceStateFromSet ::
  (ChooseableItem a) =>
  Confirmation ->
  NonEmpty a ->
  PromptChoiceState 'SRequired a
initialPromptChoiceStateFromSet confirmation options =
  PromptChoiceState
    { pcsConfirmation = confirmation,
      pcsFilter = "",
      pcsInstruction = ChoiceInstructionNormal,
      pcsOptions = options,
      pcsFilteredOptions = NE.toList options,
      pcsHoveredOption = Just initialSelectionChooseableItem
    }

initialPromptMultipleChoiceState ::
  (Chooseable a) =>
  PromptMultipleChoiceState a
initialPromptMultipleChoiceState =
  PromptMultipleChoiceState
    { pmcsFilter = "",
      pmcsInstruction = ChoiceInstructionNormal,
      pmcsOptions = universeChooseableNE,
      pmcsFilteredOptions = NE.toList universeChooseableNE,
      pmcsHoveredOption = initialSelectionChooseable,
      pmcsSelectedOptions = []
    }

initialPromptMultipleChoiceStateFromSet ::
  (ChooseableItem a) =>
  NonEmpty a ->
  PromptMultipleChoiceState a
initialPromptMultipleChoiceStateFromSet options =
  PromptMultipleChoiceState
    { pmcsFilter = "",
      pmcsInstruction = ChoiceInstructionNormal,
      pmcsOptions = options,
      pmcsFilteredOptions = NE.toList options,
      pmcsHoveredOption = Just initialSelectionChooseableItem,
      pmcsSelectedOptions = []
    }

promptChoiceInternal ::
  (MonadInput m, Renderable m requirement a) =>
  Proxy a ->
  Text ->
  PromptChoiceState requirement a ->
  m (PerRequirement requirement a)
promptChoiceInternal _ prompt s = do
  linesRendered <- renderPromptChoicePrompt prompt s
  awaitEvent >>= handlePromptChoiceEvent prompt linesRendered s

promptMultipleChoiceInternal ::
  (MonadInput m, Renderable m 'SRequired a) =>
  Proxy a ->
  Text ->
  PromptMultipleChoiceState a ->
  m [a]
promptMultipleChoiceInternal _ prompt s = do
  linesRendered <- renderPromptMultipleChoicePrompt prompt s
  awaitEvent >>= handlePromptMultipleChoiceEvent prompt linesRendered s

promptTextInternal ::
  (MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  PromptTextState requirement ->
  m (PerRequirement requirement Text)
promptTextInternal s = do
  renderPrompt $ ptsPrompt s
  withAttributes [foreground yellow] . putText $ ptsCurrentInput s
  flush
  awaitEvent >>= handlePromptTextEvent s

-- | Renders the prompt for the user and returns the number of lines output
renderPromptChoicePrompt ::
  (Renderable m requirement a) =>
  Text ->
  PromptChoiceState requirement a ->
  m Int
renderPromptChoicePrompt prompt s = do
  renderPromptLine prompt
  linesRendered <- renderChoiceOptionLines s
  renderSummaryLine s linesRendered
  renderInstructionLine instructionText $ pcsInstruction s
  renderFilterLine $ pcsFilter s
  pure $ linesRendered + 4

-- | Renders the prompt for the user and returns the number of lines output
renderPromptMultipleChoicePrompt ::
  (Renderable m 'SRequired a) =>
  Text ->
  PromptMultipleChoiceState a ->
  m Int
renderPromptMultipleChoicePrompt prompt s = do
  renderPromptLine prompt
  linesRendered <- renderMultipleChoiceOptionLines s
  renderSummaryLine s linesRendered
  renderCurrentSelectionsLine s
  renderInstructionLine instructionTextMulti $ pmcsInstruction s
  renderFilterLine $ pmcsFilter s
  pure $ linesRendered + 5

renderPrompt :: (MonadColorPrinter m, MonadFormattingPrinter m) => Text -> m ()
renderPrompt = withAttributes [bold, foreground blue] . putText . (<> " ")

renderPromptLine :: (MonadColorPrinter m, MonadFormattingPrinter m) => Text -> m ()
renderPromptLine = withAttributes [bold, foreground blue] . putTextLn

renderSummaryLine :: (IsPromptChoiceState s, MonadColorPrinter m, MonadFormattingPrinter m) => s -> Int -> m ()
renderSummaryLine s numberOfOptionsRendered = do
  let nAll = ipcsGetNumberOfOptions s
      nFiltered = ipcsGetNumberOfFilteredOptions s
  withAttributes [italic, foreground cyan] . putTextLn $
    T.pack (show nFiltered)
      <> "/"
      <> T.pack (show nAll)
      <> " included by filter, "
      <> T.pack (show numberOfOptionsRendered)
      <> " shown."

renderCurrentSelectionsLine :: (ChooseableItem a, MonadColorPrinter m, MonadFormattingPrinter m) => PromptMultipleChoiceState a -> m ()
renderCurrentSelectionsLine s = do
  withAttributes [italic, foreground cyan] $ putText "Current Selections: "
  withAttributes [bold, italic, foreground cyan]
    . putTextLn
    . T.intercalate ", "
    . fmap chooseableItemText
    $ pmcsSelectedOptions s

renderChoiceOptionLines ::
  forall requirement a m.
  (Renderable m requirement a) =>
  PromptChoiceState requirement a ->
  m Int
renderChoiceOptionLines s = do
  let (hoveredOption, otherOptions) = getVisibleChoiceOptions s
  case hoveredOption of
    Just o -> withAttributes [bold, foreground yellow] . putTextLn . ("* " <>) $ chooseableItemText o
    Nothing -> pure ()
  forM_ otherOptions (withAttributes [foreground yellow] . putTextLn . ("  " <>) . chooseableItemText)
  pure $ length otherOptions + maybe 0 (const 1) hoveredOption

renderMultipleChoiceOptionLines ::
  forall a m.
  (Renderable m 'SRequired a) =>
  PromptMultipleChoiceState a ->
  m Int
renderMultipleChoiceOptionLines s = do
  let (hoveredOption, otherOptions) = getVisibleMultipleChoiceOptions s
  case hoveredOption of
    Just o -> withAttributes [bold, foreground yellow] . putTextLn . ("> " <>) . insertSelectedIndicator o $ chooseableItemText o
    Nothing -> pure ()
  forM_ otherOptions (\o -> withAttributes [foreground yellow] . putTextLn . ("  " <>) . insertSelectedIndicator o $ chooseableItemText o)
  pure $ length otherOptions + maybe 0 (const 1) hoveredOption
  where
    insertSelectedIndicator :: a -> Text -> Text
    insertSelectedIndicator o =
      ( ( if o `elem` pmcsSelectedOptions s
            then "[*] "
            else "[ ] "
        )
          <>
      )

-- | Returns a maximum of 6 options, including the one currently hovered as the first option.
getVisibleChoiceOptions ::
  forall requirement a result.
  (result ~ PerRequirement requirement a, Eq result) =>
  PromptChoiceState requirement a ->
  (Maybe result, [result])
getVisibleChoiceOptions s = case pcsHoveredOption s of
  Just o | o `elem` filteredOptions -> (Just o, take 5 $ filter (/= o) filteredOptions)
  _ -> (Nothing, take 6 filteredOptions)
  where
    maxNumberOfOptions :: Int
    maxNumberOfOptions = length $ pcsFilteredOptions s
    filteredOptions :: [result]
    filteredOptions = take maxNumberOfOptions . dropWhile isNotHoveredOption . cycle $ pcsFilteredOptions s
    isNotHoveredOption :: result -> Bool
    isNotHoveredOption o = case pcsHoveredOption s of
      Just target -> o /= target
      Nothing -> False

-- | Returns a maximum of 6 options, including the one currently hovered as the first option.
getVisibleMultipleChoiceOptions :: forall a. (Eq a) => PromptMultipleChoiceState a -> (Maybe a, [a])
getVisibleMultipleChoiceOptions s = case pmcsHoveredOption s of
  Just o | o `elem` filteredOptions -> (Just o, take 5 $ filter (/= o) filteredOptions)
  _ -> (Nothing, take 6 filteredOptions)
  where
    maxNumberOfOptions :: Int
    maxNumberOfOptions = length $ pmcsFilteredOptions s
    filteredOptions :: [a]
    filteredOptions = take maxNumberOfOptions . maybe id (\o -> dropWhile (/= o)) (pmcsHoveredOption s) . cycle $ pmcsFilteredOptions s

renderInstructionLine :: (MonadFormattingPrinter m) => (a -> Text) -> a -> m ()
renderInstructionLine instructionToText = withAttributes [italic] . putTextLn . instructionToText

renderFilterLine :: (MonadFormattingPrinter m, MonadColorPrinter m) => Text -> m ()
renderFilterLine f = do
  withAttributes [bold, foreground magenta] $ putText "> "
  putText f
  flush

handlePromptChoiceEvent ::
  forall m requirement a.
  (Renderable m requirement a, MonadInput m) =>
  Text ->
  Int ->
  PromptChoiceState requirement a ->
  Either Interrupt Event ->
  m (PerRequirement requirement a)
handlePromptChoiceEvent prompt linesRendered s e = do
  moveCursorUp (linesRendered - 1)
  deleteLines linesRendered
  case e of
    Left Interrupt -> liftIO exitFailure
    Right (KeyEvent key modifiers) -> case key of
      ArrowKey direction -> case direction of
        Upwards ->
          goAgain $
            s
              { pcsHoveredOption =
                  headViaNonEmpty
                    $ drop 1
                      . dropWhile (maybe (const False) (/=) (pcsHoveredOption s))
                      . cycle
                      . reverse
                    $ pcsFilteredOptions s
              }
        Downwards -> goAgain $ s {pcsHoveredOption = headViaNonEmpty . snd $ getVisibleChoiceOptions s}
        _ -> goAgain s
      BackspaceKey ->
        goAgain $
          s
            & _pcsFilter %~ T.dropEnd 1
            & applyFilter
            & _pcsInstruction %~ \case
              ChoiceInstructionNoOptionSelected -> ChoiceInstructionNormal
              currentInstruction -> currentInstruction
      EnterKey -> case pcsHoveredOption s of
        Just o -> case pcsConfirmation s of
          RequireConfirmation -> confirmSelection prompt o (chooseableItemText o) (goAgain s)
          DontConfirm -> do
            renderPrompt prompt
            withAttributes [bold, foreground yellow] . putTextLn $ chooseableItemText o
            pure o
        Nothing -> goAgain $ s {pcsInstruction = ChoiceInstructionNoOptionSelected}
      CharKey c ->
        if modifiers == mempty
          then
            if null (pcsFilteredOptions s)
              then goAgain $ s {pcsInstruction = ChoiceInstructionNoOptionSelected}
              else goAgain $ s & _pcsFilter %~ flip T.snoc c & applyFilter
          else goAgain s
      _ -> goAgain s
    _ -> goAgain s
  where
    goAgain :: PromptChoiceState requirement a -> m (PerRequirement requirement a)
    goAgain = promptChoiceInternal Proxy prompt

handlePromptMultipleChoiceEvent ::
  forall m a.
  (Renderable m 'SRequired a, MonadInput m) =>
  Text ->
  Int ->
  PromptMultipleChoiceState a ->
  Either Interrupt Event ->
  m [a]
handlePromptMultipleChoiceEvent prompt linesRendered s e = do
  moveCursorUp (linesRendered - 1)
  deleteLines linesRendered
  case e of
    Left Interrupt -> liftIO exitFailure
    Right (KeyEvent key modifiers) -> case key of
      ArrowKey direction -> case direction of
        Upwards ->
          goAgain $
            s
              { pmcsHoveredOption =
                  headViaNonEmpty
                    $ drop 1
                      . dropWhile (maybe (const False) (/=) (pmcsHoveredOption s))
                      . cycle
                      . reverse
                    $ pmcsFilteredOptions s
              }
        Downwards -> goAgain $ s {pmcsHoveredOption = headViaNonEmpty . snd $ getVisibleMultipleChoiceOptions s}
        _ -> goAgain s
      BackspaceKey ->
        goAgain $
          s
            & _pmcsFilter %~ T.dropEnd 1
            & applyFilterMultiple
            & _pmcsInstruction %~ \case
              ChoiceInstructionNoOptionSelected -> ChoiceInstructionNormal
              currentInstruction -> currentInstruction
      EnterKey ->
        confirmSelection
          prompt
          (pmcsSelectedOptions s)
          ( if null (pmcsSelectedOptions s)
              then "[nothing]"
              else T.intercalate ", " . fmap chooseableItemText $ pmcsSelectedOptions s
          )
          (goAgain s)
      CharKey c
        | c == ' ' ->
            goAgain $
              s
                & _pmcsSelectedOptions
                  %~ ( \opts -> case pmcsHoveredOption s of
                         Just hovered ->
                           if hovered `elem` opts
                             then filter (/= hovered) opts
                             else hovered : opts
                         Nothing -> opts
                     )
      CharKey c ->
        if modifiers == mempty
          then
            if null (pmcsFilteredOptions s)
              then goAgain $ s {pmcsInstruction = ChoiceInstructionNoOptionSelected}
              else goAgain $ s & _pmcsFilter %~ flip T.snoc c & applyFilterMultiple
          else goAgain s
      _ -> goAgain s
    _ -> goAgain s
  where
    goAgain :: PromptMultipleChoiceState a -> m [a]
    goAgain = promptMultipleChoiceInternal Proxy prompt

handlePromptTextEvent ::
  (MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  PromptTextState requirement ->
  Either Interrupt Event ->
  m (PerRequirement requirement Text)
handlePromptTextEvent s e = do
  deleteLines 1
  case e of
    Left Interrupt -> liftIO exitFailure
    Right (KeyEvent key modifiers) -> case key of
      BackspaceKey -> promptTextInternal $ s & _ptsCurrentInput %~ T.dropEnd 1
      EnterKey -> confirmTextInput s $ promptTextInternal s
      CharKey c ->
        if modifiers == mempty
          then promptTextInternal $ s & _ptsCurrentInput %~ (`T.snoc` c)
          else case c of
            'U'
              | modifiers == ctrlKey ->
                  promptTextInternal $ s {ptsCurrentInput = ""}
            'W'
              | modifiers == ctrlKey ->
                  promptTextInternal $ s & _ptsCurrentInput %~ (T.dropWhile C.isSpace . T.dropWhileEnd (not . C.isSpace))
            _ -> promptTextInternal s
      _ -> promptTextInternal s
    _ -> promptTextInternal s

-- | Applies the filter in `_pcsFilter` to set `_pcsOptions` and update `_pcsHoveredOption`
-- if the previously hovered option is no longer included in the filter.
applyFilter ::
  (result ~ PerRequirement requirement a, ChooseableItem result, Eq result) =>
  PromptChoiceState requirement a ->
  PromptChoiceState requirement a
applyFilter s =
  let newFilteredOptions =
        filter
          ((T.toLower (pcsFilter s) `T.isInfixOf`) . T.toLower . chooseableItemText)
          (NE.toList $ pcsOptions s)
   in s
        & _pcsFilteredOptions .~ newFilteredOptions
        & _pcsHoveredOption %~ \case
          Just o | o `elem` newFilteredOptions -> Just o
          _ -> headViaNonEmpty newFilteredOptions

-- | Applies the filter in `_pcsFilter` to set `_pcsOptions` and update `_pcsHoveredOption`
-- if the previously hovered option is no longer included in the filter.
applyFilterMultiple ::
  (ChooseableItem a, Eq a) =>
  PromptMultipleChoiceState a ->
  PromptMultipleChoiceState a
applyFilterMultiple s =
  let newFilteredOptions =
        filter
          ((T.toLower (pmcsFilter s) `T.isInfixOf`) . T.toLower . chooseableItemText)
          (NE.toList $ pmcsOptions s)
   in s
        & _pmcsFilteredOptions .~ newFilteredOptions
        & _pmcsHoveredOption %~ \case
          Just o | o `elem` newFilteredOptions -> Just o
          _ -> headViaNonEmpty newFilteredOptions

confirmSelection ::
  (MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  Text ->
  a ->
  Text ->
  m a ->
  m a
confirmSelection prompt selection selectionText onCancel = do
  renderPromptLine prompt
  withAttributes [bold, foreground cyan] $ putText "You have chosen: "
  withAttributes [bold, foreground yellow] $ putTextLn selectionText
  withAttributes [italic] $ putTextLn "Press enter again to confirm, or escape to go back."
  e <- awaitEvent
  moveCursorUp 3 *> deleteLines 4
  case e of
    Left Interrupt -> liftIO exitFailure
    Right (KeyEvent EnterKey _) -> do
      renderPrompt prompt
      withAttributes [bold, foreground yellow] $ putTextLn selectionText
      pure selection
    Right (KeyEvent EscapeKey _) -> onCancel
    _ -> confirmSelection prompt selection selectionText onCancel

confirmTextInput ::
  forall m requirement.
  (MonadColorPrinter m, MonadFormattingPrinter m, MonadInput m, MonadScreen m) =>
  PromptTextState requirement ->
  m (PerRequirement requirement Text) ->
  m (PerRequirement requirement Text)
confirmTextInput s onCancel =
  if T.null selection && responseIsRequired
    then onCancel
    else case ptsConfirmation s of
      RequireConfirmation -> do
        writePromptWithResponse
        withAttributes [italic] $ putTextLn "Press enter again to confirm, or escape to go back."
        e <- awaitEvent
        moveCursorUp 2 *> deleteLines 3
        case e of
          Left Interrupt -> liftIO exitFailure
          Right (KeyEvent EnterKey _) -> do
            writePromptWithResponse
            returnCurrentInput
          Right (KeyEvent EscapeKey _) -> onCancel
          _ -> confirmTextInput s onCancel
      DontConfirm -> writePromptWithResponse *> returnCurrentInput
  where
    (prompt, selection) = (ptsPrompt s, T.strip $ ptsCurrentInput s)
    responseIsRequired = case ptsRequirement s of Required -> True; Optional -> False
    writePromptWithResponse :: m ()
    writePromptWithResponse = do
      renderPrompt prompt
      if T.null selection
        then withAttributes [foreground cyan] $ putTextLn "(You did not enter a response)"
        else withAttributes [bold, foreground yellow] $ putTextLn selection
    returnCurrentInput :: m (PerRequirement requirement Text)
    returnCurrentInput = pure $ case (ptsRequirement s, T.null selection) of
      (Required, _) -> selection
      (Optional, True) -> Nothing
      (Optional, False) -> Just selection

-- | Internal handler for promptYesNo, without appending prompt.
promptYesNo' ::
  (result ~ PerRequirement requirement Bool, MonadIO m) =>
  -- | May they skip the question?
  Requirement requirement ->
  -- | Should they be asked to confirm their answer after entering it?
  Confirmation ->
  -- | What should the prompt say? (does not append [y/n]: automatically)
  Text ->
  m result
promptYesNo' requirement confirmation prompt = do
  response <- promptText requirement confirmation prompt
  case requirement of
    Required -> case parseYesNo response of
      Just yesNo -> pure yesNo
      Nothing -> do
        liftIO . withTerminal . runTerminalT $ putErrorLn "You must answer yes or no."
        promptYesNo' requirement confirmation prompt
    Optional ->
      case response of
        Just res -> case parseYesNo res of
          Just yesNo -> pure $ Just yesNo
          Nothing -> do
            liftIO . withTerminal . runTerminalT $ putErrorLn "Answer yes or no, or leave your answer blank to skip the question."
            promptYesNo' requirement confirmation prompt
        Nothing ->
          pure Nothing
  where
    parseYesNo :: Text -> Maybe Bool
    parseYesNo =
      \case
        "y" -> Just True
        "yes" -> Just True
        "n" -> Just False
        "no" -> Just False
        _ -> Nothing
        . T.toLower

-- | Outputs an error message
putErrorLn :: (MonadColorPrinter m, MonadFormattingPrinter m) => Text -> m ()
putErrorLn msg = withAttributes [foreground red, bold] (putTextLn msg) >> flush

-- | Applies the given attributes to the terminal, runs the action,
-- then resets the terminal's attributes.
withAttributes :: (MonadMarkupPrinter m, Foldable t) => t (Attribute m) -> m a -> m a
withAttributes as action = do
  resetAttributes
  forM_ as setAttribute
  result <- action
  resetAttributes
  pure result

-- | Returns the first element, or `Nothing` if the list is empty.
headViaNonEmpty :: [a] -> Maybe a
headViaNonEmpty =
  \case
    Just (x :| _) -> Just x
    Nothing -> Nothing
    . NE.nonEmpty
