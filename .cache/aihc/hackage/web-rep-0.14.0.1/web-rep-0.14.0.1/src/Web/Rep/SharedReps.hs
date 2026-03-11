{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Various SharedRep instances for common html input elements.
module Web.Rep.SharedReps
  ( repInput,
    repMessage,
    sliderI,
    slider,
    sliderV,
    sliderVI,
    dropdown,
    dropdownMultiple,
    datalist,
    dropdownSum,
    colorPicker,
    textbox,
    textarea,
    checkbox,
    toggle,
    toggle_,
    button,
    chooseFile,
    maybeRep,
    accordionList,
    listMaybeRep,
    listRep,
    readTextbox,
    readTextbox_,
    defaultListLabels,
    repChoice,
    subtype,
    selectItems,
    repItemsSelect,
  )
where

import Box.Codensity ()
import Control.Monad
import Control.Monad.State.Lazy
import Data.Biapplicative
import Data.Bool
import Data.ByteString (ByteString, intercalate)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe
import FlatParse.Basic hiding (take)
import MarkupParse
import Optics.Core hiding (element)
import Optics.Zoom
import Web.Rep.Bootstrap
import Web.Rep.Html.Input
import Web.Rep.Internal.FlatParse
import Web.Rep.Shared
import Prelude as P

-- $setup
-- >>> :set -XOverloadedStrings

-- | Create a sharedRep from an Input.
repInput ::
  (Monad m) =>
  -- | Parser
  (ByteString -> Either ByteString a) ->
  -- | Printer
  (a -> ByteString) ->
  -- | 'Input' type
  Input a ->
  -- | initial value
  a ->
  SharedRep m a
repInput p pr i = register p pr (\n v -> markupInput pr $ #inputVal .~ v $ #inputId .~ n $ i)

-- | Like 'repInput', but does not put a value into the HashMap on instantiation, consumes the value when found in the HashMap, and substitutes a default on lookup failure
repMessage :: (Monad m) => (ByteString -> Either ByteString a) -> (a -> ByteString) -> Input a -> a -> a -> SharedRep m a
repMessage p pr i def a =
  message p (\n v -> markupInput pr $ #inputVal .~ v $ #inputId .~ n $ i) a def

-- | double slider
--
-- For Example, a slider between 0 and 1 with a step of 0.01 and a default value of 0.3 is:
--
-- > :t slider (Just "label") 0 1 0.01 0.3
-- slider (Just "label") 0 1 0.01 0.3 :: Monad m => SharedRep m Double
slider ::
  (Monad m) =>
  Maybe ByteString ->
  Double ->
  Double ->
  Double ->
  Double ->
  SharedRep m Double
slider label l u s v =
  repInput
    (runParserEither double)
    (strToUtf8 . show)
    (Input v label mempty (Slider [Attr "min" (strToUtf8 $ show l), Attr "max" (strToUtf8 $ show u), Attr "step" (strToUtf8 $ show s)]))
    v

-- | double slider with shown value
--
-- For Example, a slider between 0 and 1 with a step of 0.01 and a default value of 0.3 is:
--
-- > :t slider (Just "label") 0 1 0.01 0.3
-- slider (Just "label") 0 1 0.01 0.3 :: Monad m => SharedRep m Double
sliderV ::
  (Monad m) =>
  Maybe ByteString ->
  Double ->
  Double ->
  Double ->
  Double ->
  SharedRep m Double
sliderV label l u s v =
  repInput
    (runParserEither double)
    (strToUtf8 . show)
    (Input v label mempty (SliderV [Attr "min" (strToUtf8 $ show l), Attr "max" (strToUtf8 $ show u), Attr "step" (strToUtf8 $ show s)]))
    v

-- | integral slider
--
-- For Example, a slider between 0 and 1000 with a step of 10 and a default value of 300 is:
--
-- > :t sliderI (Just "label") 0 1000 10 300
-- sliderI (Just "label") 0 1000 10 300
--   :: (Monad m, ToHtml a, P.Integral a, Show a) => SharedRep m a
sliderI ::
  (Monad m, P.Integral a, ToByteString a) =>
  Maybe ByteString ->
  a ->
  a ->
  a ->
  a ->
  SharedRep m a
sliderI label l u s v =
  repInput
    (runParserEither (fromIntegral <$> int))
    toByteString
    (Input v label mempty (Slider [Attr "min" (toByteString l), Attr "max" (toByteString u), Attr "step" (toByteString s)]))
    v

-- | integral slider with shown value
sliderVI ::
  (Monad m, P.Integral a, ToByteString a) =>
  Maybe ByteString ->
  a ->
  a ->
  a ->
  a ->
  SharedRep m a
sliderVI label l u s v =
  repInput
    (runParserEither (fromIntegral <$> int))
    toByteString
    (Input v label mempty (SliderV [Attr "min" (toByteString l), Attr "max" (toByteString u), Attr "step" (toByteString s)]))
    v

-- | textbox classique
--
-- > :t textbox (Just "label") "some text"
-- textbox (Just "label") "some text" :: Monad m => SharedRep m ByteString
textbox :: (Monad m) => Maybe ByteString -> ByteString -> SharedRep m ByteString
textbox label v =
  repInput
    (runParserEither takeRest)
    id
    (Input v label mempty TextBox)
    v

-- | textbox that only updates on focusout
textbox' :: (Monad m) => Maybe ByteString -> ByteString -> SharedRep m ByteString
textbox' label v =
  repInput
    (runParserEither takeRest)
    id
    (Input v label mempty TextBox')
    v

-- | textarea input element, specifying number of rows.
textarea :: (Monad m) => Int -> Maybe ByteString -> ByteString -> SharedRep m ByteString
textarea rows label v =
  repInput
    (runParserEither takeRest)
    id
    (Input v label mempty (TextArea rows))
    v

-- | Non-typed hex color input
colorPicker :: (Monad m) => Maybe ByteString -> ByteString -> SharedRep m ByteString
colorPicker label v =
  repInput
    (runParserEither takeRest)
    id
    (Input v label mempty ColorPicker)
    v

-- | dropdown box
dropdown ::
  (Monad m) =>
  -- | parse an a from ByteString
  (ByteString -> Either ByteString a) ->
  -- | print an a to ByteString
  (a -> ByteString) ->
  -- | label suggestion
  Maybe ByteString ->
  -- | list of dropbox elements (as text)
  [ByteString] ->
  -- | initial value
  a ->
  SharedRep m a
dropdown p pr label opts v =
  repInput
    p
    pr
    (Input v label mempty (Dropdown opts))
    v

-- | dropdown box with multiple selections
dropdownMultiple ::
  (Monad m) =>
  -- | parse an a from ByteString
  Parser ByteString a ->
  -- | print an a to ByteString
  (a -> ByteString) ->
  -- | label suggestion
  Maybe ByteString ->
  -- | list of dropbox elements (as text)
  [ByteString] ->
  -- | initial value
  [a] ->
  SharedRep m [a]
dropdownMultiple p pr label opts vs =
  repInput
    (runParserEither (sep comma p))
    (intercalate "," . fmap pr)
    (Input vs label mempty (DropdownMultiple opts ','))
    vs

-- | a datalist input
datalist :: (Monad m) => Maybe ByteString -> [ByteString] -> ByteString -> ByteString -> SharedRep m ByteString
datalist label opts v id'' =
  repInput
    (runParserEither takeRest)
    (strToUtf8 . show)
    (Input v label mempty (Datalist opts id''))
    v

-- | A dropdown box designed to help represent a haskell sum type.
dropdownSum ::
  (Monad m) =>
  (ByteString -> Either ByteString a) ->
  (a -> ByteString) ->
  Maybe ByteString ->
  [ByteString] ->
  a ->
  SharedRep m a
dropdownSum p pr label opts v =
  repInput
    p
    pr
    (Input v label mempty (DropdownSum opts))
    v

-- | A checkbox input.
checkbox :: (Monad m) => Maybe ByteString -> Bool -> SharedRep m Bool
checkbox label v =
  repInput
    (runParserEither ((== "true") <$> takeRest))
    (bool "false" "true")
    (Input v label mempty (Checkbox v))
    v

-- | a toggle button
toggle :: (Monad m) => Maybe ByteString -> Bool -> SharedRep m Bool
toggle label v =
  repInput
    (runParserEither ((== "true") <$> takeRest))
    (bool "false" "true")
    (Input v label mempty (Toggle v))
    v

-- | a toggle button, with no label
toggle_ :: (Monad m) => Maybe ByteString -> Bool -> SharedRep m Bool
toggle_ label v =
  repInput
    (runParserEither ((== "true") <$> takeRest))
    (bool "false" "true")
    (Input v label mempty (Toggle v))
    v

-- | a button
button :: (Monad m) => Maybe ByteString -> SharedRep m Bool
button label =
  repMessage
    (const (Right True))
    (bool "false" "true")
    (Input False label mempty Button)
    False
    False

-- | filename input
chooseFile :: (Monad m) => Maybe ByteString -> ByteString -> SharedRep m ByteString
chooseFile label v =
  repInput
    (runParserEither takeRest)
    (strToUtf8 . show)
    (Input v label mempty ChooseFile)
    v

-- | Represent a Maybe using a checkbox.
--
-- Hides the underlying content on Nothing
maybeRep ::
  (Monad m) =>
  Maybe ByteString ->
  Bool ->
  SharedRep m a ->
  SharedRep m (Maybe a)
maybeRep label st sa = SharedRep $ do
  id' <- zoom _1 (genNamePre "maybe")
  unshare $ bimap (hmap id') mmap (checkboxShow label id' st) <<*>> sa
  where
    hmap id' a b =
      cardify
        (a, [])
        Nothing
        ( element
            "div"
            [ Attr "id" id',
              Attr
                "style"
                ("display:" <> bool "none" "block" st)
            ]
            b,
          [Attr "style" "padding-top: 0.25rem; padding-bottom: 0.25rem;"]
        )
    mmap a b = bool Nothing (Just b) a

checkboxShow :: (Monad m) => Maybe ByteString -> ByteString -> Bool -> SharedRep m Bool
checkboxShow label id' v =
  SharedRep $ do
    name <- zoom _1 genName
    zoom _2 (modify (HashMap.insert name (bool "false" "true" v)))
    pure $
      Rep
        (markupInput (strToUtf8 . show) (Input v label name (Checkbox v)) <> scriptToggleShow name id')
        ( \s ->
            ( s,
              join $
                maybe
                  (Left "HashMap.lookup failed")
                  (Right . runParserEither ((== "true") <$> takeRest))
                  (HashMap.lookup name s)
            )
        )

-- | toggle show/hide
scriptToggleShow :: ByteString -> ByteString -> Markup
scriptToggleShow checkName toggleId =
  elementc
    "script"
    []
    ( "$('#"
        <> checkName
        <> "').on('change', (function(){"
        <> "  var vis = this.checked ? \"block\" : \"none\";"
        <> "  document.getElementById(\""
        <> toggleId
        <> "\").style.display = vis;"
        <> "}));"
    )

-- | A (fixed-size) list represented in html as an accordion card
-- A major restriction of the library is that a 'SharedRep' does not have a Monad instance. In practice, this means that the external representation of lists cannot have a dynamic size.
accordionList :: (Monad m) => Maybe ByteString -> ByteString -> Maybe ByteString -> (ByteString -> a -> SharedRep m a) -> [ByteString] -> [a] -> SharedRep m [a]
accordionList title prefix open srf labels as = SharedRep $ do
  (Rep h fa) <-
    unshare $
      first (accordion prefix open . zip labels) $
        foldr
          (\a x -> bimap (:) (:) a <<*>> x)
          (pure [])
          (zipWith srf labels as)
  h' <- zoom _1 h
  pure (Rep (maybe mempty (elementc "h5" []) title <> h') fa)

-- | A (fixed-sized) list of (Bool, a) tuples.
accordionBoolList :: (Monad m) => Maybe ByteString -> ByteString -> (a -> SharedRep m a) -> (Bool -> SharedRep m Bool) -> [ByteString] -> [(Bool, a)] -> SharedRep m [(Bool, a)]
accordionBoolList title prefix bodyf checkf labels xs = SharedRep $ do
  (Rep h fa) <-
    unshare $
      first
        ( accordionChecked prefix
            . zipWith (\l (ch, a) -> (l, a, ch)) labels
        )
        ( foldr
            ((\a x -> bimap (:) (:) a <<*>> x) . (\(ch, a) -> bimap (,) (,) (checkf ch) <<*>> bodyf a))
            (pure [])
            xs
        )
  h' <- zoom _1 h
  pure (Rep (maybe mempty (elementc "h5" []) title <> h') fa)

-- | A fixed-sized list of Maybe a\'s
listMaybeRep :: (Monad m) => Maybe ByteString -> ByteString -> (ByteString -> Maybe a -> SharedRep m (Maybe a)) -> Int -> [a] -> SharedRep m [Maybe a]
listMaybeRep t p srf n as =
  accordionList t p Nothing srf (defaultListLabels n) (take n ((Just <$> as) <> repeat Nothing))

-- | A SharedRep of [a].  Due to the applicative nature of the bridge, the size of lists has to be fixed on construction.  listRep is a workaround for this, to enable some form of dynamic sizing.
listRep ::
  (Monad m) =>
  Maybe ByteString ->
  ByteString ->
  -- | name prefix (should be unique)
  (Bool -> SharedRep m Bool) ->
  -- | Bool Rep
  (a -> SharedRep m a) ->
  -- | a Rep
  Int ->
  -- | maximum length of list
  a ->
  -- | default value for new rows
  [a] ->
  -- | initial values
  SharedRep m [a]
listRep t p brf srf n defa as =
  second (mconcat . fmap (\(b, a) -> bool [] [a] b)) $
    accordionBoolList
      t
      p
      srf
      brf
      (defaultListLabels n)
      (take n (((True,) <$> as) <> repeat (False, defa)))

-- | A sensible default for the accordion row labels for a list
defaultListLabels :: Int -> [ByteString]
defaultListLabels n = (\x -> "[" <> strToUtf8 (show x) <> "]") <$> [0 .. n] :: [ByteString]

-- | Parse from a textbox
--
-- Uses focusout so as not to spam the reader.
readTextbox :: (Monad m, Read a, ToByteString a) => Maybe ByteString -> a -> SharedRep m (Either ByteString a)
readTextbox label v = parsed . utf8ToStr <$> textbox' label (toByteString v)
  where
    parsed str =
      case reads str of
        [(a, "")] -> Right a
        _badRead -> Left (strToUtf8 str)

-- | Parse from a textbox
--
-- Uses focusout so as not to spam the reader.
readTextbox_ :: (Monad m, Read a, ToByteString a) => Maybe ByteString -> a -> SharedRep m a
readTextbox_ label v = parsed . utf8ToStr <$> textbox' label (toByteString v)
  where
    parsed str =
      case reads str of
        [(a, "")] -> a
        _badRead -> error "bad read"

-- | Dropdown representation of a multi-element list.
repChoice :: (Monad m) => Int -> [(ByteString, SharedRep m a)] -> SharedRep m a
repChoice initt xs =
  bimap hmap mmap dd
    <<*>> foldr (\x a -> bimap (:) (:) x <<*>> a) (pure []) cs
  where
    ts = fst <$> xs
    cs = snd <$> xs
    dd = dropdownSum (runParserEither takeRest) id Nothing ts t0
    t0 = ts List.!! initt
    hmap dd' cs' =
      element
        "div"
        []
        (dd' <> mconcat (zipWith (addSubtype t0) ts cs'))
    mmap dd' cs' = maybe (List.head cs') (cs' List.!!) (List.elemIndex dd' ts)

-- | select test keys from a Map
selectItems :: [ByteString] -> HashMap.HashMap ByteString a -> [(ByteString, a)]
selectItems ks m =
  HashMap.toList $
    HashMap.filterWithKey (\k _ -> k `elem` ks) m

-- | rep of multiple items list
repItemsSelect :: (Monad m) => [ByteString] -> [ByteString] -> SharedRep m [ByteString]
repItemsSelect initial full =
  dropdownMultiple (strToUtf8 <$> some (satisfy (`notElem` ([','] :: [Char])))) id (Just "items") full initial

-- | subtype Html class.
subtype :: ByteString -> ByteString -> [Attr]
subtype origt t =
  [ Attr "class" "subtype ",
    Attr "data_sumtype" t,
    Attr "style" ("display:" <> bool "block" "none" (origt /= t))
  ]

addSubtype :: ByteString -> ByteString -> Markup -> Markup
addSubtype origt t (Markup trees) =
  Markup $
    fmap (fmap (\toke -> fromMaybe toke $ addAttrs (subtype origt t) toke)) trees
