{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | this program based on Yesod.Form.Bootstrap3 of yesod-form
-- yesod-form under MIT license, author is Michael Snoyman <michael@snoyman.com>

module Yesod.Form.Bootstrap4
  ( renderBootstrap4
  , BootstrapFormLayout(..)
  , BootstrapGridOptions(..)
  , bfs
  , bfsFile
  , withPlaceholder
  , withAutofocus
  , withLargeInput
  , withSmallInput
  , bootstrapSubmit
  , mbootstrapSubmit
  , BootstrapSubmit(..)
  ) where

import           Control.Arrow                 (second)
import           Data.String                   (IsString (..))
import           Data.Text                     (Text)
import qualified Data.Text.Lazy                as TL
import           Text.Blaze.Html.Renderer.Text
import           Yesod.Core
import           Yesod.Form

bfs :: RenderMessage site msg => msg -> FieldSettings site
bfs msg
  = FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", "form-control")]

bfsFile :: RenderMessage site msg => msg -> FieldSettings site
bfsFile msg
  = FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", "form-control-file")]

withPlaceholder :: Text -> FieldSettings site -> FieldSettings site
withPlaceholder placeholder fs = fs { fsAttrs = newAttrs }
  where newAttrs = ("placeholder", placeholder) : fsAttrs fs

-- | Add an autofocus attribute to a field.
withAutofocus :: FieldSettings site -> FieldSettings site
withAutofocus fs = fs { fsAttrs = newAttrs }
  where newAttrs = ("autofocus", "autofocus") : fsAttrs fs

-- | Add the @input-lg@ CSS class to a field.
withLargeInput :: FieldSettings site -> FieldSettings site
withLargeInput fs = fs { fsAttrs = newAttrs }
  where newAttrs = addClass "form-control-lg" (fsAttrs fs)

-- | Add the @input-sm@ CSS class to a field.
withSmallInput :: FieldSettings site -> FieldSettings site
withSmallInput fs = fs { fsAttrs = newAttrs }
  where newAttrs = addClass "form-control-sm" (fsAttrs fs)

data BootstrapGridOptions = ColXs !Int | ColSm !Int | ColMd !Int | ColLg !Int | ColXl !Int
  deriving (Eq, Ord, Show, Read)

toColumn :: BootstrapGridOptions -> String
toColumn (ColXs columns) = "col-xs-" ++ show columns
toColumn (ColSm columns) = "col-sm-" ++ show columns
toColumn (ColMd columns) = "col-md-" ++ show columns
toColumn (ColLg columns) = "col-lg-" ++ show columns
toColumn (ColXl columns) = "col-xl-" ++ show columns

toOffset :: BootstrapGridOptions -> String
toOffset (ColXs columns) = "col-xs-offset-" ++ show columns
toOffset (ColSm columns) = "col-sm-offset-" ++ show columns
toOffset (ColMd columns) = "col-md-offset-" ++ show columns
toOffset (ColLg columns) = "col-lg-offset-" ++ show columns
toOffset (ColXl columns) = "col-Xl-offset-" ++ show columns

addGO :: BootstrapGridOptions -> BootstrapGridOptions -> BootstrapGridOptions
addGO (ColXs a) (ColXs b) = ColXs (a+b)
addGO (ColSm a) (ColSm b) = ColSm (a+b)
addGO (ColMd a) (ColMd b) = ColMd (a+b)
addGO (ColLg a) (ColLg b) = ColLg (a+b)
addGO a b                 | a > b = addGO b a
addGO (ColXs a) other     = addGO (ColSm a) other
addGO (ColSm a) other     = addGO (ColMd a) other
addGO (ColMd a) other     = addGO (ColLg a) other
addGO _     _             = error "Yesod.Form.Bootstrap.addGO: never here"

-- | The layout used for the bootstrap form.
data BootstrapFormLayout = BootstrapBasicForm | BootstrapInlineForm |
  BootstrapHorizontalForm
  { bflLabelOffset :: !BootstrapGridOptions
  , bflLabelSize   :: !BootstrapGridOptions
  , bflInputOffset :: !BootstrapGridOptions
  , bflInputSize   :: !BootstrapGridOptions
  }
  deriving (Eq, Ord, Show, Read)

-- | Render the given form using Bootstrap v4 conventions.
renderBootstrap4 :: Monad m => BootstrapFormLayout -> FormRender m a
renderBootstrap4 formLayout aform fragment = do
  (res, views') <- aFormToForm aform
  let views = views' []
      widget = [whamlet|
#{fragment}
$forall view <- views
  $if inputTypeBoolOrCheckBox view
    ^{renderCheckInput view}
  $else
    ^{renderGroupInput view formLayout}
|]
  return (res, widget)

-- FIXME: `.form-check-input`を`input`につける方法がわからない
renderCheckInput :: FieldView site -> WidgetFor site ()
renderCheckInput view = [whamlet|
<div .form-check (fvErrors view):.is-invalid>
  ^{fvInput view}
  <label .form-check-label for=#{fvId view}>
  ^{helpWidget view}
|]

renderGroupInput :: FieldView site -> BootstrapFormLayout -> WidgetFor site ()
renderGroupInput view formLayout = [whamlet|
$case formLayout
  $of BootstrapBasicForm
    $if fvId view /= bootstrapSubmitId
      <label for=#{fvId view}>#{fvLabel view}
    ^{fvInput view}
    ^{helpWidget view}
  $of BootstrapInlineForm
    $if fvId view /= bootstrapSubmitId
      <label .sr-only for=#{fvId view}>#{fvLabel view}
    ^{fvInput view}
    ^{helpWidget view}
  $of BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    $if fvId view /= bootstrapSubmitId
      <div .row>
        <label
          .#{toOffset labelOffset}
          .#{toColumn labelSize}
          for=#{fvId view}>#{fvLabel view}
        <div .#{toOffset inputOffset} .#{toColumn inputSize}>
          ^{fvInput view}
          ^{helpWidget view}
    $else
      <div
        .#{toOffset (addGO inputOffset (addGO labelOffset labelSize))}
        .#{toColumn inputSize}>
        ^{fvInput view}
        ^{helpWidget view}
|]

-- | 入力されたフィールドがcheck形式である必要があるか判定する
-- HTMLの内容を`Monad`の範囲で見る方法が分からなかったため,ワークアラウンドとしてlabelの内容を見て判断します
inputTypeBoolOrCheckBox :: FieldView site -> Bool
inputTypeBoolOrCheckBox FieldView{fvLabel}
  = let textLabel = renderHtml fvLabel
    in "radio" `TL.isInfixOf` textLabel || "checkbox" `TL.isInfixOf` textLabel

-- | (Internal) Render a help widget for tooltips and errors.
-- .invalid-feedbackを必ず表示する
-- bootstrap 4.1の書式ではinputがerrorでなければエラーメッセージが出ませんが
-- yesod-formのAPIではfvInput自体を弄るのが困難ですし
-- yesod-formのAPI上fvErrorsが存在する時は常にエラーメッセージは表示させるべきなので汚いやり方ですが
-- styleを上書きして常に表示します
helpWidget :: FieldView site -> WidgetFor site ()
helpWidget view = [whamlet|
$maybe err <- fvErrors view
  <div .invalid-feedback style="display: block;">
    #{err}
$maybe tt <- fvTooltip view
  <small .form-text .text-muted>
    #{tt}
|]

-- | How the 'bootstrapSubmit' button should be rendered.
data BootstrapSubmit msg =
  BootstrapSubmit
  { bsValue   :: msg -- ^ The text of the submit button.
  , bsClasses :: Text -- ^ Classes added to the @\<button>@.
  , bsAttrs   :: [(Text, Text)] -- ^ Attributes added to the @\<button>@.
  } deriving (Eq, Ord, Show, Read)

instance IsString msg => IsString (BootstrapSubmit msg) where
  fromString msg = BootstrapSubmit (fromString msg) "btn-primary" []

-- | A Bootstrap v4 submit button disguised as a field for
-- convenience.  For example, if your form currently is:
--
-- > Person <$> areq textField "Name"  Nothing
-- >    <*> areq textField "Surname" Nothing
--
-- Then just change it to:
--
-- > Person <$> areq textField "Name"  Nothing
-- >    <*> areq textField "Surname" Nothing
-- >    <*  bootstrapSubmit ("Register" :: BootstrapSubmit Text)
--
-- (Note that '<*' is not a typo.)
--
-- Alternatively, you may also just create the submit button
-- manually as well in order to have more control over its
-- layout.
bootstrapSubmit :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m) =>
  BootstrapSubmit msg -> AForm m ()
bootstrapSubmit = formToAForm . fmap (second return) . mbootstrapSubmit

-- | Same as 'bootstrapSubmit' but for monadic forms.  This isn't
-- as useful since you're not going to use 'renderBootstrap4'
-- anyway.
mbootstrapSubmit :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m) =>
  BootstrapSubmit msg -> MForm m (FormResult (), FieldView site)
mbootstrapSubmit (BootstrapSubmit msg classes attrs) =
  let res = FormSuccess ()
      widget = [whamlet|<button class="btn #{classes}" type=submit *{attrs}>_{msg}|]
      fv  = FieldView
            { fvLabel    = ""
            , fvTooltip  = Nothing
            , fvId       = bootstrapSubmitId
            , fvInput    = widget
            , fvErrors   = Nothing
            , fvRequired = False
            }
  in return (res, fv)

-- | A royal hack.  Magic id used to identify whether a field
-- should have no label.  A valid HTML4 id which is probably not
-- going to clash with any other id should someone use
-- 'bootstrapSubmit' outside 'renderBootstrap4'.
bootstrapSubmitId :: Text
bootstrapSubmitId = "b:ootstrap___unique__:::::::::::::::::submit-id"
