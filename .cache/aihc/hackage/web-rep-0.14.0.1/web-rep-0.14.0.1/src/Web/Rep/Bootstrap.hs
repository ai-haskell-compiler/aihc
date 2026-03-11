{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Some <https://getbootstrap.com/ bootstrap> assets and functionality.
module Web.Rep.Bootstrap
  ( bootstrapCss,
    bootstrapJs,
    bootstrapMeta,
    bootstrapPage,
    cardify,
    accordion,
    accordionChecked,
    accordionCard,
    accordionCardChecked,
    accordion_,
  )
where

import Control.Monad.State.Lazy
import Data.Bool
import Data.ByteString (ByteString)
import Data.Functor.Identity
import MarkupParse
import Web.Rep.Page
import Web.Rep.Shared

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Web.Rep
-- >>> import MarkupParse

-- | bootstrap css link
bootstrapCss :: Markup
bootstrapCss =
  element_
    "link"
    [ Attr "rel" "stylesheet",
      Attr "href" "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css",
      Attr "integrity" "sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC",
      Attr "crossorigin" "anonymous"
    ]

-- | bootstrap JS link
bootstrapJs :: Markup
bootstrapJs =
  element_
    "script"
    [ Attr "src" "https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js",
      Attr "integrity" "sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM",
      Attr "crossorigin" "anonymous"
    ]
    <> element_
      "script"
      [ Attr "src" "https://code.jquery.com/jquery-3.3.1.slim.min.js",
        Attr "integrity" "sha384-q8i/X+965DzO0rT7abK41JStQIAqVgRVzpbzo5smXKp4YfRvH+8abtTE1Pi6jizo",
        Attr "crossorigin" "anonymous"
      ]

-- | bootstrap meta element.
bootstrapMeta :: Markup
bootstrapMeta =
  element_ "meta" [Attr "charset" "utf-8"]
    <> element_
      "meta"
      [ Attr "name" "viewport",
        Attr "content" "width=device-width, initial-scale=1, shrink-to-fit=no"
      ]

-- | A page containing all the <https://getbootstrap.com/ bootstrap> needs for a web page.
bootstrapPage :: Page
bootstrapPage =
  Page
    bootstrapCss
    bootstrapJs
    mempty
    mempty
    mempty
    bootstrapMeta
    mempty

-- | wrap some Html with the bootstrap <https://getbootstrap.com/docs/4.3/components/card/ card> class
cardify :: (Markup, [Attr]) -> Maybe ByteString -> (Markup, [Attr]) -> Markup
cardify (h, hatts) t (b, batts) =
  element "div" ([Attr "class" "card"] <> hatts) $
    h
      <> element
        "div"
        ([Attr "class" "card-body"] <> batts)
        (maybe mempty (elementc "h5" [Attr "class" "card-title"]) t <> b)

-- | A Html object based on the bootstrap accordion card concept.
accordionCard :: Bool -> [Attr] -> ByteString -> ByteString -> ByteString -> ByteString -> Markup -> Markup
accordionCard collapse atts idp idh idb t0 b =
  element
    "div"
    ([Attr "class" "card"] <> atts)
    ( element
        "div"
        [Attr "class" "card-header p-0", Attr "id" idh]
        ( element
            "h2"
            [Attr "class" "m-0"]
            ( elementc
                "button"
                [ Attr "class" ("btn btn-link" <> bool "" " collapsed" collapse),
                  Attr "type" "button",
                  Attr "data-toggle" "collapse",
                  Attr "data-target" ("#" <> idb),
                  Attr "aria-expanded" (bool "true" "false" collapse),
                  Attr "aria-controls" idb
                ]
                t0
            )
            <> element
              "div"
              [ Attr "id" "idb",
                Attr "class" ("collapse" <> bool " show" "" collapse),
                Attr "aria-labelledby" idh,
                Attr "data-parent" ("#" <> idp)
              ]
              (element "div" [Attr "class" "card-body"] b)
        )
    )

-- | A bootstrap accordion card attached to a checkbox.
accordionCardChecked :: Bool -> ByteString -> ByteString -> ByteString -> ByteString -> Markup -> Markup -> Markup
accordionCardChecked collapse idp idh idb label bodyhtml checkhtml =
  element
    "div"
    [Attr "class" "card"]
    ( element
        "div"
        [ Attr "class" "card-header p-0",
          Attr "id" idh
        ]
        ( checkhtml
            <> element
              "h2"
              [Attr "class" "m-0"]
              ( elementc
                  "button"
                  [ Attr "class" ("btn btn-link" <> bool "" " collapsed" collapse),
                    Attr "type" "button",
                    Attr "data-toggle" "collapse",
                    Attr "data-target" ("#" <> idb),
                    Attr "aria-expanded" (bool "true" "false" collapse),
                    Attr "aria-controls" idb
                  ]
                  label
              )
        )
        <> element
          "div"
          [ Attr "id" "idb",
            Attr "class" ("collapse" <> bool " show" "" collapse),
            Attr "aria-labelledby" idh,
            Attr "data-parent" ("#" <> idp)
          ]
          (element "div" [Attr "class" "card-body"] bodyhtml)
    )

-- | create a bootstrapped accordian class
accordion ::
  (MonadState Int m) =>
  ByteString ->
  -- | name prefix.  This is needed because an Int doesn't seem to be a valid name.
  Maybe ByteString ->
  -- | card title
  [(ByteString, Markup)] ->
  -- | title, html tuple for each item in the accordion.
  m Markup
accordion pre x hs = do
  idp' <- genNamePre pre
  element "div" [Attr "class" "accordion m-1", Attr "id" idp'] . mconcat <$> aCards idp'
  where
    aCards par = mapM (aCard par) hs
    aCard par (t, b) = do
      idh <- genNamePre pre
      idb <- genNamePre pre
      pure $ accordionCard (x /= Just t) [] par idh idb t b

-- | create a bootstrapped accordian class
accordionChecked :: (MonadState Int m) => ByteString -> [(ByteString, Markup, Markup)] -> m Markup
accordionChecked pre hs = do
  idp' <- genNamePre pre
  element "div" [Attr "class" "accordion m-1", Attr "id" idp'] . mconcat <$> aCards idp'
  where
    aCards par = mapM (aCard par) hs
    aCard par (l, bodyhtml, checkhtml) = do
      idh <- genNamePre pre
      idb <- genNamePre pre
      pure $ accordionCardChecked True par idh idb l bodyhtml checkhtml

-- | This version of accordion runs a local state for naming, and will cause name clashes if the prefix is not unique.
accordion_ :: ByteString -> Maybe ByteString -> [(ByteString, Markup)] -> Markup
accordion_ pre x hs = runIdentity $ evalStateT (accordion pre x hs) 0
