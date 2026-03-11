-- | A haskell library for representing:
--
-- - web pages, as (composable) collections of Html, Css and JS text.
--
-- - things that have a tied together represention in Haskell and in the DOM, where things can have shared sub-components.
--
-- - websocket & server communication protocols.
--
-- See app/examples.hs and 'Web.Examples' for usage.
module Web.Rep
  ( -- * Shared Representation
    RepF (..),
    Rep,
    oneRep,
    SharedRepF (..),
    SharedRep,
    runOnce,
    zeroState,
    register,
    genName,
    genNamePre,

    -- * Web Rep Components
    Page (..),
    PageConfig (..),
    defaultPageConfig,
    Concerns (..),
    suffixes,
    concernNames,
    PageConcerns (..),
    PageStructure (..),
    RenderStyle (..),

    -- * Css
    Css (..),
    renderCss,
    cssColorScheme,

    -- * JS
    Js (..),
    onLoad,

    -- * re-export modules
    module Web.Rep.SharedReps,
    module Web.Rep.Render,
    module Web.Rep.Server,
    module Web.Rep.Socket,
    module Web.Rep.Html,
    module Web.Rep.Html.Input,
    module Web.Rep.Bootstrap,

    -- * re-exports
    HashMap.HashMap,
  )
where

import Data.HashMap.Strict qualified as HashMap
import MarkupParse (RenderStyle (..))
import Web.Rep.Bootstrap
import Web.Rep.Html
import Web.Rep.Html.Input
import Web.Rep.Page
import Web.Rep.Render
import Web.Rep.Server
import Web.Rep.Shared
import Web.Rep.SharedReps
import Web.Rep.Socket
