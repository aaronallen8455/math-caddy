{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend where

import qualified Text.URI as URI

import           Obelisk.Frontend
import qualified Obelisk.Configs as Cfg
import           Obelisk.Route
import           Obelisk.Generated.Static

import           Reflex.Dom.Core

import           Common.Route
import           Frontend.Async (asyncEvents)
import qualified Frontend.Model as Model
import           Frontend.Routing
import           Frontend.Widget.MainPage (mainPage)

-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Math Caddy"
      elAttr "link" ("href" =: static @"main.css"
                  <> "type" =: "text/css"
                  <> "rel" =: "stylesheet") blank
      elAttr "script" ("src" =: "https://polyfill.io/v3/polyfill.min.js?features=es6") blank
      elAttr "script" ("id" =: "MathJax-script"
                    <> "async" =: ""
                    <> "src" =: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
                      ) blank
  , _frontend_body = do

      mBaseURI <- (URI.mkURI =<<) <$> Cfg.getTextConfig "common/route"

      let mInitVals = do
            baseUri <- mBaseURI
            checkedEnc <- either (const Nothing) Just
                        $ checkEncoder fullRouteEncoder
            initUri <- getInitUri baseUri checkedEnc
            pure (baseUri, checkedEnc, URI.render initUri)

      case mInitVals of
        Nothing -> error "Initialization failed"
        Just (baseUri, checkedEncoder, initUri) ->
          prerender_ blank $ mdo
            initModelEv <- fmap (uncurry Model.mkReplaceEv) . fmapMaybe id
                       <$> (getAndDecode . (initUri <$) =<< now)

            uiEv <- mainPage modelDyn

            modelDyn <- foldDyn Model.applyEvents Model.initModel $
              fmap pure initModelEv <> fmap pure uiEv <> asyncEv

            -- TODO build filter dyn separately to avoid this use of holdUniqDyn?
            -- Without holdUniq this would fire on every update to modelDyn, right?
            filterChangeEv <- do
              filterDyn <- holdUniqDyn $ Model.modelFilter <$> modelDyn
              pure $ Model.ReqFilterChange <$> updated filterDyn

            asyncEv <- asyncEvents baseUri checkedEncoder
                     $ leftmost [uiEv, filterChangeEv]

            -- need to typeset whenever the list of entries changes

            pure ()
  }

      --prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)
-- MathJax.typeset()
-- How to be sure that this fires after the entries portion of the UI has been
-- updated? Will it be good enough to hook into knowing when the entries map
-- changes? I doubt that corresponds to the UI having been updated.
-- What if the call was placed in the do block after the entries widget is
-- produced, does that mean it will occur every time that widget is redrawn?
-- Perhaps best way to proceed is to put console logs in various places to see
-- where and when certain things happen.
