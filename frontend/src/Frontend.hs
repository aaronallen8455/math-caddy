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
import qualified Frontend.Async.Ev as Async
import           Frontend.Async (asyncEvents)
import           Frontend.MathJax (typesetMathJax)
import qualified Frontend.Model as Model
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
            pure (baseUri, checkedEnc)

      case mInitVals of
        Nothing -> error "Initialization failed"
        Just (baseUri, checkedEncoder) ->
          prerender_ blank $ mdo
            initModelEv <- ([Async.Init] <$) <$> getPostBuild

            uiEv <- mainPage modelDyn

            (modelDyn, asyncDyn) <- fmap splitDynPure
              . foldDyn Model.applyEvents (Model.initModel, [])
              $ fmap pure uiEv <> asyncResultEv

            let asyncEv = ffilter (not . null) $ updated asyncDyn
            asyncResultEv <- fmap (ffilter $ not . null)
                           . asyncEvents baseUri checkedEncoder
                           $ asyncEv <> initModelEv

            delayedAsyncEv <- delay 0.1 asyncResultEv
            -- TODO be more selective about which events trigger typeset
            performEvent_ (typesetMathJax <$ delayedAsyncEv)
            -- need to typeset whenever the list of entries changes

            pure ()
  }
