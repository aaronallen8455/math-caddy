{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import qualified Data.Text as T
import qualified Text.URI as URI

import           Obelisk.Frontend
import qualified Obelisk.Configs as Cfg
import           Obelisk.Route
import           Obelisk.Generated.Static

import           Reflex.Dom.Core

import           Common.Route
import qualified Frontend.Model as Model
import           Frontend.Routing

-- MathJax.typeset()

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
          prerender_ blank $ do
--             initModelEv <- fmap (uncurry Model.Replace) . fmapMaybe id
--                        <$> (getAndDecode . (initUri <$) =<< now)
-- 
--             modelDyn <- fmap (traceDyn "...") . foldDyn Model.applyEvent Model.empty $
--               leftmost [ initModelEv ]

            --dynText $ T.pack . show <$> modelDyn
            pure ()
  }
