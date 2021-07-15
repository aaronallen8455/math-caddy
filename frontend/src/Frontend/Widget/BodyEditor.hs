{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Widget.BodyEditor
  ( bodyEditor
  ) where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Bool
import qualified Data.Text as T
import           Language.Javascript.JSaddle.Types (MonadJSM)

import           Reflex.Dom.Core

import           Frontend.MathJax (typesetMathJax)
import           Frontend.Widget.Checkbox

bodyEditor
  :: (PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m), MonadHold t m, DomBuilder t m, MonadFix m)
  => T.Text -> m (Dynamic t T.Text)
bodyEditor bodyTxt = mdo
  text "Body"

  showPreviewEv <-
    labeledCheckbox "Preview typesetting" "preview-typesetting-toggle" False

  currentTextDyn <- holdDyn bodyTxt (updated bodyTextDyn)

  delayedShowPreviewEv <- delay 0.1 showPreviewEv
  performEvent_ $ typesetMathJax <$ delayedShowPreviewEv

  let entry = do
        currentText <- sample $ current currentTextDyn
        taEl <- textAreaElement def
          { _textAreaElementConfig_initialValue = currentText }
        let blurEv = domEvent Blur taEl
        holdDyn currentText $ current (_textAreaElement_value taEl) <@ blurEv

      preview = do
        txt <- sample $ current currentTextDyn
        elClass "p" "preview-typesetting-p" $ text txt
        pure $ pure txt

  bodyTextDyn <- join <$>
    widgetHold entry (bool entry preview <$> showPreviewEv)
  pure currentTextDyn
