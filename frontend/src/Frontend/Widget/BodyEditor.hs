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
  :: (PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m), MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m)
  => T.Text -> m (Dynamic t T.Text)
bodyEditor bodyTxt =
  divClass "body-editor" $ mdo
    showPreviewEv <-
      labeledCheckbox "Preview typesetting" "preview-typesetting-toggle" False

    currentTextDyn <- holdDyn bodyTxt (updated bodyTextDyn)

    let entry = do
          currentText <- sample $ current currentTextDyn
          _textAreaElement_value <$> textAreaElement def
            { _textAreaElementConfig_initialValue = currentText }

        preview = do
          postBuildEv <- delay 0.03 =<< getPostBuild
          performEvent_ $ typesetMathJax <$ postBuildEv
          txt <- sample $ current currentTextDyn
          elClass "p" "preview-typesetting" $ text txt
          pure $ pure txt

    bodyTextDyn <- join <$>
      widgetHold entry (bool entry preview <$> showPreviewEv)
    pure currentTextDyn
