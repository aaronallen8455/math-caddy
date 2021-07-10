module Frontend.Widget.Checkbox
  ( labeledCheckbox
  ) where

import           Control.Monad
import qualified Data.Text as T

import           Reflex.Dom.Core

labeledCheckbox
  :: (DomBuilder t m, MonadSample t m)
  => Dynamic t Bool
  -> T.Text
  -> T.Text
  -> m (Event t Bool)
labeledCheckbox checkedDyn labelTxt idTxt = do
  --initChecked <- sample $ current checkedDyn

  let checkboxAttrs
        = ("class" =: "labeled-checkbox")
       <> ("type" =: "checkbox")
       <> ("id" =: idTxt)

      inputConfig =
        def & inputElementConfig_initialChecked .~ False
            & inputElementConfig_setChecked .~ updated checkedDyn
            & inputElementConfig_elementConfig
              . elementConfig_initialAttributes .~ checkboxAttrs

  box <- inputElement inputConfig

  let labelAttrs = "for" =: idTxt
  void . elAttr "label" labelAttrs $ text labelTxt

  pure . updated $ _inputElement_checked box
