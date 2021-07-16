module Frontend.Widget.Checkbox
  ( labeledCheckbox
  ) where

import           Control.Monad
import qualified Data.Text as T

import           Reflex.Dom.Core

labeledCheckbox
  :: DomBuilder t m
  => T.Text
  -> T.Text
  -> Bool
  -> m (Event t Bool)
labeledCheckbox labelTxt idTxt checked =
  divClass "labeled-checkbox-wrapper" $ do
    let checkboxAttrs
          = ("class" =: "labeled-checkbox")
         <> ("type" =: "checkbox")
         <> ("id" =: idTxt)

        inputConfig =
          def & inputElementConfig_initialChecked .~ checked
              & inputElementConfig_elementConfig
                . elementConfig_initialAttributes .~ checkboxAttrs

    box <- inputElement inputConfig

    let labelAttrs = "for" =: idTxt
    void . elAttr "label" labelAttrs $ text labelTxt

    pure . updated $ _inputElement_checked box
