{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
module Frontend.Widget.TextEntry
  ( basicTextEntry
  , textEntryOnEnter
  , filterTextEntry
  ) where

import           Control.Monad.Fix
import           Data.Maybe
import qualified Data.Text as T

import           Reflex.Dom.Core

basicTextEntry
  :: DomBuilder t m
  => T.Text
  -> Maybe T.Text
  -> m (Dynamic t T.Text)
basicTextEntry placeHolder mInitial = do
  let attrs = ("placeholder" =: placeHolder)
           <> ("class" =: "basic-text-input")

      inpElConfig =
        def & inputElementConfig_elementConfig . elementConfig_initialAttributes
                .~ attrs
            & inputElementConfig_initialValue .~ fromMaybe "" mInitial

  inp <- inputElement inpElConfig

  pure $ _inputElement_value inp

textEntryOnEnter
  :: (DomBuilder t m, MonadFix m)
  => T.Text
  -> Maybe T.Text
  -> m (Event t T.Text)
textEntryOnEnter placeHolder mInitial = mdo
  let attrs = ("placeholder" =: placeHolder)
           <> ("class" =: "text-input")

      inpElConfig =
        def & inputElementConfig_setValue .~ clearInpEv
            & inputElementConfig_elementConfig . elementConfig_initialAttributes
                .~ attrs
            & inputElementConfig_initialValue .~ fromMaybe "" mInitial

  inp <- inputElement inpElConfig

  let enterKeyEv = inputEnterKeyEv inp
      clearInpEv = "" <$ enterKeyEv

      resultEv = current (_inputElement_value inp) <@ enterKeyEv

  pure resultEv

filterTextEntry
  :: (DomBuilder t m, MonadFix m, MonadHold t m)
  => T.Text
  -> Maybe T.Text
  -> m (Event t T.Text)
filterTextEntry placeHolder mInitial = mdo
  let attrs = ("placeholder" =: placeHolder)
           <> ("class" =: "text-input")

      inpElConfig =
        def & inputElementConfig_elementConfig . elementConfig_initialAttributes
                .~ attrs
            & inputElementConfig_initialValue .~ fromMaybe "" mInitial

  inp <- inputElement inpElConfig

  let enterKeyEv = inputEnterKeyEv inp
      resultEv = current (_inputElement_value inp) <@ enterKeyEv

  uniqueValDyn <- holdUniqDyn =<< holdDyn (fromMaybe "" mInitial) resultEv

  pure $ updated uniqueValDyn

inputEnterKeyEv :: (Reflex t, HasDomEvent t (Element er d t) 'KeydownTag, DomEventType (Element er d t) 'KeydownTag ~ Word)
                => InputElement er d t -> Event t ()
inputEnterKeyEv inp =
  let inpEl = _inputElement_element inp
      keydownEvent = keyCodeLookup . fromIntegral @Word
                 <$> domEvent Keydown inpEl
   in () <$ ffilter (== Enter) keydownEvent
