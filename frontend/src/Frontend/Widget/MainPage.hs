{-# LANGUAGE FlexibleContexts #-}
module Frontend.Widget.MainPage
  ( mainPage
  ) where

import           Control.Monad.Fix
import           Data.Text as T
import           Language.Javascript.JSaddle.Types (MonadJSM)

import           Reflex.Dom.Core

import qualified Frontend.Model as Model
import qualified Frontend.Model.Filter as Filter
import           Frontend.Widget.EntryList
import           Frontend.Widget.SideMenu
import           Frontend.Widget.TextEntry

mainPage
  :: (DomBuilder t m, PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m), PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t Model.M -> m (Event t Model.Ev)
mainPage modelDyn = do
  filterEv
     <- fmap (\t -> Model.FilterEv . Filter.SetNeedle
                  $ if T.null t then Nothing else Just t
             )
        -- ignore input that is too short
      . ffilter ((\l -> l >= 3 || l == 0) . T.length . T.strip)
    <$> filterTextEntry "Filter" Nothing

  entriesEv <- fmap Model.EntriesEv
           <$> entryListWidget (Model.modelEntries <$> modelDyn)
                               (Model.modelCategories <$> modelDyn)

  sideMenuEv <-
    sideMenuWidget
      (Model.modelCategories <$> modelDyn)
      (Model.modelFilter <$> modelDyn)

  pure $ leftmost [filterEv, entriesEv, sideMenuEv]

