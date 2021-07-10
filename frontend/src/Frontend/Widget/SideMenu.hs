{-# LANGUAGE FlexibleContexts, GADTs #-}
module Frontend.Widget.SideMenu
  ( sideMenuWidget
  ) where

import           Control.Monad
import           Control.Monad.Fix
import           Data.Bool
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Reflex.Dom.Core

import           Common.Api
import qualified Frontend.Model as Model
import qualified Frontend.Model.Entries as Entries
import qualified Frontend.Model.Filter as Filter
import           Frontend.Widget.Checkbox (labeledCheckbox)

sideMenuWidget
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t Model.CategoryMap
  -> Dynamic t Filter.M
  -> m (Event t Model.Ev)
sideMenuWidget catMapDyn filterDyn =
  divClass "side-menu-wrapper" $ do
    divClass "menu-tab" $ text "☰"
    divClass "menu-body" $ do
      newEntryEv <- (Model.EntriesEv Entries.CreateNewEntry <$)
                <$> button "New Entry"

      void . el "h3" $ text "Categories"

      catFilterEv <- fmap (Model.FilterEv . snd) . fmapMaybe M.lookupMin <$>
        listViewWithKey
          catMapDyn
          (\_ catDyn -> catCheckbox filterDyn =<< sample (current catDyn))

      void . el "h3" $ text "Types"

      typeFilterEv <-
        fmap Model.FilterEv . leftmost
          <$> traverse (typeCheckbox filterDyn) [minBound..]

      pure $ leftmost
        [ newEntryEv
        , catFilterEv
        , typeFilterEv
        ]

catCheckbox
  :: (DomBuilder t m, MonadSample t m)
  => Dynamic t Filter.M
  -> Category HasId
  -> m (Event t Filter.Ev)
catCheckbox filterDyn cat =
  divClass "category-filter" $ do
    let catId = categoryId cat
        isCheckedDyn
          = elem catId
          . Filter.filterCategory <$> filterDyn
        title = T.toTitle $ categoryName cat
        idTxt = (<> "-category-input")
              . T.toLower $ categoryName cat

    checkedEv <- labeledCheckbox isCheckedDyn title idTxt
    pure $ bool (Filter.AddCategory catId)
                (Filter.RemoveCategory catId)
      <$> checkedEv

typeCheckbox
  :: (DomBuilder t m, MonadSample t m)
  => Dynamic t Filter.M
  -> EntryType
  -> m (Event t Filter.Ev)
typeCheckbox filterDyn entType =
  divClass "type-filter" $ do
    let isCheckedDyn
          = elem entType
          . Filter.filterEntryType <$> filterDyn
        title = T.toTitle $ entryTypeToText entType
        idTxt = (<> "-type-input")
              . T.toLower $ entryTypeToText entType

    checkedEv <- labeledCheckbox isCheckedDyn title idTxt
    pure $ bool (Filter.AddEntryType entType)
                (Filter.RemoveEntryType entType)
      <$> checkedEv
