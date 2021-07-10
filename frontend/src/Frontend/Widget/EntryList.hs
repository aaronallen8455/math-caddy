{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Widget.EntryList
  ( entryListWidget
  ) where

import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Reflex.Dom.Core

import           Common.Api
import qualified Frontend.Model.Entries as Entries
import           Frontend.Widget.EntryEditor

entryListWidget
  :: (DomBuilder t m, PostBuild t m, MonadIO (Performable m), PerformEvent t m, MonadHold t m, MonadFix m)
  => Dynamic t Entries.M -> m (Event t Entries.Ev)
entryListWidget modelDyn = do
  divClass "entries" $ do
    let entriesDyn = Entries.entries <$> modelDyn

    entryEvents <- fmap snd . fmapMaybe M.lookupMin
               <$> listViewWithKey entriesDyn
                                   (const entryWidget)

    editorEvents <- switchDyn
      <$> widgetHold (pure never)
                     (editDialogWidget <$> updated modelDyn)

    -- TODO deactivate button if waiting for more
    loadMoreClick <- button "Load More"
    let loadMoreEv = fmap (Entries.LoadMore . fst)
                   . fmapMaybe M.lookupMax
                   $ current entriesDyn <@ loadMoreClick

    pure $ leftmost [editorEvents, entryEvents, loadMoreEv]

entryWidget :: (DomBuilder t m, PostBuild t m)
            => Dynamic t (Entry HasId HasId) -> m (Event t Entries.Ev)
entryWidget entryDyn = do
  elDynClass "div" (entryTypeClass . entryType <$> entryDyn) $ do
    (domEvent Click -> editClick, _) <-
      elClass' "div" "entry-edit" $ text "✎"

    let editClick' =
          (Entries.EditEntry . entryId <$> current entryDyn)
            <@ editClick

    -- TODO add confirmation dialog
    (domEvent Click -> deleteClick, _) <-
      elClass' "div" "entry-delete" $ text "✖"

    let deleteClick' =
          (Entries.DeleteEntry . entryId <$> current entryDyn)
            <@ deleteClick

    el "h4" $ dynText (entryName <$> entryDyn)

    elClass "p" "entry-body" $ dynText (entryBody <$> entryDyn)

    entryField "Type" (entryTypeToText . entryType <$> entryDyn)
    entryField "Categories" (categoryListTxt <$> entryDyn)
    entryField "References" (entryReferences <$> entryDyn)

    pure $ leftmost
      [ editClick'
      , deleteClick'
      ]

entryTypeClass :: EntryType -> T.Text
entryTypeClass = (<> "-entry entry") . entryTypeToText

entryField :: (DomBuilder t m, PostBuild t m)
           => T.Text -> Dynamic t T.Text -> m ()
entryField name valDyn = do
  divClass "entry-field" $ do
    elClass "span" "field-name" $ text name
    elClass "span" "field-value" . dynText
      $ T.toTitle <$> valDyn

categoryListTxt :: Entry a b -> T.Text
categoryListTxt = T.intercalate ", " . map categoryName . entryCategories
