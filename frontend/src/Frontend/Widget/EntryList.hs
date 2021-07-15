{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Widget.EntryList
  ( entryListWidget
  ) where

import           Control.Monad.Fix
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Language.Javascript.JSaddle.Types (MonadJSM)

import           Reflex.Dom.Core

import           Common.Api
import qualified Frontend.Model as Model
import qualified Frontend.Model.Entries as Entries
import           Frontend.Widget.EntryEditor

entryListWidget
  :: (DomBuilder t m, PerformEvent t m, TriggerEvent t m, MonadJSM (Performable m), PostBuild t m, MonadHold t m, MonadFix m)
  => Dynamic t Entries.M -> Dynamic t Model.CategoryMap -> m (Event t Entries.Ev)
entryListWidget modelDyn catMapDyn = do
  divClass "entries" $ do
    let entriesDyn = Entries.entries <$> modelDyn

    entryEvents <- fmap snd . fmapMaybe M.lookupMin
               <$> listViewWithKey entriesDyn
                                   (const entryWidget)

    editorEvents <- switchDyn
      <$> widgetHold (pure never)
                     (editDialogWidget <$> current catMapDyn <@> updated modelDyn)

    -- TODO deactivate button if waiting for more
    loadMoreClick <- button "Load More"

    let loadMoreEv = fmap (Entries.LoadMore . fst)
                   . fmapMaybe M.lookupMax
                   $ current entriesDyn <@ loadMoreClick

    pure $ leftmost [editorEvents, entryEvents, loadMoreEv]

entryWidget :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m)
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
          (Entries.ReqDeleteEntry . entryId <$> current entryDyn)
            <@ deleteClick

    el "h4" $ dynText (entryName <$> entryDyn)

    -- let maths = MathJax.startup.document.getMathItemsWithin(document.body)
    -- maths.forEach(x => x.removeFromDocument);
    bodyDyn <- holdUniqDyn $ entryBody <$> entryDyn
    -- need to entangle the entire element with the dyn so that mathjax doesn't
    -- create duplicate typesetting.
    widgetHold_
      (elClass "p" "entry-body" $ dynText bodyDyn)
      (elClass "p" "entry-body" . text <$> updated bodyDyn)

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
