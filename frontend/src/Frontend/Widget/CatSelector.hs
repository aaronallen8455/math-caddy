{-# LANGUAGE RecursiveDo #-}
module Frontend.Widget.CatSelector
  ( catSelector
  ) where

import           Control.Monad.Fix
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Reflex.Dom.Core

import           Common.Api
import qualified Frontend.Model as Model
import           Frontend.Widget.TextEntry

catSelector :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
            => Model.CategoryMap -> [Category MbId] -> m (Dynamic t [Category MbId])
catSelector catMap initCats =
  divClass "cat-selector" $ mdo
    let idLookup = M.fromList $ do
          cat <- M.elems catMap
          pure (categoryName cat, categoryId cat)
    catDyn <-
      foldDynMaybe
        (applyEv idLookup)
        initCats
        (leftmost [newEv, deleteEv])

    newEv <- fmap AddCat <$> textEntryOnEnter "Category Name" Nothing
    deleteEv <- selectedCats catDyn

    pure catDyn

selectedCats :: (PostBuild t m, DomBuilder t m, MonadHold t m)
             => Dynamic t [Category MbId] -> m (Event t CatEv)
selectedCats catsDyn =
  divClass "selected-cats" $ do
    eventOfEvents <- dyn (traverse catTag <$> catsDyn)
    switchHold never (leftmost <$> eventOfEvents)

catTag :: DomBuilder t m => Category MbId -> m (Event t CatEv)
catTag cat =
  divClass "cat-tag" $ do
    (ele, _) <- elClass' "span" "cat-remove" $ text "✖"
    let deleteEv = domEvent Click ele
    text (categoryName cat)
    pure $ DeleteCat (categoryName cat) <$ deleteEv

data CatEv
  = AddCat T.Text
  | DeleteCat T.Text

applyEv :: M.Map T.Text CategoryId -> CatEv -> [Category MbId] -> Maybe [Category MbId]
applyEv idLookup ev cats =
  case ev of
    AddCat newCat ->
      if newCat `elem` (categoryName <$> cats)
         then Nothing
         else Just $ Category
                       { categoryId   = idLookup M.!? newCat
                       , categoryName = newCat
                       } : cats
    DeleteCat delCat ->
      Just $ filter ((/= delCat) . categoryName) cats

