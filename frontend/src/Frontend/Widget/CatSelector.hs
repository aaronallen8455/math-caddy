{-# LANGUAGE RecursiveDo #-}
module Frontend.Widget.CatSelector
  ( catSelector
  ) where

import           Control.Monad.Fix
import qualified Data.Text as T

import           Reflex.Dom.Core

import           Common.Api
import           Frontend.Widget.TextEntry

-- TODO
-- Need to pipe in the current categories so that IDs get assigned to cats
-- that have them.

catSelector :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
            => [Category MbId] -> m (Dynamic t [Category MbId])
catSelector initCats = mdo
  catDyn <-
    foldDynMaybe
      applyEv
      initCats
      (leftmost [newEv, deleteEv])

  newEv <- fmap AddCat <$> textEntryOnEnter "Category Name" Nothing
  deleteEv <- selectedCats catDyn

  pure catDyn

selectedCats :: (PostBuild t m, DomBuilder t m, MonadHold t m)
             => Dynamic t [Category MbId] -> m (Event t CatEv)
selectedCats catsDyn = do
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

applyEv :: CatEv -> [Category MbId] -> Maybe [Category MbId]
applyEv ev cats =
  case ev of
    AddCat newCat ->
      if newCat `elem` (categoryName <$> cats)
         then Nothing
         else Just $ Category
                       { categoryId   = Nothing
                       , categoryName = newCat
                       } : cats
    DeleteCat delCat ->
      Just $ filter ((/= delCat) . categoryName) cats

