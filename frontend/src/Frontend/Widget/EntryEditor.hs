{-# LANGUAGE FlexibleContexts #-}
module Frontend.Widget.EntryEditor
  ( editDialogWidget
  ) where

import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Data.Time
import           Language.Javascript.JSaddle.Types (MonadJSM)

import           Reflex.Dom.Core

import           Common.Api
import qualified Frontend.Model as Model
import qualified Frontend.Model.Entries as Entries
import           Frontend.Widget.BodyEditor (bodyEditor)
import           Frontend.Widget.CatSelector
import           Frontend.Widget.DropDown
import           Frontend.Widget.TextEntry

editDialogWidget
  :: (DomBuilder t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m, PerformEvent t m, MonadHold t m, MonadFix m)
  => Model.CategoryMap -> Entries.M -> m (Event t Entries.Ev)
editDialogWidget catMap model =
  case Entries.editorStatus model of
    Entries.NotEditing       -> pure never
    Entries.CreateNew        -> editorWidget catMap Nothing
    Entries.Editing eId      -> editorWidget catMap $ Entries.entries model M.!? eId
    Entries.WaitingForServer -> pure never

editorWidget
  :: (DomBuilder t m, PostBuild t m, MonadJSM (Performable m), TriggerEvent t m, PerformEvent t m, MonadHold t m, MonadFix m)
  => Model.CategoryMap -> Maybe (Entry HasId HasId) -> m (Event t Entries.Ev)
editorWidget catMap mbEntry = do
  divClass "entry-editor-backdrop" blank
  divClass "entry-editor" $ do
    nameDyn <- basicTextEntry "Name" (entryName <$> mbEntry)
    -- TODO need special widget for this
    bodyDyn <- bodyEditor (foldMap entryBody mbEntry)
    refsDyn <- basicTextEntry "References" (entryReferences <$> mbEntry)

    catsDyn <- catSelector catMap (foldMap (map categoryMbId . entryCategories) mbEntry)

    typeDyn <- dropDown (maybe Theorem entryType mbEntry) [minBound ..]

    timeEv <- performEvent $ liftIO getCurrentTime
                <$ leftmost
                   [ () <$ updated nameDyn
                   , () <$ updated bodyDyn
                   , () <$ updated refsDyn
                   , () <$ updated catsDyn
                   , () <$ updated typeDyn
                   ]

    timeDyn <- case entryAdded <$> mbEntry of
                 Just added -> pure $ pure added
                 Nothing -> holdDyn (maybe blankTime entryAdded mbEntry) timeEv

    let entryDyn =
          Entry (entryId <$> mbEntry)
            <$> nameDyn
            <*> bodyDyn
            <*> refsDyn
            <*> catsDyn
            <*> typeDyn
            <*> timeDyn

    saveClickEv <- button "Save"
    let saveEv = (mkSaveEv <$> current entryDyn) <@ saveClickEv

    cancelEv <- (Entries.CancelEditor <$) <$> button "Cancel"

    pure $ leftmost
      [ saveEv
      , cancelEv
      ]


mkSaveEv :: Entry MbId MbId -> Entries.Ev
mkSaveEv entry =
  case entryId entry of
    Nothing -> Entries.ReqSaveNewEntry entry { entryId = () }
    Just eId -> Entries.ReqUpdateEntry entry { entryId = eId }

blankTime :: UTCTime
blankTime = UTCTime (fromGregorian 0 0 0) 0
