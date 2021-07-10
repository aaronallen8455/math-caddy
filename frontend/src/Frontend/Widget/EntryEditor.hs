{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Widget.EntryEditor
  ( editDialogWidget
  ) where

import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Data.Map.Strict as M
import           Data.Time

import           Reflex.Dom.Core

import           Common.Api
import qualified Frontend.Model.Entries as Entries
import           Frontend.Widget.CatSelector
import           Frontend.Widget.DropDown
import           Frontend.Widget.TextEntry

editDialogWidget :: (DomBuilder t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, MonadHold t m, MonadFix m)
                 => Entries.M -> m (Event t Entries.Ev)
editDialogWidget model =
  case Entries.editorStatus model of
    Entries.NotEditing       -> pure never
    Entries.CreateNew        -> editorWidget Nothing
    Entries.Editing eId      -> editorWidget $ Entries.entries model M.!? eId
    Entries.WaitingForServer -> pure never

editorWidget :: (DomBuilder t m, MonadIO (Performable m), PerformEvent t m, PostBuild t m, MonadHold t m, MonadFix m)
             => Maybe (Entry HasId HasId) -> m (Event t Entries.Ev)
editorWidget mbEntry = do
  divClass "entry-editor-backdrop" blank
  divClass "entry-editor" $ mdo
    nameDyn <- basicTextEntry "Name" (entryName <$> mbEntry)
    -- TODO need special widget for this
    bodyDyn <- basicTextEntry "Body" (entryBody <$> mbEntry)
    refsDyn <- basicTextEntry "References" (entryBody <$> mbEntry)

    catsDyn <- catSelector (foldMap (map categoryMbId . entryCategories) mbEntry)

    typeDyn <- dropDown Theorem [minBound ..]

    timeEv <- performEvent $ liftIO getCurrentTime <$ saveEv
    -- TODO does this work?
    timeDyn <- holdDyn (maybe blankTime entryAdded mbEntry) timeEv

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
