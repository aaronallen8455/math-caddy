{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module Frontend.Async
  ( Ev(..)
  , asyncEvents
  ) where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Language.Javascript.JSaddle.Types (MonadJSM)
import           Text.URI (URI)
import qualified Text.URI as URI

import           Reflex.Dom.Core

import           Common.Api
import           Common.Route
import           Frontend.Async.Ev
import qualified Frontend.Model as Model
import qualified Frontend.Model.Entries as Entries
import qualified Frontend.Model.Filter as Filter
import           Frontend.Routing

asyncEvents
  :: (TriggerEvent t m, PerformEvent t m, HasJSContext (Performable m), MonadJSM (Performable m))
  => URI
  -> CheckedFullEncoder
  -> Event t [Ev]
  -> m (Event t [Model.Ev])
asyncEvents baseUri enc
  = performRequests
  . fmapMaybe (foldMap (fmap pure . reqSpecForEvent baseUri enc))

reqSpecForEvent
  :: URI
  -> CheckedFullEncoder
  -> Ev
  -> Maybe (SomeXhrRequest, ResponseHandler)
reqSpecForEvent baseUri enc = \case
  -- this cannot change the filter, otherwise a feedback loop will occur
  RequestEntries f -> do
    uri <- getFilteredEntriesUri baseUri enc f

    let xhrReq = xhrRequest "GET" (URI.render uri) def
        handler resp =
          case decodeXhrResponse resp of
            Nothing -> [Model.FilterReqFailed]
            Just entries
              | isJust (Filter.filterLowerBound f) ->
                  -- Having a lower bound means we are requesting more entries
                  [Model.EntriesEv (Entries.AddOrReplaceEntries entries)]
              | otherwise ->
                  [Model.EntriesEv (Entries.ReplaceAllEntries entries)]

    pure (MkSomeXhrReq xhrReq, handler)

  DeleteEntry eId -> do
    uri <- getDeleteEntryUri baseUri enc eId

    let xhrReq = xhrRequest "GET" (URI.render uri) def
        handler resp =
          case decodeXhrResponse resp of
            Nothing -> [Model.EntriesEv Entries.ServerError]
            Just cats ->
              [ Model.EntriesEv $ Entries.DeleteEntry eId
              , Model.RemoveCategories cats
              ]

    pure (MkSomeXhrReq xhrReq, handler)

  SaveNewEntry entry -> do
    uri <- getAddEntryUri baseUri enc

    let xhrReq = postJson (URI.render uri) entry
        handler resp =
          case decodeXhrResponse resp of
            Nothing -> [Model.EntriesEv Entries.ServerError]
            Just newEntry ->
              [Model.AddOrReplaceCategories
                $ entryCategories (newEntry :: Entry HasId HasId)
              ]

    pure (MkSomeXhrReq xhrReq, handler)

  UpdateEntry entry -> do
    uri <- getUpdateEntryUri baseUri enc

    let xhrReq = postJson (URI.render uri) entry
        handler resp =
          case decodeXhrResponse resp of
            Nothing -> [Model.EntriesEv Entries.ServerError]
            Just newEntry ->
              [ Model.AddOrReplaceCategories $ entryCategories newEntry
              , Model.EntriesEv $ Entries.AddOrReplaceEntries [newEntry]
              ]

    pure (MkSomeXhrReq xhrReq, handler)

  Init -> do
    uri <- getInitUri baseUri enc

    let xhrReq = xhrRequest "GET" (URI.render uri) def
        handler resp =
          case decodeXhrResponse resp of
            Nothing -> error "could not initialize!"
            Just (entries, cats) ->
              [ Model.Replace Entries.M
                  { Entries.entries = M.fromList $ (entryId &&& id) <$> entries
                  , Entries.editorStatus = Entries.NotEditing
                  , Entries.waitingForMore = False
                  }
                  cats
              ]

    pure (MkSomeXhrReq xhrReq, handler)

type ResponseHandler = XhrResponse -> [Model.Ev]

performRequests
  :: (TriggerEvent t m, PerformEvent t m, HasJSContext (Performable m), MonadJSM (Performable m))
  => Event t [(SomeXhrRequest, ResponseHandler)]
  -> m (Event t [Model.Ev])
performRequests evs = performEventAsync $ ffor evs $
  \pairs cb ->
    for_ pairs $
      \(MkSomeXhrReq r, handler) ->
        void . newXMLHttpRequest r $ liftIO . cb . handler

data SomeXhrRequest where
  MkSomeXhrReq :: IsXhrPayload a => XhrRequest a -> SomeXhrRequest
