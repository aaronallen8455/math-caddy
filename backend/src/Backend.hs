{-# LANGUAGE GADTs #-}
module Backend where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Database.SQLite.Simple as DB
import           Obelisk.Backend
import           Obelisk.Route
import qualified Snap.Core as S

import           Common.Route
import           DB.Schema
import           DB.Queries

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    createTablesIfNotExist

    serve $ \case
      BackendRouteMissing :/ () -> pure ()

      BackendRouteGetFiltered :/ (mLowerBound, (pageSize, (mNeedle, (cats, types)))) -> do
        entries <- liftIO . DB.withConnection dbName $ \conn ->
          fetchEntries conn mLowerBound pageSize mNeedle cats types

        S.modifyResponse $ S.setContentType "application/json"
        S.writeLBS $ Aeson.encode entries

      BackendRouteAddEntry :/ () -> do
        reqBody <- S.readRequestBody 2048
        let eNewEntry = Aeson.eitherDecode' reqBody
        case eNewEntry of
          Left err ->
            S.modifyResponse $ S.setResponseStatus 400 (BS8.pack err)
          Right newEntry -> do
            result <- liftIO . DB.withConnection dbName $ \conn ->
              insertEntry conn newEntry
            S.modifyResponse $ S.setContentType "application/json"
            S.writeLBS $ Aeson.encode result

      BackendRouteDeleteEntry :/ entryId -> do
        deletedCats <- liftIO . DB.withConnection dbName $ \conn ->
          deleteEntry conn entryId

        S.modifyResponse $ S.setContentType "application/json"
        S.writeLBS $ Aeson.encode deletedCats

      BackendRouteUpdateEntry :/ () -> do
        reqBody <- S.readRequestBody 2048
        let eUpdatedEntry = Aeson.eitherDecode' reqBody
        case eUpdatedEntry of
          Left err ->
            S.modifyResponse $ S.setResponseStatus 400 (BS8.pack err)
          Right updatedEntry -> do
            result <- liftIO . DB.withConnection dbName $ \conn ->
              updateEntry conn updatedEntry
            S.modifyResponse $ S.setContentType "application/json"
            S.writeLBS $ Aeson.encode result

      BackendRouteGetCategories :/ () -> do
        cats <- liftIO . DB.withConnection dbName $ \conn ->
          getCategories conn

        S.modifyResponse $ S.setContentType "application/json"
        S.writeLBS $ Aeson.encode cats

      BackendRouteInit :/ pageSize -> do
        result <- liftIO . DB.withConnection dbName $ \conn -> do
          es <- fetchEntries conn Nothing pageSize Nothing [] []
          cs <- getCategories conn
          pure (es, cs)

        S.modifyResponse $ S.setContentType "application/json"
        liftIO $ print result
        S.writeLBS $ Aeson.encode result

  , _backend_routeEncoder = fullRouteEncoder
  }

