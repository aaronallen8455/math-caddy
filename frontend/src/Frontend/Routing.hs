{-# LANGUAGE RecordWildCards #-}
module Frontend.Routing
  ( getCategoriesUri
  , getFilteredEntriesUri
  , getInitUri
  , getAddEntryUri
  , getUpdateEntryUri
  , getDeleteEntryUri
  ) where

import           Data.Bitraversable
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import           Text.URI (URI)
import qualified Text.URI as URI

import           Obelisk.Route

import           Common.Api
import           Common.Route
import qualified Frontend.Model.Filter as Filter

pageSize :: PageSize
pageSize = 20

getCategoriesUri :: URI -> CheckedFullEncoder -> Maybe URI
getCategoriesUri baseURI enc = do
  let path = fst . encode enc
           $ FullRoute_Backend BackendRouteGetCategories :/ ()

  pathPiece <- NE.nonEmpty =<< traverse URI.mkPathPiece path
  pure baseURI { URI.uriPath = Just (False, pathPiece) }

getFilteredEntriesUri
  :: URI -> CheckedFullEncoder
  -> Filter.M
  -> Maybe URI
getFilteredEntriesUri baseURI enc Filter.M{..} = do
  let (path, queryMap)
        = encode enc
        $ FullRoute_Backend BackendRouteGetFiltered
            :/ ( filterLowerBound
               , (pageSize
               , (filterNeedle
               , (filterCategory
               , filterEntryType
               ))))

  query <- traverse (bitraverse URI.mkQueryKey (traverse URI.mkQueryValue))
                    (M.toList queryMap)

  pathPiece <- NE.nonEmpty =<< traverse URI.mkPathPiece path

  let mkQuery (key, Just val) = URI.QueryParam key val
      mkQuery (key, Nothing) = URI.QueryFlag key

  pure baseURI { URI.uriPath = Just (False, pathPiece)
               , URI.uriQuery = mkQuery <$> query
               }

getInitUri :: URI -> CheckedFullEncoder -> Maybe URI
getInitUri baseURI enc = do
  let path = fst . encode enc
           $ FullRoute_Backend BackendRouteInit :/ pageSize

  pathPiece <- NE.nonEmpty =<< traverse URI.mkPathPiece path

  pure baseURI { URI.uriPath = Just (False, pathPiece) }

getAddEntryUri
  :: URI -> CheckedFullEncoder
  -> Maybe URI
getAddEntryUri baseURI enc = do
  let path = fst . encode enc
           $ FullRoute_Backend BackendRouteAddEntry :/ ()

  pathPiece <- NE.nonEmpty =<< traverse URI.mkPathPiece path

  pure baseURI { URI.uriPath = Just (False, pathPiece) }

getUpdateEntryUri
  :: URI -> CheckedFullEncoder
  -> Maybe URI
getUpdateEntryUri baseURI enc = do
  let path = fst . encode enc
           $ FullRoute_Backend BackendRouteUpdateEntry :/ ()

  pathPiece <- NE.nonEmpty =<< traverse URI.mkPathPiece path

  pure baseURI { URI.uriPath = Just (False, pathPiece) }

getDeleteEntryUri
  :: URI -> CheckedFullEncoder
  -> EntryId
  -> Maybe URI
getDeleteEntryUri baseURI enc entityId = do
  let path = fst . encode enc
           $ FullRoute_Backend BackendRouteDeleteEntry :/ entityId

  pathPiece <- NE.nonEmpty =<< traverse URI.mkPathPiece path

  pure baseURI { URI.uriPath = Just (False, pathPiece) }
