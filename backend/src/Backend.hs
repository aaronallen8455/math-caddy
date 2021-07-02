{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Backend where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as DB
import           Obelisk.Backend
import           Obelisk.Route
import qualified Snap.Core as S

import           Common.Api
import           Common.Route
import           DB.Schema

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
    createTablesIfNotExist

    serve $ \case
      BackendRouteMissing :/ () -> pure ()

      BackendRouteGetFiltered :/ (cats, types) -> do
        liftIO $ print (cats, types)
        entries <- liftIO . DB.withConnection dbName $ \conn ->
          fetchEntries conn cats types

        let entriesBs = Aeson.encode entries
        S.modifyResponse $ S.setContentType "application/json"
        S.writeLBS entriesBs

      BackendRouteAddEntry :/ () -> do
        reqBody <- S.readRequestBody 2048
        let eNewEntry = Aeson.eitherDecode' reqBody
        case eNewEntry of
          Left err ->
            S.modifyResponse $ S.setResponseStatus 400 (BS8.pack err)
          Right newEntry -> do
            result <- liftIO . DB.withConnection dbName $ \conn ->
              insertEntry conn newEntry
            let entryBs = Aeson.encode result
            S.modifyResponse $ S.setContentType "application/json"
            S.writeLBS entryBs

      BackendRouteDeleteEntry :/ entryId ->
        liftIO . DB.withConnection dbName $ \conn ->
          deleteEntry conn entryId

      BackendRouteUpdateEntry :/ () -> do
        reqBody <- S.readRequestBody 2048
        let eUpdatedEntry = Aeson.eitherDecode' reqBody
        case eUpdatedEntry of
          Left err ->
            S.modifyResponse $ S.setResponseStatus 400 (BS8.pack err)
          Right updatedEntry -> do
            result <- liftIO . DB.withConnection dbName $ \conn ->
              updateEntry conn updatedEntry
            let resultBs = Aeson.encode result
            S.modifyResponse $ S.setContentType "application/json"
            S.writeLBS resultBs

  , _backend_routeEncoder = fullRouteEncoder
  }

fetchEntries :: DB.Connection -> [CategoryId] -> [EntryType] -> IO [Entry HasId HasId]
fetchEntries conn catIds types = do
  let catWhere = buildWhereInClause "CategoryId" catIds
      assocQuery =
        "SELECT EntryId, CategoryId FROM " <> entryToCatTable
          <> catWhere <> " ORDER BY EntryId ASC"

  entryToCatAssocs
    <- DB.query conn assocQuery catIds

  let entryToCatMap :: M.Map EntryId [CategoryId]
      entryToCatMap = M.fromAscListWith (++) $ do
        (entryId, catId) <- entryToCatAssocs
        [(entryId, [catId])]

      entryIds = S.toList $ M.keysSet entryToCatMap
      catIds'  = S.toList . S.fromList $ snd <$> entryToCatAssocs

  entries <-
    if null entryIds
      then pure []
      else do
        let entryWhere =
              if null catIds
                 then ""
                 else buildWhereInClause "Id" entryIds

            typeFilter =
              if null types
                 then ""
                 else " AND Type IN ("
                      <> mconcat
                           (L.intersperse "," $ replicate (length types) "?")
                      <> ")"

            entryQuery =
              "SELECT Id, Name, Body, Refs, Type, Added FROM " <> entriesTable
              <> entryWhere <> typeFilter

        DB.query conn entryQuery (entryIds DB.:. types)

  if null entries
    then pure []
    else do
      let catWhere' = buildWhereInClause "Id" catIds'
          catQuery = "SELECT Id, Name FROM categories" <> catWhere'
      cats <- M.fromList <$> DB.query conn catQuery catIds'

      pure $ mapMaybe (mkEntry cats entryToCatMap) entries

mkEntry
  :: M.Map CategoryId T.Text
  -> M.Map EntryId [CategoryId]
  -> (EntryId, T.Text, T.Text, T.Text, EntryType, UTCTime)
  -> Maybe (Entry HasId HasId)
mkEntry catToBodyMap entryToCatMap
        (entryId, entryName, entryBody, entryReferences, entryType, entryAdded)
  = do
    catIds <- M.lookup entryId entryToCatMap
    catNames <- traverse (catToBodyMap M.!?) catIds
    let cats = zipWith Category catIds catNames

    pure Entry { entryCategories = cats
               , ..
               }

buildWhereInClause :: DB.Query -> [a] -> DB.Query
buildWhereInClause colName xs =
  if null xs
     then ""
     else mconcat $
          " WHERE " <> colName <> " IN ("
          : L.intersperse "," (replicate (length xs) "?")
         ++ [")"]

insertEntry :: DB.Connection -> Entry NoId MbId -> IO (Entry HasId HasId)
insertEntry conn entry = do
  let (existingCats, newCats) = partitionCats $ entryCategories entry

  DB.withTransaction conn $ do
    -- insert new entry
    DB.execute conn
      ("INSERT INTO " <> entriesTable
      <> " (Name, Body, Refs, Type, Added) VALUES (?,?,?,?,?)")
        ( entryName entry
        , entryBody entry
        , entryReferences entry
        , entryType entry
        , entryAdded entry
        )

    newId <- EntryId <$> DB.lastInsertRowId conn

    insertedCats <- insertMultipleCats conn newCats

    -- insert associations
    DB.executeMany conn
      ("INSERT INTO " <> entryToCatTable <> " (EntryId, CategoryId) VALUES (?, ?)")
      $ zip (repeat newId) (categoryId <$> insertedCats ++ existingCats)

    pure entry
      { entryId = newId
      , entryCategories = existingCats ++ insertedCats
      }

insertMultipleCats :: DB.Connection -> [Category NoId] -> IO [Category HasId]
insertMultipleCats conn newCats = do
  DB.executeMany conn ("INSERT INTO " <> categoriesTable <> " (Name) VALUES (?)")
    $ DB.Only . categoryName <$> newCats

  lastCatId <- DB.lastInsertRowId conn

  let newCatIds = CategoryId
        <$> [lastCatId - (fromIntegral (length newCats) - 1)
            .. lastCatId
            ]

      insertedCats =
        zipWith (\c i -> c { categoryId = i })
                newCats
                newCatIds

  pure insertedCats

deleteEntry :: DB.Connection -> EntryId -> IO ()
deleteEntry conn entryId = do
  catIds <- map DB.fromOnly <$>
    DB.query conn ("SELECT CategoryId FROM " <> entryToCatTable
                <> " WHERE EntryId = ?"
                  ) (DB.Only entryId)

  DB.withTransaction conn $ do
    -- associations are auto deleted by foreign key cascading
    DB.execute conn ("DELETE FROM " <> entriesTable <> " WHERE Id = ?") (DB.Only entryId)

    -- check if any of the cats are no longer referenced
    let whereCond = buildWhereInClause "CategoryId" catIds
    remainingCatIds
      <- map DB.fromOnly <$>
         DB.query conn ("SELECT CategoryId FROM " <> entryToCatTable <> whereCond)
         catIds

    let catsToDelete = catIds L.\\ remainingCatIds :: [CategoryId]

    -- can do DELETE OR IGNORE here?
    unless (null catsToDelete) $
      DB.executeMany conn ("DELETE FROM " <> categoriesTable <> " WHERE Id = ?")
        (DB.Only <$> catsToDelete)

updateEntry :: DB.Connection -> Entry HasId MbId -> IO (Entry HasId HasId)
updateEntry conn entry@Entry{..} = do
  let (existingCats, newCats) = partitionCats entryCategories

  DB.withTransaction conn $ do
    insertedCats <- insertMultipleCats conn newCats

    let allCats = existingCats <> insertedCats
    -- create associations
    DB.executeMany conn
      ("INSERT OR IGNORE INTO " <> entryToCatTable <>
       " (EntryId, CategoryId) VALUES (?, ?)")
      $ repeat entryId `zip` (categoryId <$> allCats)

    -- update the entry
    DB.execute conn
      "UPDATE entry SET Name = ?, Body = ?, Refs = ?, Type = ? WHERE Id = ?"
      (entryName, entryBody, entryReferences, entryType, entryId)

    pure entry
      { entryCategories = allCats }
