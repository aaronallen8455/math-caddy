{-# LANGUAGE RecordWildCards #-}
module DB.Queries
  ( fetchEntries
  , insertEntry
  , deleteEntry
  , updateEntry
  , getCategories
  ) where

import           Control.Monad
import qualified Data.List as L
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as DB

import           Common.Api
import           DB.Schema

-- | Fetches a page worth of entries matching the given filter params
fetchEntries :: DB.Connection
             -> Maybe EntryId
             -> PageSize
             -> Maybe Needle
             -> [CategoryId]
             -> [EntryType]
             -> IO [Entry HasId HasId]
fetchEntries conn mLowerBound pageSize mNeedle catIds types = do
  let catWhere = buildWhereInClause "CategoryId" catIds
      assocQuery =
        "SELECT EntryId, CategoryId FROM " <> entryToCatTable
          <> catWhere <> " AND EntryId > ? ORDER BY EntryId ASC"

  entryToCatAssocs
    <- DB.query conn assocQuery
         (catIds DB.:. DB.Only (fromMaybe 0 mLowerBound))

  let entryToCatMap :: M.Map EntryId [CategoryId]
      entryToCatMap = M.fromAscListWith (++) $ do
        (entryId, catId) <- entryToCatAssocs
        [(entryId, [catId])]

      entryIds = S.toList $ M.keysSet entryToCatMap

  entries <-
    if null entryIds
      then pure []
      else do
        let entryWhere =
              if null entryIds
                 then ""
                 else buildWhereInClause "Id" entryIds

            typeFilter =
              if null types
                 then ""
                 else " AND Type IN ("
                      <> mconcat
                           (L.intersperse "," $ replicate (length types) "?")
                      <> ")"

            limitClause = " LIMIT ?"

            needleLike =
              case mNeedle of
                Just _ -> " AND (Name LIKE ? OR Body LIKE ?)"
                Nothing -> ""
            needleQuery :: [T.Text]
            needleQuery = maybe [] (\x -> [x, x]) $ do
              k <- mNeedle
              pure $ "%" <> k <> "%"

            entryQuery =
              "SELECT Id, Name, Body, Refs, Type, Added FROM " <> entriesTable
              <> entryWhere <> typeFilter <> needleLike <> limitClause

        DB.query conn entryQuery
          ( entryIds DB.:.
            types DB.:.
            needleQuery DB.:.
            DB.Only pageSize
          )

  if null entries
    then pure []
    else do
      let extractId (entryId, _, _, _, _, _) = entryId
          entryIdSet = S.fromAscList $ extractId <$> entries
          catIds' = S.toList . S.fromList . concat . M.elems
                  $ M.restrictKeys entryToCatMap entryIdSet :: [CategoryId]
          catWhere' = buildWhereInClause "Id" catIds'
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
     then " WHERE TRUE"
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
  DB.executeMany conn ("INSERT OR IGNORE INTO " <> categoriesTable <> " (Name) VALUES (?)")
    $ DB.Only . categoryName <$> newCats

  lastCatId <- DB.lastInsertRowId conn
  numInserted <- DB.changes conn

  let newCatIds = CategoryId
        <$> [lastCatId - (fromIntegral numInserted - 1)
            .. lastCatId
            ]

      insertedCats =
        zipWith (\c i -> c { categoryId = i })
                newCats
                newCatIds

  pure insertedCats

-- | Delete a given entry and any categories that are no longer referenced by
-- any entries. Returns the category Ids that were transitively deleted.
deleteEntry :: DB.Connection -> EntryId -> IO [CategoryId]
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

    pure catsToDelete

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

getCategories :: DB.Connection -> IO [Category HasId]
getCategories conn = do
  results <-
    DB.query_ conn ("SELECT DISTINCT cat.Id, Name FROM " <> categoriesTable
                 <> " cat JOIN " <> entryToCatTable
                 <> " assoc ON cat.Id = assoc.CategoryId"
                   )

  pure $ uncurry Category <$> results
