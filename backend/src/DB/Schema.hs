module DB.Schema
  ( dbName
  , entriesTable
  , categoriesTable
  , entryToCatTable
  , createTablesIfNotExist
  ) where

import qualified Database.SQLite.Simple as DB

dbName :: String
dbName = "math-caddy.db"

entriesTable :: DB.Query
entriesTable = "entries"

categoriesTable :: DB.Query
categoriesTable = "categories"

entryToCatTable :: DB.Query
entryToCatTable = "entry_to_category"

createTablesIfNotExist :: IO ()
createTablesIfNotExist =
  DB.withConnection dbName $ \conn -> do
    -- Entries table
    DB.execute_ conn
      $ "CREATE TABLE IF NOT EXISTS " <> entriesTable
      <> " (Id INTEGER PRIMARY KEY, Name TEXT NOT NULL, Body TEXT NOT NULL, \
         \Type TEXT NOT NULL, Refs TEXT NOT NULL, Added TEXT NOT NULL)"

    -- Categories table
    DB.execute_ conn
      $ "CREATE TABLE IF NOT EXISTS " <> categoriesTable
      <> " (Id INTEGER PRIMARY KEY, Name TEXT NOT NULL UNIQUE)"

    -- Entry to category association table
    DB.execute_ conn
      $ "CREATE TABLE IF NOT EXISTS " <> entryToCatTable
      <> " (Id INTEGER PRIMARY KEY, \
         \EntryId INTEGER NOT NULL, \
         \CategoryId INTEGER NOT NULL, \
         \FOREIGN KEY(EntryId) REFERENCES entries(Id) ON DELETE CASCADE, \
         \FOREIGN KEY(CategoryId) REFERENCES categories(Id) ON DELETE CASCADE, \
         \UNIQUE (EntryId, CategoryId))"
