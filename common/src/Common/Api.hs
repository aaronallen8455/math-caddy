{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Common.Api
  ( Entry(..)
  , EntryName
  , EntryType(..)
  , EntryId(..)
  , Category(..)
  , CategoryId(..)
  , MbId
  , HasId
  , NoId
  , partitionCats
  , entryTypeToText
  , PageSize
  , Needle
  ) where

import           Data.Aeson
import           Data.Int (Int64)
import           Data.Kind (Type)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Universe (Universe)
import qualified Database.SQLite.Simple as DB
import qualified Database.SQLite.Simple.FromField as DB
import qualified Database.SQLite.Simple.ToField as DB
import           GHC.Generics (Generic)

type EntryName = T.Text
type PageSize = Int
type Needle = T.Text

data IdOpt = MbId | HasId | NoId

type MbId = 'MbId
type HasId = 'HasId
type NoId = 'NoId

type family IdField (idOpt :: IdOpt) (x :: Type) :: Type where
  IdField MbId x = Maybe x
  IdField HasId x = x
  IdField NoId x = ()

data Entry (k :: IdOpt) (cat :: IdOpt) =
  Entry
    { entryId         :: IdField k EntryId
    , entryName       :: EntryName
    , entryBody       :: T.Text
    , entryReferences :: T.Text
    , entryCategories :: [Category cat]
    , entryType       :: EntryType
    , entryAdded      :: UTCTime
    -- , entryResultFrom :: [(EntryName, EntryId)] TODO
    } deriving stock Generic

deriving instance (Show (IdField k EntryId), Show (Category cat)) => Show (Entry k cat)

deriving anyclass instance
  (ToJSON (IdField k EntryId), ToJSON (Category cat))
    => ToJSON (Entry k cat)

deriving anyclass instance
  (FromJSON (IdField k EntryId), FromJSON (Category cat))
    => FromJSON (Entry k cat)

data EntryType
  = Theorem
  | Lemma
  | Definition
  | Corollary
  | Proposition
  deriving stock (Eq, Show, Bounded, Enum, Ord, Generic)
  deriving anyclass (Universe, ToJSON, FromJSON)

instance DB.FromField EntryType where
  fromField = fromSqlData . DB.fieldData
    where
      fromSqlData (DB.SQLText txt) =
        case txt of
          "theorem"     -> pure Theorem
          "lemma"       -> pure Lemma
          "definition"  -> pure Definition
          "corollary"   -> pure Corollary
          "proposition" -> pure Proposition
          _             -> fail "unkown entry type"
      fromSqlData _ = fail "expected sql text"

instance DB.ToField EntryType where
  toField = DB.SQLText . entryTypeToText

entryTypeToText :: EntryType -> T.Text
entryTypeToText = \case
  Theorem     -> "theorem"
  Lemma       -> "lemma"
  Definition  -> "definition"
  Corollary   -> "corollary"
  Proposition -> "proposition"

newtype EntryId =
  EntryId { unEntryId :: Int64 }
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (DB.ToField, DB.FromField, ToJSON, FromJSON, Integral, Real, Enum, Num)

data Category (k :: IdOpt) =
  Category
    { categoryId   :: IdField k CategoryId
    , categoryName :: T.Text
    } deriving stock Generic

deriving instance Show (IdField k CategoryId) => Show (Category k)

partitionCats :: [Category MbId] -> ([Category HasId], [Category NoId])
partitionCats = foldr go ([], []) where
  go cat (h, m) =
    case categoryId cat of
      Nothing -> (h, cat { categoryId = () } : m)
      Just i  -> (cat { categoryId = i } : h, m)

deriving anyclass instance
  (ToJSON (IdField k CategoryId)) => ToJSON (Category k)

deriving anyclass instance
  (FromJSON (IdField k CategoryId)) => FromJSON (Category k)

newtype CategoryId =
  CategoryId { unCategoryId :: Int64 }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (DB.ToField, DB.FromField, ToJSON, FromJSON, Integral, Real, Enum, Num)
