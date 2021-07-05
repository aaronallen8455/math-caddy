module Frontend.Model
  ( M(..)
  , Ev(..)
  , initModel
  , applyEvent
  ) where

import           Control.Arrow
import qualified Data.Map.Strict as M

import           Common.Api
import qualified Frontend.Model.Entries as Entries
import qualified Frontend.Model.Filter as Filter

type CategoryMap = M.Map CategoryId (Category HasId)

-- would be better to have these as Maps
data M =
  M { modelEntries :: Entries.M
    , modelCategories :: CategoryMap
    , modelFilter :: Filter.M
    } deriving Show

initModel :: M
initModel =
  M { modelEntries = Entries.initModel
    , modelCategories = mempty
    , modelFilter = Filter.initModel
    }

data Ev
  = Replace Entries.M [Category HasId]
  | FilterEv Filter.Ev
  | EntriesEv Entries.Ev
  | ReqFilterChange Filter.M
  | ReqMoreEntries Filter.M
  | FilterReqFailed
  | RemoveCategories [CategoryId]
  | AddCategories [Category HasId]

applyEvent :: Ev -> M -> M
applyEvent ev m = case ev of
  Replace e c ->
    m { modelEntries = e
      , modelCategories = M.fromList $ (categoryId &&& id) <$> c
      }

  FilterEv fEv ->
    m { modelFilter = Filter.applyEvent fEv $ modelFilter m }

  EntriesEv eEv ->
    m { modelEntries = Entries.applyEvent eEv $ modelEntries m }

  FilterReqFailed -> m
