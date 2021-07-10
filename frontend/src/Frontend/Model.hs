module Frontend.Model
  ( M(..)
  , Ev(..)
  , initModel
  , applyEvents
  , mkReplaceEv
  , CategoryMap
  ) where

import           Control.Arrow
import qualified Data.List as L
import qualified Data.Map.Strict as M

import           Common.Api
import qualified Frontend.Model.Entries as Entries
import qualified Frontend.Model.Filter as Filter

type CategoryMap = M.Map CategoryId (Category HasId)

-- would be better to have these as Maps
data M =
  M { modelEntries :: !Entries.M
    , modelCategories :: !CategoryMap
    , modelFilter :: !Filter.M
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
  | FilterReqFailed
  | RemoveCategories [CategoryId]
  | AddOrReplaceCategories [Category HasId]

applyEvents :: [Ev] -> M -> M
applyEvents evs m = foldr applyEvent m evs

applyEvent :: Ev -> M -> M
applyEvent ev m = case ev of
  Replace e c ->
    m { modelEntries = e
      , modelCategories = M.fromList $ (categoryId &&& id) <$> c
      }

  FilterEv fEv ->
    m { modelFilter = Filter.applyEvent fEv $ modelFilter m }

  EntriesEv eEv ->
    let m' = case eEv of
               -- changing the lowerBound will trigger loading more
               Entries.LoadMore eId ->
                 m { modelFilter = (modelFilter m)
                       { Filter.filterLowerBound = Just eId }
                   }
               _ -> m

     in m' { modelEntries = Entries.applyEvent eEv $ modelEntries m' }

  FilterReqFailed -> m

  ReqFilterChange _ -> m

  RemoveCategories catIds ->
    m { modelCategories =
          L.foldl' (flip M.delete) (modelCategories m) catIds
      }

  AddOrReplaceCategories cats ->
    m { modelCategories =
          L.foldl' (\cs c -> M.insert (categoryId c) c cs) (modelCategories m) cats
      }

mkReplaceEv :: [Entry HasId HasId] -> [Category HasId] -> Ev
mkReplaceEv entries = Replace entM
  where
    entM = Entries.M
      { Entries.entries = M.fromList $ (entryId &&& id) <$> entries
      , Entries.editorStatus = Entries.NotEditing
      , Entries.waitingForMore = False
      }
