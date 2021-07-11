module Frontend.Model.Filter
  ( M(..)
  , Ev(..)
  , initModel
  , applyEvent
  ) where

import qualified Data.List as L

import           Common.Api

import           Debug.Trace

data M =
  M { filterCategory :: ![CategoryId]
    , filterEntryType :: ![EntryType]
    , filterNeedle :: !(Maybe Needle)
    , filterLowerBound :: !(Maybe EntryId)
    } deriving (Show, Eq)

initModel :: M
initModel =
  M { filterCategory = []
    , filterEntryType = []
    , filterNeedle = Nothing
    , filterLowerBound = Nothing
    }

data Ev
  = AddCategory CategoryId
  | RemoveCategory CategoryId
  | AddEntryType EntryType
  | RemoveEntryType EntryType
  | SetNeedle (Maybe Needle)
  deriving Show

applyEvent :: Ev -> M -> M
applyEvent ev m = case ev of
  AddCategory c -> trace "AddCategory" m { filterCategory = c : filterCategory m }
  RemoveCategory c -> trace "RemoveCategory" m { filterCategory = L.delete c (filterCategory m) }
  AddEntryType t -> m { filterEntryType = t : filterEntryType m }
  RemoveEntryType t -> m { filterEntryType = L.delete t (filterEntryType m) }
  SetNeedle n -> m { filterNeedle = n }
