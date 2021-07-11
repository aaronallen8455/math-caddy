module Frontend.Model
  ( M(..)
  , Ev(..)
  , initModel
  , applyEvents
  , CategoryMap
  ) where

import           Control.Arrow
import qualified Data.List as L
import qualified Data.Map.Strict as M

import           Common.Api
import qualified Frontend.Async.Ev as Async
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
  | FilterReqFailed
  | RemoveCategories [CategoryId]
  | AddOrReplaceCategories [Category HasId]
  deriving Show

applyEvents :: [Ev] -> (M, [Async.Ev]) -> (M, [Async.Ev])
applyEvents evs (m, _) = foldr applyEvent (m, []) evs

applyEvent :: Ev -> (M, [Async.Ev]) -> (M, [Async.Ev])
applyEvent ev (m, asyncEvs) = case ev of
  Replace e c ->
    ( m { modelEntries = e
        , modelCategories = M.fromList $ (categoryId &&& id) <$> c
        }
    , asyncEvs
    )

  FilterEv fEv ->
    let newFilter = Filter.applyEvent fEv $ modelFilter m
     in ( m { modelFilter = Filter.applyEvent fEv $ modelFilter m }
        -- Request entries for any filter event
        , Async.RequestEntries newFilter : asyncEvs
        )

  EntriesEv eEv ->
    let (m', asyncEvs') = case eEv of
          Entries.LoadMore eId ->
            let newFilter = (modelFilter m)
                  { Filter.filterLowerBound = Just eId }

             in ( m { modelFilter = newFilter }
                , Async.RequestEntries newFilter : asyncEvs
                )

          Entries.ReqSaveNewEntry ent ->
            (m, Async.SaveNewEntry ent : asyncEvs)

          Entries.ReqUpdateEntry ent ->
            (m, Async.UpdateEntry ent : asyncEvs)

          Entries.ReqDeleteEntry eId ->
            (m, Async.DeleteEntry eId : asyncEvs)

          _ -> (m, asyncEvs)

     in ( m' { modelEntries = Entries.applyEvent eEv $ modelEntries m' }
        , asyncEvs'
        )

  FilterReqFailed -> (m, asyncEvs)

  RemoveCategories catIds ->
    ( m { modelCategories =
            L.foldl' (flip M.delete) (modelCategories m) catIds
        }
    , asyncEvs
    )

  AddOrReplaceCategories cats ->
    ( m { modelCategories =
            L.foldl' (\cs c -> M.insert (categoryId c) c cs) (modelCategories m) cats
        }
    , asyncEvs
    )
