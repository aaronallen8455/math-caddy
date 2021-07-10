module Frontend.Model.Entries
  ( M(..)
  , Ev(..)
  , EditorStatus(..)
  , applyEvent
  , initModel
  ) where

import           Control.Arrow
import qualified Data.List as L
import qualified Data.Map.Strict as M

import           Common.Api

type EntryMap = M.Map EntryId (Entry HasId HasId)

data M =
  M { entries :: !EntryMap
    , editorStatus :: !EditorStatus
    , waitingForMore :: !Bool
    } deriving Show

initModel :: M
initModel =
  M { entries = mempty
    , editorStatus = NotEditing
    , waitingForMore = False
    }

data EditorStatus
  = NotEditing
  | CreateNew
  | Editing EntryId
  | WaitingForServer
  deriving Show

data Ev
  = ReqDeleteEntry EntryId
  | DeleteEntry EntryId
  | EditEntry EntryId
  | CreateNewEntry
  | ReqSaveNewEntry (Entry NoId MbId)
  | ReqUpdateEntry (Entry HasId MbId)
  | CancelEditor
  | AddOrReplaceEntries [Entry HasId HasId]
  | ReplaceAllEntries [Entry HasId HasId]
  | LoadMore EntryId
  | ServerError

-- How to handle inserting a new Entry under an established filter?
-- Could just not try to dynamically add a new entry - entries only come from
-- server requests - would need to click 'load more' to see it after creating.
-- In that case, don't need to wait for server response after filling out
-- creation form, except to wait for new categories

applyEvent :: Ev -> M -> M
applyEvent ev m =
  case ev of
    ReqDeleteEntry _ ->
      m { editorStatus = WaitingForServer }

    DeleteEntry eid ->
      m { editorStatus = NotEditing
        , entries = M.delete eid $ entries m
        }

    EditEntry eid ->
      m { editorStatus = Editing eid }

    CreateNewEntry ->
      m { editorStatus = CreateNew }

    ReqSaveNewEntry _ ->
      m { editorStatus = NotEditing }

    ReqUpdateEntry _ ->
      m { editorStatus = WaitingForServer }

    CancelEditor ->
      m { editorStatus = NotEditing }

    ServerError ->
      m { editorStatus = NotEditing }

    AddOrReplaceEntries es ->
      m { editorStatus = NotEditing
        , entries = L.foldl' (\em e -> M.insert (entryId e) e em) (entries m) es
        , waitingForMore = False
        }

    ReplaceAllEntries es ->
      m { editorStatus = NotEditing
        , entries = M.fromList $ (entryId &&& id) <$> es
        , waitingForMore = False
        }

    LoadMore _ ->
      m { waitingForMore = True
        }
