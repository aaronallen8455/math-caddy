module Frontend.Async.Ev
  ( Ev(..)
  ) where

import           Common.Api
import qualified Frontend.Model.Filter as Filter

data Ev
  = RequestEntries Filter.M
  | DeleteEntry EntryId
  | SaveNewEntry (Entry NoId MbId)
  | UpdateEntry (Entry HasId MbId)
  | Init
  deriving Show
