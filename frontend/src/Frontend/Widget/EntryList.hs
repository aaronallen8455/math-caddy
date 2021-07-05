module Frontend.Widget.EntryList
  ( entryListWidget
  ) where

import qualified Data.Map.Strict as M

newtype M =
  M { entries      :: EntryMap
    , editorStatus :: EditorStatus
    }

entryListWidget :: _ => Dynamic t M -> m (Event t Ev)
entryListWidget modelDyn = do
  entryEvents <- leftmost . fmap M.elems
             <$> listViewWithKey (entries <$> modelDyn) (const entryWidget)
