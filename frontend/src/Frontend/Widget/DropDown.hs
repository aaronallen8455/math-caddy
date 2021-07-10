module Frontend.Widget.DropDown
  ( dropDown
  ) where

import           Control.Arrow
import           Control.Monad.Fix
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Reflex.Dom.Core

dropDown :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Ord a, Show a)
         => a -> [a] -> m (Dynamic t a)
dropDown selected options = do
  let optMap = M.fromList $ (id &&& T.toTitle . T.pack . show) <$> options

  _dropdown_value <$> dropdown selected (pure optMap) def
