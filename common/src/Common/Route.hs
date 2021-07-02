{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Common.Route where

import Prelude hiding (id, (.))
import Control.Category
import qualified Control.Categorical.Bifunctor as Cat
import qualified Control.Categorical.Functor as Cat
import Control.Lens (iso)
import           Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text (Text)
import Data.Functor.Identity

import Obelisk.Route
import Obelisk.Route.TH

import           Common.Api

type Encoder' = Encoder (Either Text) (Either Text)

data BackendRoute :: * -> * where
  BackendRouteMissing :: BackendRoute ()
  BackendRouteGetFiltered :: BackendRoute ([CategoryId], [EntryType])
  BackendRouteAddEntry :: BackendRoute ()
  BackendRouteDeleteEntry :: BackendRoute EntryId
  BackendRouteUpdateEntry :: BackendRoute ()

data FrontendRoute :: * -> * where
  FrontendRouteMain :: FrontendRoute ()

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRouteMissing :/ ())
  (\case
      BackendRouteMissing -> PathSegment "missing" $ unitEncoder mempty
      BackendRouteGetFiltered -> PathSegment "filtered"
        $ queryOnlyEncoder
        . filterQueryEncoder
        . Cat.bimap
            (Cat.fmap commaListEncoder . emptyListEncoder . Cat.fmap categoryIdEncoder)
            (Cat.fmap commaListEncoder . emptyListEncoder . Cat.fmap entryTypeEncoder)
      BackendRouteAddEntry ->
        PathSegment "add-entry" $ unitEncoder mempty
      BackendRouteDeleteEntry ->
        PathSegment "delete-entry" $ singlePathSegmentEncoder . entryIdEncoder
      BackendRouteUpdateEntry ->
        PathSegment "update-entry" $ unitEncoder mempty
  )
  (\case
      FrontendRouteMain -> PathEnd $ unitEncoder mempty
  )

commaListEncoder :: Encoder' [Text] Text
commaListEncoder = viewEncoder $ iso from to
  where
    from = T.intercalate ","
    to = T.split (== ',')

emptyListEncoder :: Encoder' [a] (Maybe [a])
emptyListEncoder = viewEncoder $ iso from concat
  where
    from [] = Nothing
    from xs = Just xs

filterQueryEncoder :: Encoder' (Maybe Text, Maybe Text) (M.Map Text (Maybe Text))
filterQueryEncoder = viewEncoder $ iso from to
  where
    from (cats, types) = M.fromList [("cats", cats), ("types", types)]
    to m = (join $ M.lookup "cats" m, join $ M.lookup "types" m)

entryTypeEncoder :: Encoder' EntryType Text
entryTypeEncoder =
  enumEncoder $ \case
    Theorem     -> "theorem"
    Lemma       -> "lemma"
    Definition  -> "definition"
    Corollary   -> "corollary"
    Proposition -> "proposition"

categoryIdEncoder :: Encoder' CategoryId Text
categoryIdEncoder = unsafeTshowEncoder . unwrappedEncoder

entryIdEncoder :: Encoder' EntryId Text
entryIdEncoder = unsafeTshowEncoder . unwrappedEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
