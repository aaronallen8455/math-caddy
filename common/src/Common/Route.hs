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

import           Prelude hiding (id, (.))
import           Control.Category
import qualified Control.Categorical.Bifunctor as Cat
import qualified Control.Categorical.Functor as Cat
import           Control.Lens (iso)
import           Control.Monad
import           Data.Functor.Identity
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text (Text)

import           Obelisk.Route
import           Obelisk.Route.TH

import           Common.Api

type Encoder' = Encoder (Either Text) (Either Text)

data BackendRoute :: * -> * where
  BackendRouteMissing :: BackendRoute ()
  BackendRouteGetFiltered
    :: BackendRoute (Maybe EntryId, (PageSize, (Maybe Needle, ([CategoryId], [EntryType]))))
  BackendRouteAddEntry :: BackendRoute ()
  BackendRouteDeleteEntry :: BackendRoute EntryId
  BackendRouteUpdateEntry :: BackendRoute ()
  BackendRouteGetCategories :: BackendRoute ()
  BackendRouteInit :: BackendRoute PageSize

data FrontendRoute :: * -> * where
  FrontendRouteMain :: FrontendRoute ()

-- can use `getAndDecode` on the frontend to call backend endpoints

type CheckedFullEncoder = Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName

(&|&) :: Cat.Bifunctor p r s t => r a b -> s c d -> t (p a c) (p b d)
(&|&) = Cat.bimap
infixr 4 &|&

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRouteMissing :/ ())
  (\case
      BackendRouteMissing -> PathSegment "missing" $ unitEncoder mempty
      BackendRouteGetFiltered -> PathSegment "filtered"
        $ queryOnlyEncoder
        . filterQueryEncoder
        . ( Cat.fmap entryIdEncoder
        &|& justEncoder . unsafeTshowEncoder
        &|& id
        &|& Cat.fmap commaListEncoder . emptyListEncoder . Cat.fmap categoryIdEncoder
        &|& Cat.fmap commaListEncoder . emptyListEncoder . Cat.fmap entryTypeEncoder
          )

      BackendRouteAddEntry ->
        PathSegment "add-entry" $ unitEncoder mempty
      BackendRouteDeleteEntry ->
        PathSegment "delete-entry" $ singlePathSegmentEncoder . entryIdEncoder
      BackendRouteUpdateEntry ->
        PathSegment "update-entry" $ unitEncoder mempty
      BackendRouteGetCategories ->
        PathSegment "get-categories" $ unitEncoder mempty
      BackendRouteInit ->
        PathSegment "init" $ singlePathSegmentEncoder . unsafeTshowEncoder
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

filterQueryEncoder :: Encoder' (Maybe Text, (Maybe Text, (Maybe Text, (Maybe Text, Maybe Text))))
                               (M.Map Text (Maybe Text))
filterQueryEncoder = viewEncoder $ iso from to
  where
    from (minId, (size, (needle, (cats, types)))) =
      M.fromList $
        [ ("size", size)
        , ("cats", cats)
        , ("types", types)
        ] ++ [ ("min", minId) | isJust minId ]
          ++ [ ("needle", needle) | isJust needle ]

    to m = ( join $ M.lookup "min" m
           , ( join $ M.lookup "size" m
             , ( join $ M.lookup "needle" m
               , ( join $ M.lookup "cats" m
                 , join $ M.lookup "types" m
                 )
               )
             )
           )

entryTypeEncoder :: Encoder' EntryType Text
entryTypeEncoder = enumEncoder entryTypeToText

categoryIdEncoder :: Encoder' CategoryId Text
categoryIdEncoder = unsafeTshowEncoder . integralEncoder

entryIdEncoder :: Encoder' EntryId Text
entryIdEncoder = unsafeTshowEncoder . integralEncoder

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
