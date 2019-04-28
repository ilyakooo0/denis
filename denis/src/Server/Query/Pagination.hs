{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    FlexibleInstances,
    MultiParamTypeClasses,
    TypeFamilies,
    UndecidableInstances,
    UndecidableSuperClasses #-}

module Server.Query.Pagination (
    PaginatingRequest(..)
    ) where

import Data.Query
import GHC.Generics
import Data.Aeson
import Servant.Docs
import Data.Proxy

instance (ToSample i, ToSample r) => ToSample (PaginatingRequest i r) where
    toSamples _ = samples $ PaginatingRequest <$> ([const Nothing, Just] <*> [snd . head $ toSamples Proxy]) <*> [20] <*> map snd (toSamples Proxy)

data PaginatingRequest i r = PaginatingRequest {
    exclusiveFrom :: Maybe i,
    limit :: Limit,
    request :: r
} deriving (Generic)

instance (ToJSON r, ToJSON i) => ToJSON (PaginatingRequest i r) where
instance (FromJSON r, FromJSON i) => FromJSON (PaginatingRequest i r) where
