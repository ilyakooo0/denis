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
    PaginatingRequest(..),
    PaginationDirection(..),
    validatePaginationRequest
    ) where

import GHC.Generics
import Data.Aeson as A
import Servant.Docs
import Data.Proxy
import Data.Word (Word64)

instance (ToSample i, ToSample r) => ToSample (PaginatingRequest i r) where
    toSamples _ = samples $ PaginatingRequest <$> ([const Nothing, Just] <*> [snd . head $ toSamples Proxy]) <*> [20] <*> map snd (toSamples Proxy) <*> [BackPagination, ForwardPagination]

data PaginatingRequest i r = PaginatingRequest {
    exclusiveFrom :: Maybe i,
    limit :: Word64,
    request :: r,
    direction :: PaginationDirection
} deriving (Generic)

validatePaginationRequest :: (PaginatingRequest i r) -> Bool
validatePaginationRequest PaginatingRequest{limit=lim} = lim > 0 && lim < 100

instance (ToJSON r, ToJSON i) => ToJSON (PaginatingRequest i r) where
instance (FromJSON r, FromJSON i) => FromJSON (PaginatingRequest i r) where

data PaginationDirection = BackPagination | ForwardPagination

instance ToJSON PaginationDirection where
    toJSON BackPagination = A.String "backward"
    toJSON ForwardPagination = A.String "forward"

instance FromJSON PaginationDirection where
    parseJSON (A.String "backward") = return BackPagination
    parseJSON (A.String "forward") = return ForwardPagination
    parseJSON _ = fail "couldn't match magination direction."