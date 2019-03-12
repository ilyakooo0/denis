{-# LANGUAGE OverloadedStrings #-}

module Server.LimitingRequest (
    LimitingRequest(..)
) where

import Data.Word (Word64)
import Servant.Docs (ToSample, toSamples, samples)
import Data.Aeson
import Data.Proxy

instance (ToSample r) => ToSample (LimitingRequest r) where
    toSamples _ = samples $ LimitingRequest <$> (map snd $ toSamples Proxy) <*> [1, 11, 17] 


data LimitingRequest r = LimitingRequest {
    requestRequest :: r,
    requestLimit :: Word64
}

instance (ToJSON r) => ToJSON (LimitingRequest r) where
    toJSON (LimitingRequest r l) = object [
        "request" .= r,
        "limit" .= l ] 

instance (FromJSON r) => FromJSON (LimitingRequest r) where
    parseJSON = withObject "limiting request" $ \e -> 
        LimitingRequest <$> e .: "request" <*> e .: "limit"

