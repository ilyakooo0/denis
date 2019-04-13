{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}

module Data.Channel.AnonymousChannel (
    AnonymousChannelId,
    AnonymousChannel(..)
) where

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import Servant.Docs (ToSample, toSamples, samples)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.User    
import qualified Data.Set as Set


-- MARK: Documentation

instance ToSample AnonymousChannel where
    toSamples _ = samples $ AnonymousChannel <$> [V.fromList ["thisIsHashTag", "thisIsAlsoHashTag"]] <*> [[2, 7, 8]]
    
-- MARK: Implementation

type AnonymousChannelId = Int64

data AnonymousChannel = AnonymousChannel {
    anonymousChannelTags :: V.Vector Text,
    anonymousChannelPeopleIds :: [UserId]
} deriving GHC.Generic

instance SOP.Generic AnonymousChannel
instance SOP.HasDatatypeInfo AnonymousChannel

instance ToJSON AnonymousChannel where
    toJSON (AnonymousChannel cTags cPeople) = object [
        "tags" .= cTags,
        "people" .= cPeople ]

instance FromJSON AnonymousChannel where
    parseJSON = withObject "anonymous channel" $ \e -> 
        AnonymousChannel <$> fmap (V.fromList . removeDuplicates) (e .: "tags") <*> fmap removeDuplicates (e .: "people")
        where 
            removeDuplicates :: Ord a => [a] -> [a]
            removeDuplicates = Set.toList . Set.fromList

        
