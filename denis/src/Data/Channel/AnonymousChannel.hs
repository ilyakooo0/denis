    {-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    RecordWildCards #-}

module Data.Channel.AnonymousChannel where

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import Servant.Docs (ToSample, toSamples, samples)
import Data.Text (Text)
import qualified Data.Vector as V
import Data.User
import qualified Data.Set as Set
import Data.Tags.Validation
import Data.Text.Validator

-- MARK: Documentation

instance ToSample AnonymousChannel where
    toSamples _ = samples $ AnonymousChannel <$> [V.fromList ["thisIsHashTag", "thisIsAlsoHashTag"]] <*> [[2, 7, 8]]


instance HasValidatableText AnonymousChannel where
    validateText AnonymousChannel{..} = all (validateText . (~= validateTag')) anonymousChannelTags

-- MARK: Implementation

-- |Анонимные канал -- канал, который не хрнится на сервере.
data AnonymousChannel = AnonymousChannel {
    -- |Список тегов в канале
    anonymousChannelTags :: V.Vector Text,
    -- |Список людей в анонимном канале
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


