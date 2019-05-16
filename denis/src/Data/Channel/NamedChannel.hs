{-# LANGUAGE
DataKinds ,
DeriveGeneric ,
OverloadedLabels,
OverloadedStrings ,
TypeApplications ,
TypeOperators,
OverloadedStrings,
RecordWildCards #-}

module Data.Channel.NamedChannel where

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import Servant.Docs (ToSample, toSamples, samples)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Vector as V
import Data.User
import qualified Data.Set as Set
import Data.Text.Validator
import Data.Tags.Validation
import Data.Limits

-- MARK: Documentation

instance (ToSample u) => ToSample (NamedChannel u) where
    toSamples _ = samples $ NamedChannel <$> [8] <*> ["социология", "ФКН"] <*> [V.fromList ["thisIsHashTag", "thisIsAlsoHashTag"]] <*> [V.fromList (map snd $ toSamples Proxy)]

instance ToSample NamedChannelCreationRequest where
    toSamples _ = samples $ NamedChannelCreationRequest <$> ["социология", "ФКН"] <*> [V.fromList ["thisIsHashTag", "thisIsAlsoHashTag"]] <*> [V.fromList [2, 7, 8]]

instance HasValidatableText (NamedChannel u) where
    validateText NamedChannel{..} = validateText (namedChannelName ~< maxChannelName) && all (validateText . (~= validateTag')) namedChannelTags

instance HasValidatableText (NamedChannelCreationRequest) where
    validateText NamedChannelCreationRequest{..} = validateText (namedChannelCreationRequestName ~< maxChannelName) && all validateText namedChannelCreationRequestTags

-- MARK: Implementation

-- |Тип -- идентификатор именнованного канала.
type NamedChannelId = Int64

-- Note: Channel owner only exists in sql
-- |Структура данных именованного канала.
data NamedChannel u = NamedChannel {
    -- |Идентификатор именованного канала.
    namedChannelId :: NamedChannelId,
    -- |Имя именнованного канала.
    namedChannelName :: Text,
    -- |Список тегов в именованном канале.
    namedChannelTags :: V.Vector Text,
    -- |Список идентификаторов пользователей в именованном канале.
    namedChannelPeopleIds :: V.Vector u
} deriving GHC.Generic

instance (ToJSON u) => ToJSON (NamedChannel u) where
    toJSON (NamedChannel cId cName cTags cPeople) = object [
        "id" .= cId,
        "name" .= cName,
        "tags" .= cTags,
        "people" .= cPeople ]

instance (FromJSON u, Ord u) => FromJSON (NamedChannel u) where
    parseJSON = withObject "named channel" $ \e ->
        NamedChannel <$> e .: "id" <*> e .: "name" <*> fmap removeDuplicates (e .: "tags") <*> fmap removeDuplicates (e .: "people")
        where
            removeDuplicates :: Ord a => [a] -> V.Vector a
            removeDuplicates = V.fromList . Set.toList . Set.fromList

-- MARK: NamedChannelCreationRequest

-- |Объект-запрос на создание именованного канала.
data NamedChannelCreationRequest = NamedChannelCreationRequest {
    -- |Название канала.
    namedChannelCreationRequestName :: Text,
    -- |Список тегов в канале.
    namedChannelCreationRequestTags :: V.Vector Text,
    -- |Список идентификаторов пользователей в канале.
    namedChannelCreationRequestPeopleIds :: V.Vector UserId
} deriving GHC.Generic

instance SOP.Generic NamedChannelCreationRequest
instance SOP.HasDatatypeInfo NamedChannelCreationRequest

instance ToJSON NamedChannelCreationRequest where
    toJSON (NamedChannelCreationRequest cName cTags cPeople) = object [
        "name" .= cName,
        "tags" .= cTags,
        "people" .= cPeople ]

instance FromJSON NamedChannelCreationRequest where
    parseJSON = withObject "named channel" $ \e ->
        NamedChannelCreationRequest <$> e .: "name" <*> fmap removeDuplicates (e .: "tags") <*> fmap removeDuplicates (e .: "people")
        where
            removeDuplicates :: Ord a => [a] -> V.Vector a
            removeDuplicates = V.fromList . Set.toList . Set.fromList
