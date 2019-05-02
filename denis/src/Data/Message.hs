{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Data.Message (
    MessageStorage(..),
    Message(..),
    MessageDetination(..),
    MessageId,
    restoreMessage,
    storeMessage,
    MessageCreation(..)
    ) where

import Data.Aeson
import Data.PostElement
import Data.User
import Data.Int (Int64)
import Data.GroupChat
import Data.Time
import Squeal.PostgreSQL.Schema
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Control.Applicative
import Data.Aeson.Types
import Data.Proxy
import Servant.Docs

instance ToSample Message where
    toSamples _ = samples $ Message 8 69 <$>
        map snd (toSamples Proxy) <*>
        map snd (toSamples Proxy) <*>
        [UTCTime (ModifiedJulianDay 1000) 8]

instance ToSample MessageDetination where
    toSamples _ = samples $ [GroupChatDestination 5, UserChatDestination 69]

type GroupChatMessageId = Int64

data MessageDetination = GroupChatDestination GroupChatId | UserChatDestination UserId
    deriving (Show)

type MessageId = Int64

data Message = Message {
    messageId :: MessageId,
    messageAuthorId :: UserId,
    messageDestinationId :: MessageDetination,
    messageBody :: PostElement Message,
    messageTime :: UTCTime
} deriving (Show)

instance ToJSON Message where
    toJSON (Message mId aId dId pb mt) = object [
        "id" .= mId,
        "author" .= aId,
        case dId of
            GroupChatDestination gId -> "group" .= gId
            UserChatDestination uId -> "user" .= uId,
        "body" .= pb,
        "time" .= mt
        ]

instance FromJSON Message where
    parseJSON = withObject "message" $ \e ->
        Message
            <$> e .: "id"
            <*> e .: "author"
            <*> ((GroupChatDestination <$> e .: "group") <|> (UserChatDestination <$> e .: "user"))
            <*> e .: "body"
            <*> e .: "time"


data MessageStorage = MessageStorage {
    messageStorageId :: MessageId,
    messageStorageAuthorId :: UserId,
    messageStorageDestinationGroupId :: Maybe GroupChatId,
    messageStorageDestinationUserId :: Maybe UserId,
    messageStorageBody :: Jsonb (PostElement Message),
    messageStorageTime :: UTCTime
} deriving GHC.Generic

instance SOP.Generic (MessageStorage)
instance SOP.HasDatatypeInfo (MessageStorage)

infixl 4 <>>

(<>>) :: Applicative f => f (a -> b) -> a -> f b
f <>> a = f <*> pure a

restoreMessage :: MessageStorage -> Maybe Message
restoreMessage (MessageStorage mId aId gId uId (Jsonb pb) mt) =
    Message mId aId <$> case (gId, uId) of
            (Just gId', Nothing) -> Just $ GroupChatDestination gId'
            (Nothing, Just uId') -> Just $ UserChatDestination uId'
            _ -> Nothing
        <>> pb <>> mt

storeMessage :: Message -> MessageStorage
storeMessage (Message mId aId dId pb mt) = case dId of
    GroupChatDestination gId -> MessageStorage mId aId (Just gId) Nothing (Jsonb pb) mt
    UserChatDestination uId -> MessageStorage mId aId Nothing (Just uId) (Jsonb pb) mt

instance ToSample MessageCreation where
    toSamples _ = samples $ (MessageCreation Nothing (Just 69) <$>
        map (Jsonb . snd) (toSamples Proxy)) <>
        (MessageCreation (Just 5) Nothing <$>
        map (Jsonb . snd) (toSamples Proxy))

data MessageCreation = MessageCreation {
    messageCreationDestinationGroupId :: Maybe GroupChatId,
    messageCreationDestinationUserId :: Maybe UserId,
    messageCreationBody :: Jsonb (PostElement (MessageCreation))
} deriving (Show, GHC.Generic)

instance SOP.Generic MessageCreation
instance SOP.HasDatatypeInfo MessageCreation
instance ToJSON MessageCreation where
    toJSON (MessageCreation gId uId pb) = object $
        case (gId, uId) of
            (Just g, Nothing) -> pure $ "group" .= g
            (Nothing, Just u) -> pure $ "user" .= u
            _ -> mempty
        <> ["body" .= getJsonb pb ]

instance FromJSON MessageCreation where
    parseJSON = withObject "message creation" $ \e ->
        (MessageCreation
            <$> (Just <$> e .: "group" :: Parser (Maybe GroupChatId))
            <>> Nothing
            <*> (Jsonb <$> e .: "body")) <|>
        (MessageCreation
            <$> (pure Nothing :: Parser (Maybe GroupChatId))
            <*> (Just <$> e .: "user")
            <*> (Jsonb <$> e .: "body"))