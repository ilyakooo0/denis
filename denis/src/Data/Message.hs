{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    TupleSections,
    RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Data.Message where

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
import Data.Text.Validator

instance ToSample Message where
    toSamples _ = samples $ Message 8 69 <$>
        map snd (toSamples Proxy) <*>
        map snd (toSamples Proxy) <*>
        [UTCTime (ModifiedJulianDay 1000) 8]

instance ToSample MessageDetination where
    toSamples _ = samples $ [GroupChatDestination 5, UserChatDestination 69]

instance HasValidatableText Message where
    validateText Message{..} = validateText messageBody

instance HasValidatableText MessageCreation where
    validateText (MessageCreation _ _ (Jsonb mb)) = validateText mb

-- |Идентификатор сообщения в групповой беседе.
type GroupChatMessageId = Int64

-- |Адресат сообщения.
data MessageDetination
    -- |Групповая беседа.
    = GroupChatDestination GroupChatId
    -- |Пользоваель.
    | UserChatDestination UserId
    deriving (Show)

--  |Идентификатор сообщения
type MessageId = Int64

-- |Объект обычного представления сообщения.
data Message = Message {
    -- |Идентификатор сообщения.
    messageId :: MessageId,
    -- |Идентификатор автора сообщения.
    messageAuthorId :: UserId,
    -- |Адресат сообщения.
    messageDestinationId :: MessageDetination,
    -- |Содержание сообщения.
    messageBody :: PostElement Message,
    -- |Время отправки сообщения.
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

-- |Объект для представления сообщения в базе данных.
data MessageStorage = MessageStorage {
    -- |Идентификатор сообщения.
    messageStorageId :: MessageId,
    -- |Идентификатор автора сообщения.
    messageStorageAuthorId :: Maybe UserId,
    -- |Идентификатор групповой беседы как адресата сообщения.
    messageStorageDestinationGroupId :: Maybe GroupChatId,
    -- |Идентификатор пользователя как адресата сообщения.
    messageStorageDestinationUserId :: Maybe UserId,
    -- |Содержание сообщения.
    messageStorageBody :: Maybe (Jsonb (PostElement Message)),
    -- |Время отправки сообщения.
    messageStorageTime :: UTCTime
} deriving GHC.Generic

instance SOP.Generic (MessageStorage)
instance SOP.HasDatatypeInfo (MessageStorage)

infixl 4 <>>

-- |Оператор, применяющий функция в функторе к объекту вне функтора.
(<>>) :: Applicative f => f (a -> b) -> a -> f b
f <>> a = f <*> pure a

-- |Функция, переводящая представление сообщения из базы данных в обычное.
restoreMessage :: MessageStorage -> Maybe Message
restoreMessage (MessageStorage mId (Just aId) gId uId (Just (Jsonb pb)) mt) =
    Message mId aId <$> case (gId, uId) of
            (Just gId', Nothing) -> Just $ GroupChatDestination gId'
            (Nothing, Just uId') -> Just $ UserChatDestination uId'
            _ -> Nothing
        <>> pb <>> mt
restoreMessage _ = Nothing

-- |Функция переводящая обычное представление сообщения в представление для базы данных.
storeMessage :: Message -> MessageStorage
storeMessage (Message mId aId dId pb mt) = case dId of
    GroupChatDestination gId -> MessageStorage mId (Just aId) (Just gId) Nothing (Just (Jsonb pb)) mt
    UserChatDestination uId -> MessageStorage mId (Just aId) Nothing (Just uId) (Just (Jsonb pb)) mt

instance ToSample MessageCreation where
    toSamples _ = samples $ (MessageCreation Nothing (Just 69) <$>
        map (Jsonb . snd) (toSamples Proxy)) <>
        (MessageCreation (Just 5) Nothing <$>
        map (Jsonb . snd) (toSamples Proxy))

-- |Объект для создания сообщения.
data MessageCreation = MessageCreation {
    -- |Идентификатор групповой беседы - адресата сообщения.
    messageCreationDestinationGroupId :: Maybe GroupChatId,
    -- |Идентификатор пользователя - адресата сообщения.
    messageCreationDestinationUserId :: Maybe UserId,
    -- |Содержание сообщения.
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