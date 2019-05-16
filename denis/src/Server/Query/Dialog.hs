{-# LANGUAGE
    OverloadedStrings,
    DeriveGeneric #-}

module Server.Query.Dialog where

import Data.Aeson
import Data.Message
import Data.GroupChat
import Data.User
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Squeal.PostgreSQL.Schema
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.PostElement
import Servant.Docs
import Data.Proxy

instance ToSample Dialog where
    toSamples _ = samples (take 1 (UserDialog <$> [69] <*> map snd (toSamples Proxy)) <>
        (GroupDialog <$> map snd (toSamples Proxy) <*> map snd (toSamples Proxy)))

-- |Объект диалога
data Dialog =
    -- |Thing
    GroupDialog GroupChat (Maybe Message) |
    -- |Also a thing
    UserDialog UserId Message
    deriving Show

instance ToJSON Dialog where
    toJSON (GroupDialog chat mess) = object [
        "groupChat" .= object [
            "group" .= chat,
            "lastMessage" .= mess
            ]
        ]
    toJSON (UserDialog uId mess) = object [
        "userChat" .= object [
            "user" .= uId,
            "lastMessage" .= mess
            ]
        ]

data ChatsResponse =
    ChatsResponse {
    chatResponseMessageId :: MessageId,
    chatResponseMessageAuthorId :: Maybe UserId,
    chatResponseMessageDestinationGroupId :: Maybe GroupChatId,
    chatResponseMessageDestinationUserId :: Maybe UserId,
    chatResponseMessageBody :: Maybe (Jsonb (PostElement Message)),
    chatResponseMessageTime :: UTCTime,
    chatResponseGroupUsers :: Maybe (Jsonb UserPermissions),
    chatResponseGroupName :: Maybe Text
} deriving (GHC.Generic)

instance SOP.Generic ChatsResponse
instance SOP.HasDatatypeInfo ChatsResponse

responseToDialogs :: UserId -> [ChatsResponse] -> Maybe [Dialog]
responseToDialogs selfId = sequence . map toDialog
    where
        toDialog :: ChatsResponse -> Maybe Dialog
        toDialog (ChatsResponse mId (Just uId) Nothing (Just dId) (Just (Jsonb mb)) mt Nothing Nothing) =
            Just $ UserDialog
                (if selfId == uId then dId else uId)
                (Message mId uId (UserChatDestination dId) mb mt)
        toDialog (ChatsResponse mId (Just uId) (Just gId) Nothing (Just (Jsonb mb)) mt (Just users) (Just name)) =
            Just $ GroupDialog
                (GroupChat gId users name)
                (Just $ Message mId uId (GroupChatDestination gId) mb mt)
        toDialog (ChatsResponse _ Nothing (Just gId) Nothing Nothing _ (Just users) (Just name)) =
            Just $ GroupDialog
                (GroupChat gId users name)
                Nothing
        toDialog _ = Nothing

