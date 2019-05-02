{-# LANGUAGE
    OverloadedStrings,
    DeriveGeneric #-}

module Server.Query.Dialog (
    ChatsResponse(..),
    responseToDialogs,
    Dialog(..)
    ) where

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
    toSamples _ = samples ((GroupDialog <$> map snd (toSamples Proxy) <*> map snd (toSamples Proxy)) <>
        (UserDialog <$> [69] <*> map snd (toSamples Proxy)))

data Dialog = GroupDialog GroupChat Message | UserDialog UserId Message
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

data ChatsResponse = ChatsResponse {
    chatResponseMessageId :: MessageId,
    chatResponseMessageAuthorId :: UserId,
    chatResponseMessageDestinationGroupId :: Maybe GroupChatId,
    chatResponseMessageDestinationUserId :: Maybe UserId,
    chatResponseMessageBody :: Jsonb (PostElement Message),
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
        toDialog (ChatsResponse mId uId Nothing (Just dId) (Jsonb mb) mt Nothing Nothing) =
            Just $ UserDialog
                (if selfId == uId then dId else uId)
                (Message mId uId (UserChatDestination dId) mb mt)
        toDialog (ChatsResponse mId uId (Just gId) Nothing (Jsonb mb) mt (Just users) (Just name)) =
            Just $ GroupDialog
                (GroupChat gId users name)
                (Message mId uId (GroupChatDestination gId) mb mt)
        toDialog _ = Nothing

