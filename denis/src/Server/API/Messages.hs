{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Server.API.Messages (
    MessagesApi,
    messagesServer
) where

import Servant.API
import Server.App
import Servant.Server
import Data.Query
import Data.User
import Data.Connection
import Server.Query.Pagination
import Data.Message
import Data.GroupChat
import Server.Query.Dialog

type GetAllChatsDescription = Description "Gets chats starting from/ending with the given message id."
type GetGroupChatDescription = Description "Gets information about a given group chat."
type GetUpdateGroupChatDescription = Description "Updates given group chat.\n\nReturns 404 if you are not part of the group chat.\n\nReturn 401 if you do not have sufficient permissions to edit the chat."
type CreateGroupChatDescription = Description "Creates a group chat with the given user permission.\n\nThe calling user gets add and promoted to maximum permissions automatically. You do not have to include him.\n\nReturns the id of the created group chat"
type LeaveGroupChatDescription = Description "Removes the user from the given group chat."
type MessagesGetGroupChatDesctiption = Description "Gets the messages from the supplied group chat starting from/ending with the given message id."
type MessagesGetUserChatDesctiption = Description "Gets the messages from the supplied user chat (two people) starting from/ending with the given message id."
type MessagesSend = Description "Sends the given message.\n\nReturns the id of the sent message."

type MessagesApi =
    "chats" :> (
        "getAll" :> GetAllChatsDescription :> ReqBody '[JSON] (PaginatingRequest MessageId (Maybe ())) :> Post '[JSON] (ResponseWithUsers [Dialog]) :<|>
        "getGroupChat" :> GetGroupChatDescription :> ReqBody '[JSON] (GroupChatId) :> Post '[JSON] GroupChat :<|>
        "updateGroupChat" :> GetUpdateGroupChatDescription :> ReqBody '[JSON] GroupChat :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent :<|>
        "createGroupChat" :> CreateGroupChatDescription :> ReqBody '[JSON] GroupChatCreation :> Post '[JSON] GroupChatId :<|>
        "leaveGroupChat" :> LeaveGroupChatDescription :> ReqBody '[JSON] GroupChatId :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent) :<|>
    "messages" :> "get" :> (
        "groupChat" :> MessagesGetGroupChatDesctiption :> ReqBody '[JSON] (PaginatingRequest MessageId GroupChatId) :> Post '[JSON] [Message] :<|>
        "userChat" :> MessagesGetUserChatDesctiption :> ReqBody '[JSON] (PaginatingRequest MessageId UserId) :> Post '[JSON] [Message]) :<|>
        "send" :> MessagesSend :> ReqBody '[JSON] MessageCreation :> Post '[JSON] MessageId


messagesServer :: UserId -> ServerT MessagesApi App
messagesServer uId =
    (
        getChats uId :<|>
        getGroupChatInfo uId :<|>
        updateGroupChatApi uId :<|>
        createGroupChatApi uId :<|>
        leaveChatAPI uId) :<|>
    (getGroupChat uId :<|> getUserChat uId) :<|>
    sendMessageApi uId


getChats :: UserId -> PaginatingRequest MessageId (Maybe ()) -> App (ResponseWithUsers [Dialog])
getChats uId (PaginatingRequest mId lim _ dir) = runQerror $ getDialogs uId mId lim dir

getGroupChat :: UserId -> PaginatingRequest MessageId GroupChatId -> App [Message]
getGroupChat uId (PaginatingRequest mId lim gId dir) =
    runQerror $ getMessagesInGroupChat uId gId mId lim dir

getUserChat :: UserId -> PaginatingRequest MessageId UserId -> App [Message]
getUserChat uId (PaginatingRequest mId lim dId dir) =
    runQerror $ getMessagesInUserChat uId dId mId lim dir

getGroupChatInfo :: UserId -> GroupChatId -> App GroupChat
getGroupChatInfo uId gId = runQerror $ getGroupChatForUser uId gId

updateGroupChatApi :: UserId -> GroupChat -> App NoContent
updateGroupChatApi uId group = runQerror (updateGroupChat uId group) >> return NoContent

sendMessageApi :: UserId -> MessageCreation -> App MessageId
sendMessageApi uId mes = runQerror $ sendMessage uId mes

createGroupChatApi :: UserId -> GroupChatCreation -> App GroupChatId
createGroupChatApi uId group = runQerror $ createGroupChat uId group

leaveChatAPI :: UserId -> GroupChatId -> App NoContent
leaveChatAPI uId gIg = runQerror (leaveGroupChat uId gIg) >> return NoContent