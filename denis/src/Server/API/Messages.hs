{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    RecordWildCards #-}

module Server.API.Messages where

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
import Data.Text.Validator
import Control.Monad
import Server.Error
import qualified Data.Map as M
import Squeal.PostgreSQL.Schema
import Data.Limits
import Control.Monad.Except

type GetAllChatsDescription = Description "Возвращает чаты с данного идентификатора сообщения."
type GetGroupChatDescription = Description "Возвращает информацию о групповой беседе."
type GetUpdateGroupChatDescription = Description "Обновляет групповой чат.\n\nВозвращает 404 если пользователь не является участником беседы.\n\nВозвращает 401 Если у пользователя не достаточно прав на редактирование беседы."
type CreateGroupChatDescription = Description "Создает групповую беседу с данными правами.\n\nВызывающий пользовтаель автоматически приобретает максимальные права. Его не надо добавлять.\n\nВозвращает идентификатор созданного чата."
type LeaveGroupChatDescription = Description "Убирает пользователя из беседы."
type MessagesGetGroupChatDesctiption = Description "Возвращает сообщения в групповой бесед начиная с данного идентификатора сообщения."
type MessagesGetUserChatDesctiption = Description "Возвращает сообщения в диалоге начиная с данного идентификатора сообщения."
type MessagesSend = Description "Отправляет сообщения.\n\nВозвращает идентификато отправленного сообщения."

type MessagesApi =
    "chats" :> (
        "getAll" :> GetAllChatsDescription :> ReqBody '[JSON] (PaginatingRequest MessageId (Maybe ())) :> Post '[JSON] (ResponseWithUsers [Dialog]) :<|>
        "getGroupChat" :> GetGroupChatDescription :> ReqBody '[JSON] (GroupChatId) :> Post '[JSON] GroupChat :<|>
        "updateGroupChat" :> GetUpdateGroupChatDescription :> ReqBody '[JSON] GroupChat :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent :<|>
        "createGroupChat" :> CreateGroupChatDescription :> ReqBody '[JSON] GroupChatCreation :> Post '[JSON] GroupChatId :<|>
        "leaveGroupChat" :> LeaveGroupChatDescription :> ReqBody '[JSON] GroupChatId :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent) :<|>
    "messages" :> (
        "get" :> (
            "groupChat" :> MessagesGetGroupChatDesctiption :> ReqBody '[JSON] (PaginatingRequest MessageId GroupChatId) :> Post '[JSON] [Message] :<|>
            "userChat" :> MessagesGetUserChatDesctiption :> ReqBody '[JSON] (PaginatingRequest MessageId UserId) :> Post '[JSON] [Message]) :<|>
        "send" :> MessagesSend :> ReqBody '[JSON] MessageCreation :> Post '[JSON] MessageId)


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
getChats uId req@(PaginatingRequest mId lim _ dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    runQerror $ getDialogs uId mId lim dir

getGroupChat :: UserId -> PaginatingRequest MessageId GroupChatId -> App [Message]
getGroupChat uId req@(PaginatingRequest mId lim gId dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    runQerror $ getMessagesInGroupChat uId gId mId lim dir

getUserChat :: UserId -> PaginatingRequest MessageId UserId -> App [Message]
getUserChat uId req@(PaginatingRequest mId lim dId dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    runQerror $ getMessagesInUserChat uId dId mId lim dir

getGroupChatInfo :: UserId -> GroupChatId -> App GroupChat
getGroupChatInfo uId gId = do
    groupIsValid <- runQerror $ groupChatIsValidForUser uId gId
    unless (groupIsValid) $ throwError impossibleContent
    runQerror $ getGroupChatForUser uId gId

updateGroupChatApi :: UserId -> GroupChat -> App NoContent
updateGroupChatApi uId group = do
    unless (validateText group) $ throwError lengthExceeded
    groupIsValid <- runQerror $ groupChatIsValidForUser uId (groupChatId group)
    unless (groupIsValid) $ throwError impossibleContent
    runQerror (updateGroupChat uId group) >> return NoContent

sendMessageApi :: UserId -> MessageCreation -> App MessageId
sendMessageApi uId mes = do
    unless (validateText mes) $ throwError lengthExceeded
    runQerror $ sendMessage uId mes

createGroupChatApi :: UserId -> GroupChatCreation -> App GroupChatId
createGroupChatApi uId group@GroupChatCreation{..} = do
    unless (validateText group) $ throwError lengthExceeded
    unless ((M.size $ getJsonb groupChatCreationUsers) < groupChatSizeLimit) $ throwError amountExceeded
    runQerror $ createGroupChat uId group

leaveChatAPI :: UserId -> GroupChatId -> App NoContent
leaveChatAPI uId gId = do
    groupIsValid <- runQerror $ groupChatIsValidForUser uId gId
    unless (groupIsValid) $ throwError impossibleContent
    runQerror (leaveGroupChat uId gId) >> return NoContent