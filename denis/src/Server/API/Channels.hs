{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    RecordWildCards #-}

module Server.API.Channels (
    ChannelsApi,
    channelsApi
) where

import Servant.API
import Server.App
import Servant.Server
import Data.Query
import Data.User
import Data.Connection
import Data.Channel.NamedChannel
import Data.Channel.AnonymousChannel
import qualified Data.Post as P
import Server.Query.Pagination
import Server.Error
import Data.Text (Text)
import Data.Text.Validator
import Control.Monad
import Data.Limits
import qualified Data.Text as T

type CreateDescription = Description "Creates a new channel\n\nReturns 406 if the channel limit has been exceeded."

type SearchDescription = Description "Search for user channels."

-- MARK: Implementation

type ChannelsApi = Post '[JSON] [NamedChannel UserId] :<|>
    "get" :> ReqBody '[JSON] (PaginatingRequest P.PostId (Maybe NamedChannelId)) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "getAnonymous" :> ReqBody '[JSON] (PaginatingRequest P.PostId (Maybe AnonymousChannel)) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "create" :> CreateDescription :> ReqBody '[JSON] NamedChannelCreationRequest :> Post '[JSON] NamedChannelId :<|>
    "update" :> ReqBody '[JSON] (NamedChannel UserId) :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent :<|>
    "delete" :> ReqBody '[JSON] NamedChannelId :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent :<|>
    "search" :> SearchDescription :> ReqBody '[JSON, PlainText] Text :> Post '[JSON] [NamedChannel UserId]

channelsApi :: UserId -> ServerT ChannelsApi App
channelsApi uId = getChannelsApi uId :<|> getChannel uId :<|> getAnonymousChannelsApi :<|> createChannelApi uId :<|> updateChannelApi uId :<|> deleteChannelApi uId :<|> searchChannelsApi uId


getChannelsApi :: UserId -> App [NamedChannel UserId]
getChannelsApi uId = runQnotFound $ getAllChannels uId

getAnonymousChannelsApi :: PaginatingRequest P.PostId (Maybe AnonymousChannel) -> App (ResponseWithUsers [P.Post])
getAnonymousChannelsApi req'@(PaginatingRequest pId lim (Just req) dir) = do
    unless (validateText req && validatePaginationRequest req') $ throwError lengthExceeded
    runQnotFound $ getAnonymousChannelPosts pId lim dir req
getAnonymousChannelsApi req@(PaginatingRequest pId lim Nothing dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    maybeNotFound . runQnotFound $ getLastPosts pId lim dir

getChannel :: UserId -> PaginatingRequest P.PostId (Maybe NamedChannelId) -> App (ResponseWithUsers [P.Post])
getChannel uId req@(PaginatingRequest pId lim (Just cId) dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    runQnotFound $ getChannelPosts uId pId lim dir cId
getChannel _ req@(PaginatingRequest pId lim Nothing dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    maybeNotFound . runQnotFound $ getLastPosts pId lim dir

createChannelApi :: UserId -> NamedChannelCreationRequest -> App NamedChannelId
createChannelApi uId req = do
    cCount <- runQerror $ channelCountForUser uId
    unless (validateText req) $ throwError lengthExceeded
    if cCount >= channelCountLimit
        then throwError $ amountExceeded
        else maybeNotFound . runQerror $ createNewChannel uId req

updateChannelApi :: UserId -> NamedChannel UserId -> App NoContent
updateChannelApi uId req@NamedChannel{..} = do
    unless (validateText req) $ throwError lengthExceeded
    runQnotFound $ updateChannel uId req
    return NoContent

deleteChannelApi :: UserId -> NamedChannelId -> App NoContent
deleteChannelApi uId cId = do
    runQnotFound $ deleteNamedChannel uId cId
    return NoContent

searchChannelsApi :: UserId -> Text -> App [NamedChannel UserId]
searchChannelsApi uId query = do
    unless (validateText query) $ throwError lengthExceeded
    if (T.null . T.strip) query
        then return []
        else runQerror . searchChannels uId $ query
