{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

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
import Data.Int
import Server.Error

-- MARK: Implementation

type ChannelsApi = Post '[JSON] [NamedChannel UserId] :<|>
    "get" :> ReqBody '[JSON] (PaginatingRequest P.PostId (Maybe NamedChannelId)) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "getAnonymous" :> ReqBody '[JSON] (PaginatingRequest P.PostId (Maybe AnonymousChannel)) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "create" :> ReqBody '[JSON] NamedChannelCreationRequest :> Post '[JSON] NamedChannelId :<|>
    "update" :> ReqBody '[JSON] (NamedChannel UserId) :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent :<|>
    "delete" :> ReqBody '[JSON] NamedChannelId :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent

channelsApi :: UserId -> ServerT ChannelsApi App
channelsApi uId = getChannelsApi uId :<|> getChannel uId :<|> getAnonymousChannelsApi :<|> createChannelApi uId :<|> updateChannelApi uId :<|> deleteChannelApi uId


getChannelsApi :: UserId -> App [NamedChannel UserId]
getChannelsApi uId = runQnotFound $ getAllChannels uId

getAnonymousChannelsApi :: PaginatingRequest P.PostId (Maybe AnonymousChannel) -> App (ResponseWithUsers [P.Post])
getAnonymousChannelsApi (PaginatingRequest pId lim (Just req) dir) = runQnotFound $ getAnonymousChannelPosts pId lim dir req
getAnonymousChannelsApi (PaginatingRequest pId lim Nothing dir) = maybeNotFound . runQnotFound $ getLastPosts pId lim dir

getChannel :: UserId -> PaginatingRequest P.PostId (Maybe NamedChannelId) -> App (ResponseWithUsers [P.Post])
getChannel uId (PaginatingRequest pId lim (Just cId) dir) = runQnotFound $ getChannelPosts uId pId lim dir cId
getChannel _ (PaginatingRequest pId lim Nothing dir) = maybeNotFound . runQnotFound $ getLastPosts pId lim dir

createChannelApi :: UserId -> NamedChannelCreationRequest -> App NamedChannelId
createChannelApi uId req = do
    cCount <- runQerror $ channelCountForUser uId
    if cCount >= channelCountLimit
        then throwError $ limitExceeded "You have exceeded the number of channels you can create."
        else maybeNotFound . runQerror $ createNewChannel uId req

updateChannelApi :: UserId -> NamedChannel UserId -> App NoContent
updateChannelApi uId req = do
    runQnotFound $ updateChannel uId req
    return NoContent

deleteChannelApi :: UserId -> NamedChannelId -> App NoContent
deleteChannelApi uId cId = do
    runQnotFound $ deleteNamedChannel uId cId
    return NoContent

-- TODO: Increase
channelCountLimit :: Int64
channelCountLimit = 5