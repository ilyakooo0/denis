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
import Server.LimitingRequest
import Data.Channel.NamedChannel
import Data.Channel.AnonymousChannel
import qualified Data.Post as P

-- MARK: Implementation

type NamedChannelRequest = LimitingRequest NamedChannelId

type ChannelsApi = Post '[JSON] [NamedChannel UserId] :<|>
    "get" :> ReqBody '[JSON] NamedChannelRequest :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "getAnonymous" :> ReqBody '[JSON] (LimitingRequest AnonymousChannel) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "create" :> ReqBody '[JSON] NamedChannelCreationRequest :> Post '[JSON] NamedChannelId :<|>
    "update" :> ReqBody '[JSON] (NamedChannel UserId) :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent :<|>
    "delete" :> ReqBody '[JSON] NamedChannelId :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent

channelsApi :: UserId -> ServerT ChannelsApi App
channelsApi uId = getChannelsApi uId :<|> getChannel uId :<|> getAnonymousChannelsApi :<|> createChannelApi uId :<|> updateChannelApi uId :<|> deleteChannelApi uId


getChannelsApi :: UserId -> App [NamedChannel UserId]
getChannelsApi uId = runQnotFound $ getAllChannels uId

getAnonymousChannelsApi :: LimitingRequest AnonymousChannel -> App (ResponseWithUsers [P.Post])
getAnonymousChannelsApi (LimitingRequest req lim) = runQnotFound $ getAnonymousChannelPosts lim req

getChannel :: UserId -> NamedChannelRequest -> App (ResponseWithUsers [P.Post])
getChannel uId (LimitingRequest cId lim) = runQnotFound $ getChannelPosts uId lim cId

createChannelApi :: UserId -> NamedChannelCreationRequest -> App NamedChannelId
createChannelApi uId req = maybeNotFound . runQnotFound $ createNewChannel uId req

updateChannelApi :: UserId -> NamedChannel UserId -> App NoContent
updateChannelApi uId req = do
    runQnotFound $ updateChannel uId req
    return NoContent

deleteChannelApi :: UserId -> NamedChannelId -> App NoContent
deleteChannelApi uId cId = do
    runQnotFound $ deleteNamedChannel uId cId
    return NoContent