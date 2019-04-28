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

-- MARK: Implementation

type ChannelsApi = Post '[JSON] [NamedChannel UserId] :<|>
    "get" :> ReqBody '[JSON] (PaginatingRequest P.PostId NamedChannelId) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "getAnonymous" :> ReqBody '[JSON] (PaginatingRequest P.PostId AnonymousChannel) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "create" :> ReqBody '[JSON] NamedChannelCreationRequest :> Post '[JSON] NamedChannelId :<|>
    "update" :> ReqBody '[JSON] (NamedChannel UserId) :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent :<|>
    "delete" :> ReqBody '[JSON] NamedChannelId :> PostNoContent '[JSON, FormUrlEncoded, PlainText] NoContent

channelsApi :: UserId -> ServerT ChannelsApi App
channelsApi uId = getChannelsApi uId :<|> getChannel uId :<|> getAnonymousChannelsApi :<|> createChannelApi uId :<|> updateChannelApi uId :<|> deleteChannelApi uId


getChannelsApi :: UserId -> App [NamedChannel UserId]
getChannelsApi uId = runQnotFound $ getAllChannels uId

getAnonymousChannelsApi :: PaginatingRequest P.PostId AnonymousChannel -> App (ResponseWithUsers [P.Post])
getAnonymousChannelsApi (PaginatingRequest pId lim req) = runQnotFound $ getAnonymousChannelPosts pId lim req

getChannel :: UserId -> PaginatingRequest P.PostId NamedChannelId -> App (ResponseWithUsers [P.Post])
getChannel uId (PaginatingRequest pId lim cId) = runQnotFound $ getChannelPosts uId pId lim cId

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