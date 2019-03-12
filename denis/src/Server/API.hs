{-# LANGUAGE 
    FlexibleInstances,
    DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server.API (
        API,
        serverProxy,
        mkServerAPI,
        Authentication
) where 
    
import Data.Proxy
import Servant.API
import Data.User
import Server.Auth
import Servant.Server
import Data.Connection
import Data.Query
import Server.App
import Server.API.Posts
import Server.Logger
import Servant.Docs (HasDocs, docsFor, notes, DocNote(DocNote))
import Server.API.Channels
import Control.Lens


-- MARK: Documentation

instance HasDocs api => HasDocs (Authentication :> api) where
        docsFor Proxy (endpoint, action) =
            docsFor (Proxy :: Proxy api) (endpoint, action & notes <>~ [DocNote "Authentication" ["This method requires cookies set in the `POST /authenticate` method.\n\nReturns error `498 Invalid Token` if the token is invalid or the token header is missing. Returns `401 Unathorized` if the token doesn't premit access to the requested data."]])


-- MARK: Implementation

type Authentication = AuthProtect "basicAuth"

type API = 
        LoggerAPI :<|>
        "authentication" :> AuthenticationHandler :<|>
        Authentication :> (
            "authentication" :> "me" :> Post '[JSON] UserId :<|>
            "users" :> ReqBody '[JSON] [UserId] :> Post '[JSON] [User] :<|>
            "posts" :> PostApi :<|>
            "channels" :> ChannelsApi
            )

serverProxy :: Proxy API
serverProxy = Proxy

mkServerAPI :: Logger -> ServerT API App
mkServerAPI l = 
        l :<|> 
        authenticationAPI :<|> (\uId -> 
        return uId :<|>
        maybeNotFound . runQnotFound . getUsers :<|>
        postApi uId :<|>
        channelsApi uId
        )