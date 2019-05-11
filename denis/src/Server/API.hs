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
import Server.Query.ComplexQuery
import Server.API.Completions
import Server.API.Messages
import Data.Faculty
import Server.API.Faculty
import Data.Text (Text)

-- MARK: Documentation

instance HasDocs api => HasDocs (Authentication :> api) where
        docsFor Proxy (endpoint, action) =
            docsFor (Proxy :: Proxy api) (endpoint, action & notes <>~ [DocNote "Authentication" ["This method requires cookies set in the `POST /authenticate` method.\n\nReturns error `498 Invalid Token` if the token is invalid or the token header is missing. Returns `401 Unathorized` if the token doesn't permit access to the requested data."]])

type UserSearchDescription = Description "Searches for users with the given string.\n\nFeed in the raw user input string."

-- MARK: Implementation

type Authentication = AuthProtect "basicAuth"

type API =
        LoggerAPI :<|>
        "authentication" :> AuthenticationHandler :<|>
        Authentication :> ComplexQuery :> (
            "authentication" :> "me" :> Post '[JSON] UserId :<|>
            "users" :> ReqBody '[JSON] [UserId] :> Post '[JSON] [User Faculty] :<|>
            "users" :> "all" :> Post '[JSON] [User Faculty] :<|>
            "users" :> "search" :> UserSearchDescription :> ReqBody '[JSON, PlainText] Text :> Post '[JSON] [User Faculty] :<|>
            "posts" :> PostApi :<|>
            "channels" :> ChannelsApi :<|>
            MessagesApi
            ) :<|>
        "tags" :> TagsAPI :<|>
        "faculty" :> FacultyAPI

serverProxy :: Proxy API
serverProxy = Proxy

mkServerAPI :: Logger -> ServerT API App
mkServerAPI l =
        l :<|>
        authenticationAPI :<|> (\uId ->
        return uId :<|>
        maybeNotFound . runQnotFound . getUsers :<|>
        runQerror getAllUsers :<|>
        runQerror . searchUsers :<|>
        postApi uId :<|>
        channelsApi uId :<|>
        messagesServer uId
        ) :<|>
        tagsServer :<|>
        facultyServer