{-# LANGUAGE
    FlexibleInstances,
    DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables,
    RecordWildCards #-}
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
import Data.Maybe (isJust)
import Control.Monad.Except

-- MARK: Documentation

instance HasDocs api => HasDocs (Authentication :> api) where
        docsFor Proxy (endpoint, action) =
            docsFor (Proxy :: Proxy api) (endpoint, action & notes <>~ [DocNote "Authentication" ["This method requires cookies set in the `POST /authenticate` method.\n\nReturns error `498 Invalid Token` if the token is invalid or the token header is missing. Returns `401 Unathorized` if the token doesn't permit access to the requested data."]])

type UserSearchDescription = Description "Searches for users with the given string.\n\nFeed in the raw user input string."

type UpdateUserDescription = Description "Updates the current user.\n\nThrow 401 if you try to change email or something like that."

type DeactivateAllDescription = Description "Deactivates all tokens for the logged in user including the one used to authenticate this request."

-- MARK: Implementation

type Authentication = AuthProtect "basicAuth"

type API =
        LoggerAPI :<|>
        "authentication" :> AuthenticationHandler :<|>
        Authentication :> ComplexQuery :> (
            "authentication" :> "me" :> Post '[JSON] UserId :<|>
            "deactivateAll" :> DeactivateAllDescription
                :> DeleteNoContent '[JSON, PlainText, FormUrlEncoded] NoContent :<|>
            "users" :> ReqBody '[JSON] [UserId] :> Post '[JSON] [User Faculty] :<|>
            "users" :> "all" :> Post '[JSON] [User Faculty] :<|>
            "users" :> "search" :> UserSearchDescription :> ReqBody '[JSON, PlainText] Text :> Post '[JSON] [User Faculty] :<|>
            "users" :> "update" :> UpdateUserDescription :> ReqBody '[JSON] UserUpdate :> PostNoContent '[JSON, PlainText] NoContent :<|>
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
        deactivateApi uId :<|>
        maybeNotFound . runQnotFound . getUsers :<|>
        runQerror getAllUsers :<|>
        runQerror . searchUsers :<|>
        updateUserApi uId :<|>
        postApi uId :<|>
        channelsApi uId :<|>
        messagesServer uId
        ) :<|>
        tagsServer :<|>
        facultyServer

updateUserApi :: UserId -> UserUpdate -> App NoContent
updateUserApi uId UserUpdate{..} = do
    res <- runQerror $ updateUser UserUpdateRow {
            userUpdateRowId = uId,
            userUpdateRowFirstName = userUpdateFirstName,
            userUpdateRowMiddleName = userUpdateMiddleName,
            userUpdateRowLastName = userUpdateLastName,
            userUpdateRowUserFaculty = userUpdateUserFaculty
        }
    if isJust res
        then return NoContent
        else throwError err401

deactivateApi :: UserId -> App NoContent
deactivateApi uId = runQerror (killAllTokens uId) >> return NoContent