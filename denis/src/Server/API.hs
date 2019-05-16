{-# LANGUAGE
    FlexibleInstances,
    DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables,
    RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server.API where

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
import Server.Error
import Data.Text.Validator

-- MARK: Documentation

instance HasDocs api => HasDocs (Authentication :> api) where
        docsFor Proxy (endpoint, action) =
            docsFor (Proxy :: Proxy api) (endpoint, action & notes <>~ [DocNote "Authentication" ["Этот метод требует токен, который можно получить через запрос `POST /authenticate/login`.\n\nВозвращает `498 Invalid Token` если токен не валидный или отсутствует. Возвращает `401 Unathorized` если токен не дает доступа к запрашиваемому ресурсу."]])

type UserSearchDescription = Description "Ищет пользователей по данной строке."

type UpdateUserDescription = Description "Обновляет профиль текущего пользователя."

type DeactivateAllDescription = Description "Деактивирует все токены для текущего пользователя."

type MeDescription = Description "Возвращает идентификатор текущего пользователя."

type UsersDescription = Description "Возвращает пользователей для данных идентификаторов."

type UsersAllDescription = Description "Возвращает всех пользователей."

-- MARK: Implementation

type Authentication = AuthProtect "basicAuth"

type API =
        LoggerAPI :<|>
        "authentication" :> AuthenticationHandler :<|>
        Authentication :> ComplexQuery :> (
            "authentication" :> "me" :> MeDescription :> Post '[JSON] UserId :<|>
            "deactivateAll" :> DeactivateAllDescription
                :> DeleteNoContent '[JSON, PlainText, FormUrlEncoded] NoContent :<|>
            "users" :> UsersDescription :> ReqBody '[JSON] [UserId] :> Post '[JSON] [User Faculty] :<|>
            "users" :> "all" :> UsersAllDescription :> Post '[JSON] [User Faculty] :<|>
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
    unless (validateText $ userUpdateUserFaculty ~< 100) $ throwError lengthExceeded
    facultyIsValid <- runQerror $ isValidFaculty userUpdateUserFaculty
    unless (facultyIsValid) $ throwError impossibleContent
    res <- runQ impossibleContent $ updateUser UserUpdateRow {
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