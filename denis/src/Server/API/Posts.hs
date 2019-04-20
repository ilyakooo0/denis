{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Server.API.Posts (
    PostApi,
    postApi,
    UserPostsRequest
) where

import Servant.API
import qualified Data.Post as P
import Server.App
import Servant.Server
import Data.Query
import Data.User
import Data.Connection
import Data.Word (Word64)
import Server.LimitingRequest

-- MARK: Implementation

type UserPostsRequest = LimitingRequest UserId

type PostApi = ReqBody '[JSON] [P.PostId] :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "last" :> ReqBody '[JSON] Word64 :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "forUser" :> ReqBody '[JSON] UserPostsRequest :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "publish" :> ReqBody '[JSON] P.PostCreation :> Post '[JSON] P.PostId

postApi :: UserId -> ServerT PostApi App
postApi uId = getPostsApi :<|> lastPosts :<|> getPostsForUserApi :<|> publishPostApi uId

getPostsApi :: [P.PostId] -> App (ResponseWithUsers [P.Post])
getPostsApi = maybeNotFound . runQnotFound . getPosts

lastPosts :: Word64 -> App (ResponseWithUsers [P.Post])
lastPosts = maybeNotFound . runQnotFound . getLastPosts

getPostsForUserApi :: UserPostsRequest -> App (ResponseWithUsers [P.Post])
getPostsForUserApi (LimitingRequest uId lim) = maybeNotFound . runQnotFound $ getPostsForUser lim uId

publishPostApi :: UserId -> P.PostCreation -> App P.PostId
publishPostApi uId = runQerror . publishPost uId