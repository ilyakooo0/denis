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

type PostApi = ReqBody '[JSON] [P.PostId] :> Post '[JSON] PostResponse :<|>
    "last" :> ReqBody '[JSON] Word64 :> Post '[JSON] PostResponse :<|>
    "forUser" :> ReqBody '[JSON] UserPostsRequest :> Post '[JSON] PostResponse :<|>
    "publish" :> ReqBody '[JSON] P.PostCreation :> Post '[JSON] P.PostId    
    
postApi :: UserId -> ServerT PostApi App
postApi uId = getPostsApi :<|> lastPosts :<|> getPostsForUserApi :<|> publishPostApi uId

getPostsApi :: [P.PostId] -> App PostResponse
getPostsApi = maybeNotFound . runQnotFound . getPosts 

lastPosts :: Word64 -> App PostResponse
lastPosts = maybeNotFound . runQnotFound . getLastPosts

getPostsForUserApi :: UserPostsRequest -> App PostResponse
getPostsForUserApi (LimitingRequest uId lim) = maybeNotFound . runQnotFound $ getPostsForUser lim uId

publishPostApi :: UserId -> P.PostCreation -> App P.PostId
publishPostApi uId = runQerror . publishPost uId