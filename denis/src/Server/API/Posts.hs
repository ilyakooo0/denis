{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Server.API.Posts (
    PostApi,
    postApi
) where

import Servant.API
import qualified Data.Post as P
import Server.App
import Servant.Server
import Data.Query
import Data.User
import Data.Connection
import Server.Query.Pagination

-- MARK: Implementation

type PostApi = ReqBody '[JSON] [P.PostId] :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "last" :> ReqBody '[JSON] (PaginatingRequest P.PostId (Maybe ())) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "forUser" :> ReqBody '[JSON] (PaginatingRequest P.PostId UserId) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "publish" :> ReqBody '[JSON] P.PostCreation :> Post '[JSON] P.PostId

postApi :: UserId -> ServerT PostApi App
postApi uId = getPostsApi :<|> lastPosts :<|> getPostsForUserApi :<|> publishPostApi uId

getPostsApi :: [P.PostId] -> App (ResponseWithUsers [P.Post])
getPostsApi = maybeNotFound . runQnotFound . getPosts

lastPosts :: PaginatingRequest P.PostId (Maybe ()) -> App (ResponseWithUsers [P.Post])
lastPosts (PaginatingRequest pId lim _) = maybeNotFound . runQnotFound $ getLastPosts pId lim

getPostsForUserApi :: PaginatingRequest P.PostId UserId -> App (ResponseWithUsers [P.Post])
getPostsForUserApi (PaginatingRequest pId lim uId) = maybeNotFound . runQnotFound $ getPostsForUser pId lim uId

publishPostApi :: UserId -> P.PostCreation -> App P.PostId
publishPostApi uId = runQerror . publishPost uId