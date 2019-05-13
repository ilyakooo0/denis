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
import Data.Limits
import Server.Error
import Control.Monad
import Data.Text.Validator

-- MARK: Implementation

type PostApi = ReqBody '[JSON] [P.PostId] :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "last" :> ReqBody '[JSON] (PaginatingRequest P.PostId (Maybe ())) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "forUser" :> ReqBody '[JSON] (PaginatingRequest P.PostId UserId) :> Post '[JSON] (ResponseWithUsers [P.Post]) :<|>
    "publish" :> ReqBody '[JSON] P.PostCreation :> Post '[JSON] P.PostId

postApi :: UserId -> ServerT PostApi App
postApi uId = getPostsApi :<|> lastPosts :<|> getPostsForUserApi :<|> publishPostApi uId

getPostsApi :: [P.PostId] -> App (ResponseWithUsers [P.Post])
getPostsApi req = do
    unless (listLimit < length req) $ throwError lengthExceeded
    maybeNotFound . runQnotFound . getPosts $ req

lastPosts :: PaginatingRequest P.PostId (Maybe ()) -> App (ResponseWithUsers [P.Post])
lastPosts req@(PaginatingRequest pId lim _ dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    maybeNotFound . runQnotFound $ getLastPosts pId lim dir

getPostsForUserApi :: PaginatingRequest P.PostId UserId -> App (ResponseWithUsers [P.Post])
getPostsForUserApi req@(PaginatingRequest pId lim uId dir) = do
    unless (validatePaginationRequest req) $ throwError lengthExceeded
    maybeNotFound . runQnotFound $ getPostsForUser pId lim dir uId

publishPostApi :: UserId -> P.PostCreation -> App P.PostId
publishPostApi uId creation = do
    unless (validateText creation) $ throwError lengthExceeded
    runQerror . publishPost uId $ creation