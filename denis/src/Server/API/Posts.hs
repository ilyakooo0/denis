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
    UserPostsRequest(..)
) where

import Servant.API
import qualified Data.Post as P
import Data.Aeson 
import Server.App
import Servant.Server
import Data.Query
import Data.User
import Data.Connection
import Servant
import Data.Word (Word64)
import Servant.Docs (ToSample, toSamples, samples)
    
-- MARK: Documentation

instance ToSample UserPostsRequest where
    toSamples _ = samples [UserPostsRequest 8 10, UserPostsRequest 42 20]
    
-- MARK: Implementation

data UserPostsRequest = UserPostsRequest {
    userPostRequestId :: UserId,
    userPostRequestLimit :: Word64
}

instance ToJSON UserPostsRequest where
    toJSON (UserPostsRequest uId lim) = object ["userId" .= uId, "limit" .= lim]
instance FromJSON UserPostsRequest where
    parseJSON = withObject "User posts request" $ \e -> 
        UserPostsRequest <$> e .: "userId" <*> e.: "limit"

type PostApi = ReqBody '[JSON] [P.PostId] :> Post '[JSON] [P.Post] :<|>
    "last" :> ReqBody '[JSON] Word64 :> Post '[JSON] [P.Post] :<|>
    "forUser" :> ReqBody '[JSON] UserPostsRequest :> Post '[JSON] [P.Post] :<|>
    "publish" :> ReqBody '[JSON] [P.PostElement P.Post] :> Post '[JSON] P.PostId

maybeNotFound :: App (Maybe a) -> App a
maybeNotFound = (>>= (\t -> case t of
    Just y -> return y
    Nothing -> throwError err404))
    
    
postApi :: UserId -> ServerT PostApi App
postApi uId = getPosts :<|> lastPosts :<|> getPostsForUserApi :<|> publishPostApi uId

getPosts :: [P.PostId] -> App [P.Post]
getPosts = mapM (maybeNotFound . runQnotFound . getPost) 

lastPosts :: Word64 -> App [P.Post]
lastPosts = maybeNotFound . runQnotFound . getLastPosts

getPostsForUserApi :: UserPostsRequest -> App [P.Post]
getPostsForUserApi (UserPostsRequest uId lim) = maybeNotFound . runQnotFound $ getPostsForUser lim uId

publishPostApi :: UserId -> [P.PostElement P.Post] -> App P.PostId
publishPostApi uId = runQerror . publishPost uId