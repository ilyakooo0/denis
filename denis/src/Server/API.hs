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
        mkServerAPI
) where 
    
import Data.Proxy
import Servant.API
import Data.User
import Server.Auth
import Data.Int ()
import Servant.Server
import Data.Connection
import Data.Query
import Server.App
import Server.API.Posts
import Server.Logger
import Servant.Docs (HasDocs, docsFor, notes, DocNote(DocNote))
import Control.Lens

-- MARK: Documentation

instance HasDocs api => HasDocs (Authentication :> api) where
        docsFor Proxy (endpoint, action) =
            docsFor (Proxy :: Proxy api) (endpoint, action & notes <>~ [DocNote "Authentication" ["This method requires cookies set in the `POST /authenticate` method.\n\nReturns error `401 Unauthorized` if the cookies are invalid or don't premit access to the requested data."]])

-- MARK: Implementation

type Authentication = AuthProtect "basicAuth"

type API = 
        LoggerAPI :<|>
        "authenticate" :> AuthenticationHandler :<|>
            Authentication :> (
            "users" :> ReqBody '[JSON] [UserId] :> Post '[JSON] [User] :<|>
            "posts" :> PostApi
            )

serverProxy :: Proxy API
serverProxy = Proxy

mkServerAPI :: Logger -> ServerT API App
mkServerAPI l = 
        l :<|> 
        authenticate :<|> (\uId -> 
        mapM (runQnotFound . getUser) :<|>
        postApi uId
        )

-- getPostsS :: [UserId] -> App [User]
-- getPostsS = mapM getPost

