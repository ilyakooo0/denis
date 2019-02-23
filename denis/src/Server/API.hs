{-# LANGUAGE DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables #-}

module Server.API (
        API,
        serverProxy,
        serverAPI
) where 
    
import Data.Proxy
import Servant.API
-- import Servant
import Servant.Server.Experimental.Auth
import Data.User
import qualified Data.Post as P
import Server.Auth
import Data.ByteString
import Data.Int (Int64)
import Servant.Server
import Data.Connection
import Data.Query
import Server.Auth
import Server.App
import Control.Monad.Error
import Server.API.Draft
import Server.API.Posts

maybeNotFound :: App (Maybe a) -> App a
maybeNotFound = (>>= (\t -> case t of
    Just y -> return y
    Nothing -> throwError err404))

type API = "authenticate" :> AuthenticationHandler 
        :<|>
            AuthProtect "basicAuth" :> (
            "users" :> ReqBody '[JSON] [UserId] :> Post '[JSON] [User] :<|>
            "posts" :> PostApi :<|>
            "drafts" :> DraftApi
            )

serverProxy :: Proxy API
serverProxy = Proxy

serverAPI :: ServerT API App
serverAPI = authenticate :<|> (\uId -> 
        mapM (runQnotFound . getUser) :<|>
        postApi uId :<|>
        draftApi uId
        )

-- getPostsS :: [UserId] -> App [User]
-- getPostsS = mapM getPost

