{-# LANGUAGE DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverloadedStrings,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

module Server.Auth (
    checkToken,
    cookieTokenKey,
    genAuthServerContext,
    AuthenticationHandler,
    authenticate,
    contextProxy,
    AuthenticationCredits(..)
) where

import Server.App
import Data.User
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Query
import Data.Binary (decode)
import Server.Error
import Control.Monad.Error
import Text.Read (readMaybe)
import Squeal.PostgreSQL.PQ
import Data.Connection
import Servant.Server.Experimental.Auth
import Servant.API
import Network.Wai (Request, requestHeaders)
import Servant.Server
import Web.Cookie
import Data.Time.Clock
import Data.Monoid ((<>))
import Data.Text
import Data.Proxy
import qualified GHC.Generics as GHC
import Data.Aeson
import Data.Binary.Builder (toLazyByteString)

type AuthenticationHandler = ReqBody '[JSON] AuthenticationCredits :> Post '[JSON] (Headers '[Header "Set-Cookie" String] Int)

data AuthenticationCredits = AuthenticationCredits {authenticationId :: UserId}
    deriving GHC.Generic

instance ToJSON AuthenticationCredits
instance FromJSON AuthenticationCredits

authenticate :: ServerT AuthenticationHandler App
authenticate (AuthenticationCredits uId) = do
    conn <- ask
    expire <- liftIO $ fmap (addUTCTime (1 * 24 * 60 * 60)) getCurrentTime -- one days
    tId <- fmap userId . runQ invalidToken . getUser $ uId
    let cookie = defaultSetCookie { 
        setCookieName = cookieTokenKey, 
        setCookieValue = C.pack $ show tId, 
        setCookieExpires = Just expire, 
        -- setCookieDomain = Just "127.0.0.1", 
        setCookiePath = Just "/" }
    -- let token = cookieTokenKeyString <> "=" <> show tId <> "; Expires=" <> show expire <> "; Domain=127.0.0.1:2000; Path=/" -- ; Secure" --; HttpOnly"
    return $ addHeader (L.unpack . toLazyByteString . renderSetCookie $ cookie) 0 


checkToken :: DBConnection -> ByteString -> Handler UserId
checkToken conn token = do
    uId <- case ((readMaybe . C.unpack) token :: Maybe UserId) of
        Just uId -> return uId
        Nothing -> throwError invalidToken
    fmap userId . runQ' conn invalidToken . getUser $ uId

authHandler :: DBConnection -> AuthHandler Request UserId
authHandler conn = mkAuthHandler handler
    where
        maybeToEither e = maybe (Left e) Right
        throw401 msg = throwError $ err401 { errBody = msg }
        handler req = either throw401 (checkToken conn) $ do
            cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
            maybeToEither "Missing token in cookie" $ lookup cookieTokenKey $ parseCookies cookie

cookieTokenKey :: ByteString
cookieTokenKey = "servant-auth-cookie"

cookieTokenKeyString = C.unpack cookieTokenKey

type instance AuthServerData (AuthProtect "basicAuth") = UserId

genAuthServerContext :: DBConnection -> Context (AuthHandler Request UserId ': '[])
genAuthServerContext conn = authHandler conn :. EmptyContext

contextProxy :: Proxy '[AuthHandler Request UserId]
contextProxy = Proxy