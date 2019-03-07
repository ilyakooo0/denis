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
    logIn,
    contextProxy,
    AuthenticationCredits(..),
    authenticationAPI
) where

import Server.App
import Data.User
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Query
import Server.Error
import Control.Monad.Except
import Text.Read (readMaybe)
import Data.Connection
import Servant.Server.Experimental.Auth
import Servant.API
import Network.Wai (Request, requestHeaders)
import Servant.Server
import Web.Cookie
import Data.Time.Clock
import Data.Time.Calendar
import Data.Proxy
import qualified GHC.Generics as GHC
import Data.Aeson
import Data.Binary.Builder (toLazyByteString)
import Servant.Docs (ToSample, samples, toSamples)

-- MARK: Documentation

instance ToSample AuthenticationCredits where
    toSamples _ = samples $ map AuthenticationCredits [56, 103, 8]
            
-- MARK: Implementation

type AuthenticationHandler = 
    "login" :> 
        ReqBody '[JSON] AuthenticationCredits :> 
        Post '[JSON] (Headers '[Header "Set-Cookie" String] UserId) :<|>
    "logout" :> 
        PostNoContent '[JSON, PlainText, FormUrlEncoded] (Headers '[Header "Set-Cookie" String] NoContent)

newtype AuthenticationCredits = AuthenticationCredits {authenticationId :: UserId}
    deriving GHC.Generic

instance ToJSON AuthenticationCredits
instance FromJSON AuthenticationCredits

authenticationAPI :: ServerT AuthenticationHandler App
authenticationAPI = logIn :<|> logOut

logIn :: AuthenticationCredits -> App (Headers '[Header "Set-Cookie" String] UserId)
logIn (AuthenticationCredits uId) = do
    expire <- liftIO $ fmap (addUTCTime (1 * 24 * 60 * 60)) getCurrentTime -- one days
    tId <- fmap userId . runQ err404 . getUser $ uId
    let cookie = defaultSetCookie { 
        setCookieName = cookieTokenKey, 
        setCookieValue = C.pack $ show tId, 
        setCookieExpires = Just expire, 
        setCookiePath = Just "/" }
    return $ addHeader (L.unpack . toLazyByteString . renderSetCookie $ cookie) tId 

logOut :: App (Headers '[Header "Set-Cookie" String] NoContent)
logOut =   
    let cookie = defaultSetCookie { 
        setCookieName = cookieTokenKey, 
        setCookieValue = "invalid", 
        setCookieExpires = Just (UTCTime (ModifiedJulianDay 0) 0), 
        setCookiePath = Just "/" }
    in return $ addHeader (L.unpack . toLazyByteString . renderSetCookie $ cookie) NoContent 

checkToken :: DBConnection -> BS.ByteString -> Handler UserId
checkToken conn token = do
    uId <- case ((readMaybe . C.unpack) token :: Maybe UserId) of
        Just uId -> return uId
        Nothing -> throwError invalidToken
    fmap userId . runQ' conn invalidToken . getUser $ uId

authHandler :: DBConnection -> AuthHandler Request UserId
authHandler conn = mkAuthHandler handler
    where
        maybeToEither = maybe (Left ()) Right
        throw = const $ throwError invalidToken
        handler req = either throw (checkToken conn) $ do
            cookie <- maybeToEither $ lookup "cookie" $ requestHeaders req
            maybeToEither $ lookup cookieTokenKey $ parseCookies cookie

cookieTokenKey :: BS.ByteString
cookieTokenKey = "servant-auth-cookie"

type instance AuthServerData (AuthProtect "basicAuth") = UserId

genAuthServerContext :: DBConnection -> Context (AuthHandler Request UserId ': '[])
genAuthServerContext conn = authHandler conn :. EmptyContext

contextProxy :: Proxy '[AuthHandler Request UserId]
contextProxy = Proxy