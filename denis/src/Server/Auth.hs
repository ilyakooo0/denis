{-# LANGUAGE DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverloadedStrings,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances,
    DeriveAnyClass,
    RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
import qualified Data.Aeson as A
import Data.Binary.Builder (toLazyByteString)
import Servant.Docs
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Text.Regex
import qualified Data.ByteString.Base64.URL as BS
import Data.Maybe
import Server.Auth.Token
import Server.Auth.Mail
import Web.UAParser
import qualified Data.Text.Lazy as TL
import Server.Auth.HTML

-- MARK: Documentation

instance ToSample AuthenticationCredits where
    toSamples _ = samples $ map AuthenticationCredits ["vchernyshev@hse.ru", "iikostyuchenko@hse.ru"]

instance ToSample UserLoginResponse where
    toSamples _ = samples $ map UserLoginResponse [UserIsRegistered, UserCanRegister, InvalidUser]

instance ToSample Int where
    toSamples _ = samples [382934, 103845, 294905]

instance ToSample UserCreation where
    toSamples _ = samples $ [UserCreation "Seva" "Algebrovich" "Leonidov" "cs.hse.ru/dse/"  "vchernyshev@hse.ru"]

instance ToSample TokenActivationData where
    toSamples _ = singleSample $ TokenActivationData 69 "ABCDE"

instance ToSample TokenActivationResult where
    toSamples _ = samples [TokenActivationResultYou, TokenActivationResultNotYou]

instance ToParam (QueryParam' mods "data" String) where
  toParam _ =
    DocQueryParam "data"
                  ["AAABBBC="]
                  "The data recieved from the email."
                  Normal

instance ToParam (QueryParam' mods "id" UserId) where
  toParam _ =
    DocQueryParam "id"
                  ["8"]
                  "The id of the user."
                  Normal

instance ToSample Markup where
    toSamples _ = noSamples

type LoginDescription = Description "Try to log in with the given email\n\nResponse fields:\n\n- `registered` -- User is registered and the cookie has been set. You now need to get a verification code from the user and pass it to `/verify`.\n- `canRegister` -- the email is valid, but the user needs to register. Prompt him for information and pass it to `/register`.\n- `invalid` -- the email is invalid."

type LogoutDescription = Description "Log out with the given cookie"

type VerifyDescription = Description "Verify token with the verification code from the user.\n\nReturns 400 if the token was not supplied.\n\nReturns 498 if the token was parsed but is invalid.\n\nReturns 403 if the token/code compination is invalid."

type RegisterDescription = Description "Register the user for a new account.\n\nReturns 409 if a user can not be created. (Most likely the email is already registered).\n\nSets the token if creation was successful.\n\nYou should prompt user for code and verify the token with `/verify`."

type CheckCookieDescription = Description "Checks if a parseable cookie is present in the request.\n\nDoes not check if the cookie itself contains valid credentials.\n\nReturns 204 NoContent if cookie is present.\n\nReturns 401 Unauthorized if the cookie is not present."

type ActivateDescription = Description "Activates a token using data from email"

type DeactivateDescription = Description "Deactivates a token using data from email"

-- MARK: Implementation

type AuthenticationHandler =
    "login" :> LoginDescription :>
        ReqBody '[JSON] AuthenticationCredits :>
        Header "User-Agent" String :>
        Post '[JSON] (Headers '[Header "Set-Cookie" String] UserLoginResponse) :<|>
    "logout" :> LogoutDescription
        :> Header "cookie" String
        :> PostNoContent '[JSON, PlainText, FormUrlEncoded] (Headers '[Header "Set-Cookie" String] NoContent) :<|>
    "verify" :> VerifyDescription :>
        Header "cookie" String :> ReqBody '[JSON] Int :> PostNoContent '[JSON, PlainText, FormUrlEncoded] NoContent :<|>
    "register" :> RegisterDescription :>
        ReqBody '[JSON] UserCreation :>
        Header "User-Agent" String :>
        PostNoContent '[JSON, PlainText, FormUrlEncoded] (Headers '[Header "Set-Cookie" String] NoContent) :<|>
    "checkCookie" :> CheckCookieDescription :>
        Header "cookie" String :> GetNoContent '[JSON, PlainText, FormUrlEncoded] NoContent :<|>
    "activate" :> ActivateDescription
        :> Header "cookie" String
        :> ReqBody '[JSON] TokenActivationData
        :> Post '[JSON] TokenActivationResult :<|>
    "deactivate" :> DeactivateDescription
        :> ReqBody '[JSON] TokenActivationData
        :> PostNoContent '[JSON] NoContent :<|>
    "activateUser"
        :> QueryParam "id" UserId
        :> QueryParam "data" String
        :> Get '[HTML] Markup :<|>
    "deactivateUser"
        :> QueryParam "id" UserId
        :> QueryParam "data" String
        :>  Get '[HTML] Markup

data UserResponseStatus = UserIsRegistered | UserCanRegister | InvalidUser
    deriving (Show)

instance ToJSON UserResponseStatus where
    toJSON UserIsRegistered = "registered"
    toJSON UserCanRegister = "canRegister"
    toJSON InvalidUser = "invalid"

data UserLoginResponse = UserLoginResponse {
    userStatus :: UserResponseStatus
} deriving (Show, GHC.Generic, ToJSON)

data AuthenticationCookieData = AuthenticationCookieData {
    cookieUserId :: UserId,
    cookieUserToken :: BS.ByteString
} deriving (Show, GHC.Generic, ToJSON, FromJSON)

data TokenActivationData = TokenActivationData {
    tokenVerificationUserId :: UserId,
    tokenVerificationData :: BS.ByteString
} deriving (Show)

instance ToJSON TokenActivationData where
    toJSON (TokenActivationData uId vData) = object [
        "id" .= uId,
        "data" .= vData
        ]

instance FromJSON TokenActivationData where
    parseJSON = withObject "token verification data" $ \ e ->
        TokenActivationData <$> e .: "id" <*> e .: "data"

data TokenActivationResult = TokenActivationResultYou | TokenActivationResultNotYou

instance ToJSON TokenActivationResult where
    toJSON TokenActivationResultYou = object ["you" .= True]
    toJSON TokenActivationResultNotYou = object ["you" .= False]

instance ToJSON BS.ByteString where
    toJSON = A.String . T.decodeUtf8 . BS.encode

instance FromJSON BS.ByteString where
    parseJSON (A.String s) = case (BS.decode . T.encodeUtf8) s of
        Left e -> fail e
        Right v -> return v
    parseJSON _ = fail "Couldn't deserialise ByteString"

newtype AuthenticationCredits = AuthenticationCredits {authenticationEmail :: UserEmail}
    deriving (GHC.Generic, ToJSON, FromJSON)

activateUser :: Maybe UserId -> Maybe String -> App Markup
activateUser Nothing _ = return $ page "The link is invalid."
activateUser _ Nothing = return $ page "The link is invalid."
activateUser (Just uId) (Just aData') = catchErrorPage $ do
    case BS.decode . C.pack $ aData' of
        Left _ -> return $ page $ "The link is invalid."
        Right aData -> do
            tokenData' <- runQerror $ tryActivateToken uId aData
            case tokenData' of
                Nothing -> return $ page "This activation link has been invalidated."
                Just Token{..} -> return $ genActivationPage "in" tokenUserAgent

deactivateUser :: Maybe UserId -> Maybe String -> App Markup
deactivateUser Nothing _ = return $ page "The link is invalid."
deactivateUser _ Nothing = return $ page "The link is invalid."
deactivateUser (Just uId) (Just dData') = catchErrorPage $ do
    case BS.decode . C.pack $ dData' of
        Left _ -> return $ page $ "The link is invalid."
        Right dData -> do
            ua' <- runQerror $ deactivateToken uId dData
            case ua' of
                Nothing -> return $ page "This activation link has been invalidated."
                Just ua -> return $ genActivationPage "out" ua

catchErrorPage :: App Markup -> App Markup
catchErrorPage = flip catchError $ \ (_ :: ServantErr) -> return errorPage

authenticationAPI :: ServerT AuthenticationHandler App
authenticationAPI = logIn :<|> logOut :<|> verifyToken :<|> register :<|> checkCookie :<|> validateTokenApi :<|> deactivateTokenApi :<|> activateUser :<|> deactivateUser

logIn :: AuthenticationCredits -> Maybe String -> App (Headers '[Header "Set-Cookie" String] UserLoginResponse)
logIn (AuthenticationCredits email') ua' = do
    let email = T.toLower . T.strip $ email'
    if (not . validateEmail) email
        then return . noHeader $ UserLoginResponse InvalidUser
        else do
            user <- runQerror $ getUserWithEmail email
            case user of
                Nothing -> return . noHeader $ UserLoginResponse UserCanRegister
                Just User{..} -> do
                    let ua = getUserAgent ua'
                    GeneratedToken token value code activation deactivation <- generateToken userId ua
                    runQerror . createToken $ token
                    let cookie = generateCookie token value
                    sendTokenVerificationEmail code userId (BS.encode activation) (BS.encode deactivation) (TL.fromStrict ua) email
                    return . addHeader cookie $ UserLoginResponse UserIsRegistered

generateCookie :: Token -> BS.ByteString -> String
generateCookie token value = L.unpack . toLazyByteString . renderSetCookie $ defaultSetCookie {
    setCookieName = cookieTokenKey,
    setCookieValue = L.toStrict . encode $ AuthenticationCookieData (tokenUserId token) value,
    setCookieExpires = Just (tokenExpiryDate token),
    setCookiePath = Just "/",
    setCookieHttpOnly = True,
    setCookieSecure = True }

getCookie :: String -> Maybe AuthenticationCookieData
getCookie = (>>= (decode . L.fromStrict)) . lookup cookieTokenKey . parseCookies . C.pack

checkCookie :: Maybe String -> App NoContent
checkCookie cookie = do
    let token = cookie >>= getCookie
    case token of
        Nothing -> throwError err401
        Just _ -> return NoContent

verifyToken :: Maybe String -> Int -> App NoContent
verifyToken Nothing _ = throwError err400
verifyToken (Just cookie) code = do
    let mbToken = getCookie cookie
    case mbToken of
        Nothing -> throwError err400
        (Just (AuthenticationCookieData uId tokenData)) -> do
            token <- runQ invalidToken $ tryVerifyToken uId tokenData code
            case token of
                Just _ -> return NoContent
                Nothing -> throwError err403

logOut :: Maybe String -> App (Headers '[Header "Set-Cookie" String] NoContent)
logOut cookie = do
    _ <- fromMaybe (return ()) $ do
        AuthenticationCookieData uId tokenData <- cookie >>= getCookie
        return . runQsilent $ invalidateToken uId tokenData
    let newCookie = defaultSetCookie {
        setCookieName = cookieTokenKey,
        setCookieValue = "invalid",
        setCookieExpires = Just (UTCTime (ModifiedJulianDay 0) 0),
        setCookiePath = Just "/" }
    return $ addHeader (L.unpack . toLazyByteString . renderSetCookie $ newCookie) NoContent

validateTokenApi :: Maybe String -> TokenActivationData -> App TokenActivationResult
validateTokenApi cookie' (TokenActivationData uId aData) = do
    tokenData' <- runQerror $ tryActivateToken uId aData
    case tokenData' of
        Nothing -> throwError err401
        Just Token{..} -> do
            case cookie' >>= getCookie of
                Nothing -> return TokenActivationResultNotYou
                Just (AuthenticationCookieData uId' cData) ->
                    if hash cData == tokenValue && uId' == tokenUserId
                        then return TokenActivationResultYou
                        else return TokenActivationResultNotYou

deactivateTokenApi :: TokenActivationData -> App NoContent
deactivateTokenApi (TokenActivationData uId dData) = do
    res <- runQerror $ deactivateToken uId dData
    if isJust res
        then return NoContent
        else throwError err401

checkToken :: DBConnection -> AuthenticationCookieData -> Handler UserId
checkToken conn (AuthenticationCookieData uId tokenData) = do
    token <- runQ' conn err500 $ getVerifiedToken uId tokenData
    case token of
        Nothing -> throwError invalidToken
        Just (Token newUId _ _ _ _ _ _ _) -> return newUId

register :: UserCreation -> Maybe String -> App (Headers '[Header "Set-Cookie" String] NoContent)
register creation' ua' = do
    let creation = normalizaUser creation'
    unless (validateUser creation) $ throwError err400
    genToken <- generateTokenM
    let ua = getUserAgent ua'
    GeneratedToken token value code activation deactivation <- runQ err409 . commitedTransactionallyUpdate $ do
        uId <- createUser creation
        let gToken = genToken uId ua
        createToken . generatedToken $ gToken
        return gToken
    sendTokenVerificationEmail code (tokenUserId token) (BS.encode activation) (BS.encode deactivation) (TL.fromStrict ua) (userCreationUserEmail creation)
    let cookie = generateCookie token value
    return $ addHeader cookie NoContent

getUserAgent :: Maybe String -> T.Text
getUserAgent uaData' = fromMaybe "Unknown" $ do
    uaData <- fmap C.pack uaData'
    let os = osrFamily <$> parseOS uaData
    let browser = uarFamily <$> parseUA uaData
    foldl (mappendMaybeThrough " — ") Nothing $ [os, browser]
    where
        mappendMaybeThrough :: (Semigroup a) => a -> Maybe a -> Maybe a -> Maybe a
        mappendMaybeThrough _ Nothing (Just a) = Just a
        mappendMaybeThrough _ (Just a) Nothing = Just a
        mappendMaybeThrough i (Just a) (Just b) = Just (a <> i <> b)
        mappendMaybeThrough _ Nothing Nothing = Nothing

normalizaUser :: UserCreation -> UserCreation
normalizaUser (UserCreation fName mName lName faculty email) =
    UserCreation
        (T.strip fName)
        (T.strip mName)
        (T.strip lName)
        (T.toLower . T.strip $ faculty)
        (T.toLower . T.strip $ email)

validateUser :: UserCreation -> Bool
validateUser (UserCreation fName mName lName faculty email) =
    (not . T.null) fName &&
    (not . T.null) mName &&
    (not . T.null) lName &&
    (not . T.null) faculty &&
    validateEmail email

authHandler :: DBConnection -> AuthHandler Request UserId
authHandler conn = mkAuthHandler handler
    where
        handler req = do
            let cookie' = lookup "cookie" $ requestHeaders req
            case cookie' of
                Nothing -> throwError (invalidToken)
                (Just cookie) -> do
                    let mbToken = (>>= (decode . L.fromStrict)) . lookup cookieTokenKey $ parseCookies cookie
                    case mbToken of
                        Nothing -> throwError invalidToken
                        Just t -> checkToken conn t

cookieTokenKey :: BS.ByteString
cookieTokenKey = "valera-denis-auth-cookie"

type instance AuthServerData (AuthProtect "basicAuth") = UserId

genAuthServerContext :: DBConnection -> Context (AuthHandler Request UserId ': '[])
genAuthServerContext conn = authHandler conn :. EmptyContext

contextProxy :: Proxy '[AuthHandler Request UserId]
contextProxy = Proxy

validateEmail :: T.Text -> Bool
validateEmail "ilyakooo0@gmail.com" = True
validateEmail "rednikina.com@yandex.ru" = True
validateEmail "admikhaleva@edu.hse.ru" = True
validateEmail e = (isJust . matchRegex regex . T.unpack $ e) || (isJust . matchRegex falseRegex . T.unpack $ e)
    where
        regex = mkRegexWithOpts
            "^[A-Z0-9a-z._%-]+@hse\\.ru$"
            False
            False
        falseRegex = mkRegexWithOpts
            "^[A-Z0-9a-z._%-+]+@edu.hse\\.ru$"
            False
            False
