{-# LANGUAGE DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverloadedStrings,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators,
    DeriveAnyClass #-}

module Server.Auth.Token (
    Token(..),
    generateToken,
    TokenVerificationCode,
    generateTokenM
    ) where

import Data.Time.Clock
import Data.ByteString
import Data.User
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Int
import Control.Monad.IO.Class
import Crypto.RNG
import Data.Text (Text)

type TokenVerificationCode = Int32

data Token = Token {
    tokenUserId :: UserId,
    tokenValue :: ByteString,
    tokenExpiryDate :: UTCTime,
    tokenVerificationCode :: Maybe TokenVerificationCode,
    tokenActivationTriesLeft :: Int32,
    tokenUserAgent :: Text,
    tokenActivationCode :: Maybe ByteString,
    tokenDeactivationCode :: ByteString
} deriving (Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

generateToken :: (MonadIO m, CryptoRNG m) => UserId -> Text -> m Token
generateToken uId ua = do
    gen <- generateTokenM
    return $ gen uId ua

generateTokenM :: (MonadIO m, CryptoRNG m) => m (UserId -> Text -> Token)
generateTokenM = do
    token <- randomBytes 64
    activationToken <- randomBytes 64
    deactivationToken <- randomBytes 64
    code <- randomR (0, 999999)
    expire <- liftIO $ fmap (addUTCTime (nominalDay * 365)) getCurrentTime -- one year
    return $ \ uId ua -> Token uId token expire (Just code) 10 ua (Just activationToken) deactivationToken