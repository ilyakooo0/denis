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
    generateTokenM,
    hashVerificationCode,
    GeneratedToken(..),
    hash
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
import Crypto.Hash.SHA512
import qualified Data.ByteString.Char8 as C

type TokenVerificationCode = Int

data GeneratedToken = GeneratedToken {
    generatedToken :: Token,
    generatedTokenValue :: ByteString,
    generatedVerificationCode :: Int,
    generatedActivationCode :: ByteString,
    generatedDeactivationCode :: ByteString
}

data Token = Token {
    tokenUserId :: UserId,
    tokenValue :: ByteString,
    tokenExpiryDate :: UTCTime,
    tokenVerificationCode :: Maybe ByteString,
    tokenActivationTriesLeft :: Int32,
    tokenUserAgent :: Text,
    tokenActivationCode :: Maybe ByteString,
    tokenDeactivationCode :: ByteString
} deriving (Show, GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

generateToken :: (MonadIO m, CryptoRNG m) => UserId -> Text -> m GeneratedToken
generateToken uId ua = do
    gen <- generateTokenM
    return $ gen uId ua

generateTokenM :: (MonadIO m, CryptoRNG m) => m (UserId -> Text -> GeneratedToken)
generateTokenM = do
    token <- randomBytes 64
    activationToken <- randomBytes 64
    deactivationToken <- randomBytes 64
    code <- randomR (0, 999999)
    expire <- liftIO $ fmap (addUTCTime (60*30 {- 30 minutes -} {- nominalDay * 365 -})) getCurrentTime -- one year
    return $ \ uId ua -> GeneratedToken {
        generatedToken = Token uId (hash token) expire (Just . hashVerificationCode $ code) 10 ua (Just . hash $ activationToken) (hash deactivationToken),
        generatedTokenValue = token,
        generatedVerificationCode = code,
        generatedActivationCode = activationToken,
        generatedDeactivationCode = deactivationToken
        }

hashVerificationCode :: Int -> ByteString
hashVerificationCode = hash . C.pack . show