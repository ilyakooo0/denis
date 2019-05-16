{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    TypeSynonymInstances,
    FlexibleInstances #-}

module Server.App where

import Servant.Server
import Control.Monad.Reader
import Squeal.PostgreSQL.Pool
import Data.Schema
import Generics.SOP (K)
import Squeal.PostgreSQL.PQ (Connection)
import Crypto.RNG
import Server.Error
import Control.Monad.Except

type App = ReaderT Config Handler

type DBConnection = Pool (K Connection Schema)

data Config = Config {
    getPool :: !DBConnection,
    cryptoState :: !CryptoRNGState,
    selfRootUrl :: !String,
    mailConfig :: !MailConfig
}

data MailConfig = MailConfig {
    mailHost :: !String,
    mailPort :: !Int,
    mailUser :: !String,
    mailPassword :: !String
}

instance CryptoRNG App where
    randomBytes n = join $ asks (liftIO . randomBytesIO n . cryptoState)

fromMaybeThrow :: ServantErr -> App (Maybe a) -> App a
fromMaybeThrow err mb = do
    a <- mb
    case a of
        Just b -> return b
        Nothing -> throwError err