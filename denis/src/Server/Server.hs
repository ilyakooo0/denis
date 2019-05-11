{-# LANGUAGE OverloadedStrings #-}

module Server.Server (
    runServer
) where

import Server.API
import Server.Auth
import Server.App

import Servant.Server
import System.Environment
import qualified Data.ByteString.Char8 as B
import Squeal.PostgreSQL.Pool
import Data.Maybe
import Control.Monad.Reader
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Method
import Server.Logger
import Control.Concurrent
import Data.Faculty.Parser
import Data.Connection
import Data.Query
import Crypto.RNG
import Network.Wai.Middleware.Cors

getDBString :: IO B.ByteString
getDBString = do
    mDatabaseURL <- lookupEnv "DATABASE_URL"
    return $ B.pack $ fromMaybe "host=localhost port=5432 dbname=postgres connect_timeout=10" mDatabaseURL

getMailConfig :: IO MailConfig
getMailConfig = do
    (Just host) <- lookupEnv "MAIL_HOST"
    port <- (fromMaybe 25 . fmap read) <$> lookupEnv "MAIL_PORT"
    (Just user) <- lookupEnv "MAIL_USER"
    (Just pass) <- lookupEnv "MAIL_PASS"
    return $ MailConfig host port user pass

getServerPort :: IO Int
getServerPort = maybe 2000 read <$> lookupEnv "PORT"

getConfig :: IO Config
getConfig = do
    databaseURL <- getDBString
    conn <- createConnectionPool databaseURL 3 0.5 10
    crypto <- newCryptoRNGState
    -- (Jusr selfRoot) <- lookupEnv "SELF_ROOT_URL"
    mail <- getMailConfig
    return $ Config conn crypto mail

runServer :: IO ()
runServer = do
    port <- getServerPort
    cfg <- getConfig
    void . forkIO $ updateFaculties cfg
    let ctx = genAuthServerContext $ getPool cfg
    (middleLogger, logger) <- mkLogger 100
    let serverAPI = mkServerAPI logger
    let s = hoistServerWithContext serverProxy contextProxy (flip runReaderT cfg) serverAPI
    runSettings (settings port) $ cors (const $ Just policy) . middleLogger $ serveWithContext serverProxy ctx s

policy :: CorsResourcePolicy
policy = CorsResourcePolicy {
    corsOrigins = Just (["http://localhost:8080", "http://localhost:4000", "http://127.0.0.1:4000", "http://127.0.0.1:8080", "http://lvh.me:4000", "http://lvh.me:8080", "http://valera-kristina.herokuapp.com", "https://valera-kristina.herokuapp.com"], True), -- Nothing,
    corsMethods=[methodGet, methodPost],
    corsRequestHeaders = ["content-type"],
    corsExposedHeaders = Nothing,
    corsMaxAge = Nothing,
    corsVaryOrigin = True,
    corsRequireOrigin = False,
    corsIgnoreFailures = True
}

settings :: Int -> Settings
settings port = setPort port . setServerName "Denis" $ defaultSettings

updateFaculties :: Config -> IO ()
updateFaculties cfg = do
    fs <- getFaculties
    forM_ fs (runHandler . flip runReaderT cfg . runQerror . updateFaculty)