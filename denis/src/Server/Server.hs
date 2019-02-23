{-# LANGUAGE OverloadedStrings #-}

module Server.Server (
    runServer
) where

import Server.API
import Server.Auth
import Server.App

import Servant.Server
import Servant.API
import System.Environment
import qualified Data.ByteString.Char8 as B
import Squeal.PostgreSQL.Pool
import Data.Maybe
import Control.Monad.Reader
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Method 

import Network.Wai.Middleware.Cors


getDBString :: IO B.ByteString
getDBString = do
    mDatabaseURL <- lookupEnv "DATABASE_URL"
    return $ B.pack $ fromMaybe "host=localhost port=5432 dbname=postgres connect_timeout=10" mDatabaseURL

getServerPort :: IO Int
getServerPort = maybe 2000 read <$> lookupEnv "PORT" 

getConfig :: IO Config
getConfig = do
    databaseURL <- getDBString
    conn <- createConnectionPool databaseURL 1 0.5 10
    return $ Config conn

runServer :: IO ()
runServer = do
    port <- getServerPort
    cfg <- getConfig
    let ctx = genAuthServerContext $ getPool cfg
    let s = hoistServerWithContext serverProxy contextProxy (flip runReaderT cfg) serverAPI
    runSettings (settings port) $ cors (const $ Just policy) $ serveWithContext serverProxy ctx s
    -- TODO: remove simpleCors

-- runServer :: IO ()
-- runServer = do
--     s <- getServer

policy :: CorsResourcePolicy
policy = CorsResourcePolicy {
    corsOrigins = Just (["http://localhost:8080", "http://localhost:4000", "http://127.0.0.1:4000", "http://127.0.0.1:8080", "http://lvh.me:4000", "http://lvh.me:8080"], True), -- Nothing,
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