{-# LANGUAGE DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables #-}

module Server.Logger (
    Logger,
    LoggerAPI,
    mkLogger,
    Log
) where

import Network.Wai
import Control.Concurrent.STM
import Data.CyclicBuffer
import Servant
import Servant.API
import Servant.Server
import Data.Text.Encoding
import qualified Data.Text as T
import Data.Foldable
import qualified Data.ByteString as BL
import Data.List
import Server.App
import Data.CaseInsensitive (original)
import Data.Monoid ((<>), mempty, mappend)
import Network.HTTP.Types.Header
import Network.Wai.Middleware.RequestLogger
import Data.Default.Class
import qualified System.Log.FastLogger as FL
import Data.ByteString.Char8 (unpack)

type Log = T.Text

type LoggerAPI = "log" :> Get '[PlainText] Log
type Logger = App Log

mkLogger :: Int -> IO (Middleware, Logger)
mkLogger lim = do
    buff <- newTVarIO $ mkCyclicBuffer lim
    logger <- mkWaiLogger buff
    return (logger, getLog buff)

getLog :: TVar (CyclicBuffer BL.ByteString) -> Logger
getLog buff = decodeUtf8 . BL.concat . intersperse "\n" . elems <$> liftIO (readTVarIO buff)

mkWaiLogger :: TVar (CyclicBuffer BL.ByteString) -> IO Middleware
mkWaiLogger buff = let 
    settings = def {
        -- outputFormat = CustomOutputFormatWithDetails (\ date req _ _ tDiff bss bldr -> 
        --     FL.toLogStr (show date) <> (FL.toLogStr . unpack $ fold bss))
        outputFormat = Detailed False,
        destination = Callback (atomically . modifyTVar buff . flip insertIntoCyclicBuffer . FL.fromLogStr)    
        }
    in mkRequestLogger settings