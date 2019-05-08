{-# LANGUAGE DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables,
    TypeSynonymInstances,
    FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString as BL
import Data.List
import Server.App
import Network.Wai.Middleware.RequestLogger
import Data.Default.Class
import qualified System.Log.FastLogger as FL
import Servant.Docs

instance HasDocs "log" where
    docsFor _ _ _ = emptyAPI

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
        outputFormat = Apache FromHeader,
        destination = Callback (atomically . modifyTVar buff . flip insertIntoCyclicBuffer . FL.fromLogStr)
        }
    in mkRequestLogger settings