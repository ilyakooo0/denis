{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    FlexibleContexts #-}

module Data.Connection where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Pool
import Data.Schema
import Server.App
import Control.Monad.Reader
import Control.Monad.Except
import Servant.Server
import Server.Error
import Control.Monad.Trans.Control

-- |Монада для совершения запросов и манимпуляций с базой данных в рамках сервера.
type StaticPQ = PoolPQ Schema Handler

-- |Функция, печатающая текст красным цветом в консоль.
printRed :: (Show a) => a -> IO ()
printRed a = do
    putStr "\a\x001b[31m"
    print a
    putStr "\x001b[0m"

-- |Функция, выполняющая запрос к бае данных.
runQ :: ServantErr -> StaticPQ a -> App a
runQ err req = handleSqueal ((>> throwError err) . liftIO . printRed) $ asks getPool >>= lift . runPoolPQ req

-- |Функция, выполняющая запрос к бае данных.
runQ' :: DBConnection -> ServantErr -> StaticPQ a -> Handler a
runQ' conn err req = handleSqueal (const $ throwError err) $ runPoolPQ req conn

-- |Функция, выполняющая запрос к бае данных.
runQnotFound :: StaticPQ a -> App a
runQnotFound = runQ err404

-- |Функция, выполняющая запрос к бае данных.
runQerror :: StaticPQ a -> App a
runQerror = runQ err500

-- |Функция, выполняющая запрос к бае данных.
runQsilent :: StaticPQ a -> App ()
runQsilent req = handleSqueal (liftIO . print) $ asks getPool >>= lift . runPoolPQ req
    >> return ()

-- |Функция, выкидывающая исключение при отсутсвии значения.
maybeNotFound :: App (Maybe a) -> App a
maybeNotFound = (>>= (\t -> case t of
    Just y -> return y
    Nothing -> throwError err404))

-- |Функция, выкидывающая исключение при отсутсвии значения.
maybeInvalidToken :: App (Maybe a) -> App a
maybeInvalidToken = (>>= (\t -> case t of
    Just y -> return y
    Nothing -> throwError invalidToken))

-- |Функция, совершающая операцию внутри транакции базы данных.
commitedTransactionallyUpdate :: (MonadBaseControl IO tx, MonadPQ schema tx) => tx x -> tx x
commitedTransactionallyUpdate = transactionally $ TransactionMode ReadCommitted ReadWrite NotDeferrable