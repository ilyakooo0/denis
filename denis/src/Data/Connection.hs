{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    FlexibleContexts #-}

module Data.Connection (
    Connection,
    StaticPQ,
    runQ,
    runQ',
    runQnotFound,
    runQerror,
    maybeNotFound,
    maybeInvalidToken,
    commitedTransactionallyUpdate
) where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Pool
import Data.Schema
import Server.App
import Control.Monad.Reader
import Control.Monad.Except
import Servant.Server
import Server.Error
import Control.Monad.Trans.Control
import Squeal.PostgreSQL.Transaction
import Squeal.PostgreSQL.PQ

type StaticPQ = PoolPQ Schema Handler


runQ :: ServantErr -> StaticPQ a -> App a
runQ err req = handleSqueal (const $ throwError err) $ asks getPool >>= lift . runPoolPQ req

runQ' :: DBConnection -> ServantErr -> StaticPQ a -> Handler a
runQ' conn err req = handleSqueal (const $ throwError err) $ runPoolPQ req conn

runQnotFound :: StaticPQ a -> App a
runQnotFound = runQ err404

runQerror :: StaticPQ a -> App a
runQerror = runQ err500

maybeNotFound :: App (Maybe a) -> App a
maybeNotFound = (>>= (\t -> case t of
    Just y -> return y
    Nothing -> throwError err404))

maybeInvalidToken :: App (Maybe a) -> App a
maybeInvalidToken = (>>= (\t -> case t of
    Just y -> return y
    Nothing -> throwError invalidToken))
    
commitedTransactionallyUpdate :: (MonadBaseControl IO tx, MonadPQ schema tx) => tx x -> tx x
commitedTransactionallyUpdate = transactionally $ TransactionMode ReadCommitted ReadWrite NotDeferrable