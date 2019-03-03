{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Data.Connection (
    Connection,
    StaticPQ,
    runQ,
    runQ',
    runQnotFound,
    runQerror
) where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Pool
import Data.Schema
import Server.App
import Control.Monad.Reader
import Control.Monad.Except
import Servant.Server

type StaticPQ = PoolPQ Schema Handler


runQ :: ServantErr -> StaticPQ a -> App a
runQ err req = handleSqueal (const $ throwError err) $ asks getPool >>= lift . runPoolPQ req

runQ' :: DBConnection -> ServantErr -> StaticPQ a -> Handler a
runQ' conn err req = handleSqueal (const $ throwError err) $ runPoolPQ req conn

runQnotFound :: StaticPQ a -> App a
runQnotFound = runQ err404

runQerror :: StaticPQ a -> App a
runQerror = runQ err500
