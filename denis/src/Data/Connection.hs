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
    runQnotFound
) where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.PQ
import Squeal.PostgreSQL.Pool
import qualified Generics.SOP as SOP
import Data.Schema
import Server.App
import Control.Monad.Base
import Control.Monad.Reader
import Servant.Server
import Control.Monad.Error
import Servant.Server

type StaticPQ = PoolPQ Schema Handler


runQ :: ServantErr -> StaticPQ a -> App a
runQ err req = handleSqueal (const $ throwError err) $ asks getPool >>= lift . runPoolPQ req

runQ' :: DBConnection -> ServantErr -> StaticPQ a -> Handler a
runQ' conn err req = handleSqueal (const $ throwError err) $ runPoolPQ req conn

runQnotFound = runQ err404
