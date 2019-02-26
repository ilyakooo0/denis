{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Server.App (
    App,
    DBConnection,
    Config(..),
    ask,
    asks,
    liftIO
) where

import Servant.Server
import Control.Monad.Reader
import Squeal.PostgreSQL.Pool
import Data.Schema
import Generics.SOP (K)
import Squeal.PostgreSQL.PQ (Connection)
import Control.Monad.IO.Class ()

type App = ReaderT Config Handler 

type DBConnection = Pool (K Connection Schema)

data Config = Config {
    getPool :: DBConnection
}
