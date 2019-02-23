{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}

module Main where

import Data.Schema
import Squeal.PostgreSQL.Definition
import Squeal.PostgreSQL.Render
import Squeal.PostgreSQL.Schema
import Squeal.PostgreSQL.Expression
import Server.Server

main :: IO ()
main = runServer
