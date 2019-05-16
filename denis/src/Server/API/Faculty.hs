{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    LambdaCase #-}

module Server.API.Faculty where

import Servant.Server
import Servant
import Server.App
import Data.Connection
import Data.Query
import Data.Text (Text)
import qualified Data.Text as T
import Data.Faculty
import Data.Text.Validator
import Control.Monad
import Server.Error
import Data.Char

type SearchDescription = Description "Возвращает факультет по данному запросу."

type FacultyAPI =
    "search" :> SearchDescription :> ReqBody '[PlainText, JSON] Text :> Post '[JSON] [Faculty]

facultyServer :: ServerT FacultyAPI App
facultyServer = search

search :: Text -> App [Faculty]
search query = do
    if T.null . T.filter (not . isSpace) . T.filter isPrint $ query
        then return []
    else do
        unless (validateText query) $ throwError lengthExceeded
        runQerror . getFacultyFromQuery $ query