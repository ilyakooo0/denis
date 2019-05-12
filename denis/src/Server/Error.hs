{-# LANGUAGE OverloadedStrings #-}

module Server.Error (
    invalidToken,
    limitExceeded,
    throwError
) where

import Servant.Server
import Control.Monad.Except

invalidToken :: ServantErr
invalidToken = ServantErr {
    errHTTPCode = 498,
    errReasonPhrase = "Invalid Token",
    errBody = "",
    errHeaders = []
}

limitExceeded :: String -> ServantErr
limitExceeded msg = err406 {
    errReasonPhrase = msg
}
