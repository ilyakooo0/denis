{-# LANGUAGE OverloadedStrings #-}

module Server.Error (
    invalidToken
) where 

import Servant.Server

invalidToken :: ServantErr
invalidToken = ServantErr {
    errHTTPCode = 498,
    errReasonPhrase = "Invalid Token",
    errBody = "The supplied token is invalid",
    errHeaders = []
}
