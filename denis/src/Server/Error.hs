{-# LANGUAGE OverloadedStrings #-}

module Server.Error (
    invalidToken
) where 

import Servant.Server

invalidToken :: ServantErr
invalidToken = ServantErr {
    errHTTPCode = 401,
    errReasonPhrase = "Invalid Token",
    errBody = "The supplied token is invalid",
    errHeaders = []
}
