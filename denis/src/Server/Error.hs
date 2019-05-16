{-# LANGUAGE OverloadedStrings #-}

module Server.Error where

import Servant.Server
import Control.Monad.Except

invalidToken :: ServantErr
invalidToken = ServantErr {
    errHTTPCode = 498,
    errReasonPhrase = "Invalid Token",
    errBody = "",
    errHeaders = []
}

amountExceeded :: ServantErr
amountExceeded = ServantErr {
    errHTTPCode = 470,
    errReasonPhrase = "The amount of elements the user can have has been exceeded.",
    errBody = "",
    errHeaders = []
}

lengthExceeded :: ServantErr
lengthExceeded = ServantErr {
    errHTTPCode = 471,
    errReasonPhrase = "The length of user content has been exceeded.",
    errBody = "",
    errHeaders = []
}

formatError :: ServantErr
formatError = ServantErr {
    errHTTPCode = 472,
    errReasonPhrase = "The format of the supplied content is invalid.",
    errBody = "",
    errHeaders = []
}

impossibleContent :: ServantErr
impossibleContent = ServantErr {
    errHTTPCode = 473,
    errReasonPhrase = "The content you have supplied is invalid and/or conflict with the rest of the existing content.",
    errBody = "",
    errHeaders = []
}

