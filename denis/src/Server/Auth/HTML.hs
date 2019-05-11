{-# LANGUAGE DataKinds,
    DeriveGeneric,
    FlexibleContexts,
    FlexibleInstances,
    MultiParamTypeClasses,
    OverloadedStrings,
    ScopedTypeVariables,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances,
    DeriveAnyClass,
    RecordWildCards #-}

module Server.Auth.HTML (
    HTML,
    genActivationPage,
    Markup,
    errorPage,
    page
    ) where

import Servant.HTML.Blaze
import Text.Blaze.Html5 as H
import Text.Blaze.XHtml5.Attributes as A
import Data.Text (Text)

genActivationPage :: Text -> Text -> Markup
genActivationPage verb ua = genPage $ do
    "You have successfully "
    u $ do
        "logged "
        toHtml verb
    " on "
    u $ toHtml ua


genPage :: Markup -> Markup
genPage pageContent = html $ do
    H.head $ do
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
    body ! A.style "font-family: sans-serif; background-color: #eee" $ do
        H.div ! A.style "text-align: center; margin-top: 20vh; margin-left: 10%; margin-right: 10%" $ do
            H.div ! A.style "font-size: 40px; font-weight: bold;" $ pageContent

errorPage :: Markup
errorPage = genPage $ "Sorry, something went wrong."

page :: Text -> Markup
page t = genPage $ toHtml t