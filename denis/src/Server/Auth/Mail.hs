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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Server.Auth.Mail (
    sendTokenVerificationEmail
    ) where

import Network.Mail.SMTP
import Server.App
import Server.Auth.Token
import Control.Monad.IO.Class
import Network.Mail.Mime (Mail, Part)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

sendTokenVerificationEmail :: TokenVerificationCode -> Text -> App ()
sendTokenVerificationEmail code email = do
    mailCfg <- mailConfig <$> ask
    let mail = simpleMail
            (Address (Just "HSE Social Network") "noreply@hse.ru")
            [Address Nothing email] [] []
            "HSE Social Network Verification Code"
            [genMail code]
    sendMailWithConfig mailCfg mail

sendMailWithConfig :: (MonadIO m) => MailConfig -> Mail -> m ()
sendMailWithConfig (MailConfig host port user pass) =
    liftIO . sendMailWithLogin' host (fromIntegral port) user pass

genMail :: TokenVerificationCode -> Part
genMail code = htmlPart $ "<html><body style=\"font-family: sans-serif;\"> <h1> <div align=\"center\" style=\"margin-top: 20vh;\"> <a href=\"#\" style=\"font-size: 34px; font-weight: bold; display: inline-block; padding: 7px 13px 7px 13px; margin-bottom: 89px; text-decoration: underline; font-family: sans-serif; color: #555;\"> Log in </a> <br/> <div style=\"font-size: 55px; font-weight: bold; border-radius: 10px; background-color: #eee; display: inline-block; padding: 7px 13px 7px 13px; box-shadow: 0px 5px 21px #bbb; background-image: linear-gradient(#f9f9f9, #eee);\">" <> TL.pack [a, b] <> "<div style=\"display: inline-block; width: 10px;\"></div>" <> TL.pack [c, d] <> "<div style=\"display: inline-block; width: 10px;\"></div>" <> TL.pack [e, f] <> "</div></div><div align=\"center\" style=\"margin-top: 233px;\"> <a href=\"#\" style=\"text-decoration: underline; font-family: sans-serif; color: #555;font-size: 13px;\">Revoke access</a> </div></body></html>"
    where
        [a, b, c, d, e, f] = reverse . take 6 . (++ repeat '0') . reverse . show $ code
