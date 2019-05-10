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

module Server.Auth.Mail (
    sendTokenVerificationEmail
    ) where

import Network.Mail.SMTP
import Server.App
import Server.Auth.Token
import Control.Monad.IO.Class
import Network.Mail.Mime (Mail)
import Data.Text (Text, pack)
import Data.Text.Lazy (fromStrict)

sendTokenVerificationEmail :: TokenVerificationCode -> Text -> App ()
sendTokenVerificationEmail code email = do
    mailCfg <- mailConfig <$> ask
    let mail = simpleMail
            (Address (Just "HSE Social Network") "noreply@hse.ru")
            [Address Nothing email] [] []
            "HSE Social Network Verification Code"
            [plainTextPart . fromStrict . pack . show $ code]
    sendMailWithConfig mailCfg mail
    -- sendMailWithLogin' "smtp.sendgrid.net" 25 "apikey" "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" (simpleMail (Address (Just "HSE") "foo@hse.ru") [(Address (Just "YOU") "iikostyuchenko@edu.hse.ru")] [] [] "HI" [plainTextPart "HELLO\n\nsosi"])

sendMailWithConfig :: (MonadIO m) => MailConfig -> Mail -> m ()
sendMailWithConfig (MailConfig host port user pass) =
    liftIO . sendMailWithLogin' host (fromIntegral port) user pass
