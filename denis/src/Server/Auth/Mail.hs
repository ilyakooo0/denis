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

module Server.Auth.Mail where

import Network.Mail.SMTP
import Server.App
import Server.Auth.Token
import Control.Monad.IO.Class
import Network.Mail.Mime (Mail, Part)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Text.Blaze.Html5 as H
import Text.Blaze.XHtml5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Data.ByteString (ByteString)
import Data.User
import Data.ByteString.Char8 (unpack)
import Control.Monad.Reader

sendTokenVerificationEmail :: TokenVerificationCode -> UserId -> ByteString -> ByteString -> TL.Text -> Text -> App ()
sendTokenVerificationEmail userCode uId activation deactivation ua email = do
    cfg <- ask
    let mailCfg = mailConfig cfg
    let rootUrl = selfRootUrl cfg
    let mail = simpleMail
            (Address (Just "HSE Social Network") "noreply@hse.ru")
            [Address Nothing email] [] []
            "HSE Social Network Verification Code"
            [genMail rootUrl userCode uId (unpack activation) (unpack deactivation) ua]
    sendMailWithConfig mailCfg mail

sendMailWithConfig :: (MonadIO m) => MailConfig -> Mail -> m ()
sendMailWithConfig (MailConfig host port user pass) =
    liftIO . sendMailWithLogin' host (fromIntegral port) user pass

genMail :: String -> TokenVerificationCode -> UserId -> String -> String -> TL.Text -> Part
genMail rootUrl userCode uId activation deactivation ua = htmlPart . renderHtml $ html $ body ! A.style "font-family: sans-serif;" $ do
    H.div ! A.style "display: none; max-height: 0px; overflow: hidden;" $ do
        "Verification code: " <> toHtml ua
        br
        toHtml [a', b', ' ', c', d', ' ', e', f']
    H.div ! A.style "display: none; max-height: 0px; overflow: hidden;" $ "            "
    H.div ! A.style "text-align: center; margin-top: 55px;" $ do
        H.a ! href (stringValue $ rootUrl <> "/authentication/activateUser?id=" <> show uId <> "&data=" <> activation) ! A.style "font-size: 34px; font-weight: bold; display: inline-block; padding: 7px 13px 7px 13px; margin-bottom: 89px; text-decoration: underline; font-family: sans-serif; color: #555;" $ "Log in"
        br
        H.div ! A.style "font-size: 34px; font-weight: bold;" $ do
            H.span ! A.style "color: #bbb; font-weight: bold;" $ "from: "
            toHtml ua
        br
        H.div ! A.style "font-size: 55px; font-weight: bold; border-radius: 10px; background-color: #eee; display: inline-block; padding: 7px 13px 7px 13px; box-shadow: 0px 5px 21px #bbb; background-image: linear-gradient(#f9f9f9, #eee);" $ do
            toHtml [a', b']
            H.div ! A.style "display: inline-block; width: 13px;" $ mempty
            toHtml [c', d']
            H.div ! A.style "display: inline-block; width: 13px;" $ mempty
            toHtml [e', f']
    H.div ! A.style "text-align: center; margin-top: 233px;" $ H.a ! href (stringValue $ rootUrl <> "/authentication/deactivateUser?id=" <> show uId <> "&data=" <> deactivation) ! A.style "text-decoration: underline; font-family: sans-serif; color: #555;font-size: 13px; margin-bottom: 21px;" $ "Revoke access"
    where
        [a', b', c', d', e', f'] = reverse . take 6 . (++ repeat '0') . reverse . show $ userCode
