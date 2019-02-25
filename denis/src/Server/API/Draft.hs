{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Server.API.Draft (
    DraftApi,
    draftApi,
    DraftUpdate(..)
) where

import Data.Proxy
import Servant.API
import qualified Data.Post as P
import Data.Aeson 
import GHC.Generics
import Data.Int (Int64)
import Server.App
import Servant.Server
import Data.Query
import Data.User
import Data.Connection

data DraftUpdate = DraftUpdate {
    draftId :: P.DraftId,
    draftContent :: [P.PostElement P.Draft] 
    } deriving Generic

instance FromJSON DraftUpdate where 
    parseJSON = withObject "Post update object" $ \e -> 
        DraftUpdate <$> e .: "id" <*> e.: "body"
instance ToJSON DraftUpdate where
    toJSON (DraftUpdate dId body) = object [
        "id" .= dId,
        "body" .= body
        ]


type DraftApi = "create" :> ReqBody '[JSON] [P.PostElement P.Draft] :> Post '[JSON] P.DraftId :<|>
    "update" :> ReqBody '[JSON] DraftUpdate :> PostNoContent '[JSON, PlainText, FormUrlEncoded] NoContent :<|>
    "publish" :> ReqBody '[JSON] P.DraftId :> PostNoContent '[JSON, PlainText, FormUrlEncoded] NoContent :<|>
    "all" :> Post '[JSON] [DraftUpdate] :<|>
    "createAndPublish" :> ReqBody '[JSON] [P.PostElement P.Draft] :> PostNoContent '[JSON, PlainText, FormUrlEncoded] NoContent

draftApi :: UserId -> ServerT DraftApi App
draftApi uId = 
    (fmap P.postId . runQnotFound . createDraft uId) :<|> updateDraftApi uId :<|> 
    publishDraftApi uId :<|>
    runQnotFound (fmap (map postToDraft) $ getDrafts uId) :<|>
    createAndPublish uId

updateDraftApi :: UserId -> DraftUpdate -> App NoContent
updateDraftApi uId (DraftUpdate dId els) = do 
    runQnotFound $ updateDraft uId dId els
    return NoContent

publishDraftApi :: UserId -> P.DraftId -> App NoContent
publishDraftApi uId dId = do 
    runQnotFound $ publishDraft uId dId
    return NoContent

postToDraft :: P.Draft -> DraftUpdate
postToDraft (P.PostData dId _ body) = DraftUpdate dId body

createAndPublish :: UserId -> [P.PostElement P.Draft] -> App NoContent
createAndPublish uId els = do
   dId <- fmap P.postId . runQnotFound . createDraft uId $ els
   publishDraftApi uId dId