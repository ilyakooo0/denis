{-# LANGUAGE
    FlexibleInstances,
    DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables,
    MultiParamTypeClasses,
    KindSignatures,
    TypeFamilies,
    LambdaCase,
    PartialTypeSignatures,
    TupleSections,
    DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server.Query.ComplexQuery (
    ComplexQuery
) where

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Char8 as BC
import Servant.Server
import Data.Proxy
import Servant.API
import Servant.Server.Internal
import qualified Data.Text as T
import GHC.Conc
import Network.Wai.Internal
import Control.Monad.IO.Class
import Control.Concurrent.STM.TVar
import Data.IORef (newIORef,modifyIORef',readIORef)
import Network.Wai
import Control.Monad
import Control.Monad.Error.Class
import Servant.Docs hiding (Response)
import qualified Data.Map as M
import Data.Text.Encoding (decodeUtf8)
import Data.Aeson
import GHC.Generics

-- MARK: Documentation

instance (HasDocs api) => HasDocs (ComplexQuery :> api) where
    docsFor _ = docsFor (Proxy :: Proxy (ComplexApi :<|> api))

instance {-# Overlapping #-} ToSample [ComplexQueryRequest] where
    toSamples _ = singleSample $ [ComplexQueryRequest "channels/" "", ComplexQueryRequest "posts/last" "3"]

instance {-# Overlapping #-} ToSample [ComplexQueryRepsonse] where
    toSamples _ = singleSample $ map ComplexQueryRepsonse ["[{\"people\":[1],\"name\":\"newChannelName\",\"id\":7,\"tags\":[]},{\"people\":[1],\"name\":\"newChannelName\",\"id\":6,\"tags\":[]},{\"people\":[1],\"name\":\"newChannelName\",\"id\":5,\"tags\":[]},{\"people\":[1,1,2,2,2],\"name\":\"sssss\",\"id\":4,\"tags\":[\"thisIsHashTag\",\"thisIsHashTag\",\"thisIsHashTag\",\"thisIsHashTag\",\"thisIsAlsoHashTag\",\"cs\"]},{\"people\":[1,7,2,19],\"name\":\"соытвототтоциология\",\"id\":1,\"tags\":[\"sos\"]}]","{\"users\":{\"1\":{\"middleName\":\"FirstUser\",\"lastName\":\"FirstUser\",\"firstName\":\"FirstUser\",\"id\":1}},\"response\":[{\"body\":[{\"markdown\":\"hello\"}],\"authorId\":1,\"id\":28,\"updated\":\"2019-04-13T16:14:38.495832Z\",\"tags\":[]},{\"body\":[{\"markdown\":\"Hello, blyat\"}],\"authorId\":1,\"id\":27,\"updated\":\"2005-01-03T12:34:56Z\",\"tags\":[]}]}"]

-- MARK: Implementation

data ComplexQuery


data ComplexQueryRequest = ComplexQueryRequest {
    url :: T.Text,
    body :: String
} deriving (Generic)

instance ToJSON ComplexQueryRequest
instance FromJSON ComplexQueryRequest

newtype ComplexQueryRepsonse = ComplexQueryRepsonse {responseText :: T.Text}

instance ToJSON ComplexQueryRepsonse where
    toJSON = toJSON . responseText

type ComplexQueryDescription = Description "Complex query\n\nAllows you to query several endpoints within one request.\n\nResponses are returned in the same order as the requests are sent.\n\nThe order of query execution is not determined. If one of the queries returns a non-2XX response, the first such response is returned.\n\n## Do not perform modifications in this query. You will have no way of telling whether it succeeded."

type ComplexApi = "complex" :> ComplexQueryDescription :> ReqBody '[JSON] ([ComplexQueryRequest]) :> Post '[JSON] [ComplexQueryRepsonse]

instance HasServer api context => HasServer (ComplexQuery :> api :: *) context where

    type ServerT (ComplexQuery :> api) m = ServerT api m

    hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

    route _ context subserver = choice complex rest
        where
            rest = route (Proxy :: Proxy api) context subserver
            complexProxy = Proxy :: Proxy (ComplexApi)
            complexServer :: _ -> Request -> [ComplexQueryRequest] -> Handler [ComplexQueryRepsonse]
            complexServer env req query = do
                responses <- liftIO . newTVarIO $ Right M.empty
                recieved <- liftIO $ forM (zip [0..] query) $ uncurry $ \queryIndex (ComplexQueryRequest queryPath queryBody) -> do
                    newReq <- replaceBody (BSC.pack queryBody) req
                    serverRest (replacePath queryPath newReq) $ respond responses queryIndex
                recieved `seq` do
                    result <- liftIO $ readTVarIO responses
                    case result of
                        Left err -> throwError err
                        Right resp -> sequence . map (fmap ComplexQueryRepsonse . throwMaybe . flip M.lookup resp) $ [0..((length query) - 1)]
                where
                    serverRest = runRouterEnv (route (Proxy :: Proxy api) context subserver) env
                    respond :: TVar (Either ServantErr (M.Map Int T.Text)) -> Int -> RouteResult Response -> IO ResponseReceived
                    respond responses queryIndex = (>> return ResponseReceived) .
                        \case
                            (Fail err) -> atomically . writeTVar responses $ Left err
                            (FailFatal err) -> atomically . writeTVar responses $ Left err
                            (Route r) -> do
                                rBody <- decodeUtf8 . BSC.toStrict <$> responseBody r
                                atomically . modifyTVar responses $ fmap (M.insert queryIndex rBody)
                    throwMaybe :: Maybe a -> Handler a
                    throwMaybe Nothing = throwError err500
                    throwMaybe (Just a) = return a
            complex = RawRouter $ \env req ->
                (flip runRouterEnv env $ route complexProxy context (emptyDelayed (Route $ complexServer env req))) req

-- ref: https://stackoverflow.com/questions/45467648/how-to-read-response-body-in-wai-middleware
responseBody :: Response -> IO ByteString
responseBody res =
  let (_,_,rBody) = responseToStream res in
  rBody $ \f -> do
    content <- newIORef mempty
    f (\chunk -> modifyIORef' content (<> chunk)) (return ())
    toLazyByteString <$> readIORef content

replaceBody :: ByteString -> Request -> IO Request
replaceBody newB resp = do
    t <- newTVarIO True
    return resp {
        requestBody = do
            v <- atomically $ do
                oldValue <- readTVar t
                writeTVar t False
                return oldValue
            return $ if v then BSC.toStrict newB else BC.empty
        }

replacePath :: T.Text -> Request -> Request
replacePath newPath req = req {
    pathInfo = T.splitOn "/" newPath}
