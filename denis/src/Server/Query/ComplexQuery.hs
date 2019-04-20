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
    TupleSections #-}

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

-- MARK: Documentation

instance (HasDocs api) => HasDocs (ComplexQuery :> api) where
    docsFor _ = docsFor (Proxy :: Proxy (ComplexApi :<|> api))

instance {-# Overlapping #-} ToSample (ComplexQueryRequest) where
    toSamples _ = samples $ [
        M.fromList [("posts/last", "8"), ("users/all", "")],
        M.fromList [("channels/","[{\"people\":[1],\"name\":\"newChannelName\",\"id\":7,\"tags\":[]},{\"people\":[1],\"name\":\"newChannelName\",\"id\":6,\"tags\":[]},{\"people\":[1],\"name\":\"newChannelName\",\"id\":5,\"tags\":[]},{\"people\":[1,1,2,2,2],\"name\":\"sssss\",\"id\":4,\"tags\":[\"thisIsHashTag\",\"thisIsHashTag\",\"thisIsHashTag\",\"thisIsHashTag\",\"thisIsAlsoHashTag\",\"cs\"]},{\"people\":[1,7,2,19],\"name\":\"ChannelName\",\"id\":1,\"tags\":[\"sos\"]}]"),("posts/last", "{\"users\":{\"1\":{\"middleName\":\"FirstUser\",\"lastName\":\"FirstUser\",\"firstName\":\"FirstUser\",\"id\":1}},\"response\":[{\"body\":[{\"markdown\":\"hello\"}],\"authorId\":1,\"id\":28,\"updated\":\"2019-04-13T16:14:38.495832Z\",\"tags\":[]}]}")]]

-- MARK: Implementation

data ComplexQuery


type ComplexQueryRequest = M.Map T.Text String
type ComplexQueryRepsonse =  M.Map T.Text String

type ComplexQueryDescription = Description "Complex query\nNOTE: both request and response samples are the same. The first one is the request, the second one is the response.\nAllows you to query several endpoints within one request.\nThe order of queries (both execution and response) is not determined. If one of the queries returns a non-2XX response, the first such response is returned.\n## Do not perform modifications in this query. You will have no way of telling whether it succeeded."

type ComplexApi = "complex" :> ComplexQueryDescription :> ReqBody '[JSON] (ComplexQueryRequest) :> Post '[JSON] (ComplexQueryRepsonse)

instance HasServer api context => HasServer (ComplexQuery :> api :: *) context where

    type ServerT (ComplexQuery :> api) m = ServerT api m

    hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy api)

    route _ context subserver = choice complex rest
        where
            rest = route (Proxy :: Proxy api) context subserver
            complexProxy = Proxy :: Proxy (ComplexApi)
            complexServer :: _ -> Request -> ComplexQueryRequest -> Handler (ComplexQueryRepsonse)
            complexServer env req query = do
                responses <- liftIO . newTVarIO $ Right M.empty
                recieved <- liftIO $ forM (M.toList query) $ uncurry $ \queryPath body -> do
                    newReq <- replaceBody (BSC.pack body) req
                    serverRest (replacePath queryPath newReq) $ respond responses queryPath
                recieved `seq` do
                    result <- liftIO $ readTVarIO responses
                    case result of
                        Left err -> throwError err
                        Right resp -> return resp
                where
                    serverRest = runRouterEnv (route (Proxy :: Proxy api) context subserver) env
                    respond :: TVar (Either ServantErr (M.Map T.Text String)) -> T.Text -> RouteResult Response -> IO ResponseReceived
                    respond responses queryPath = (>> return ResponseReceived) .
                        \case
                            (Fail err) -> atomically . writeTVar responses $ Left err
                            (FailFatal err) -> atomically . writeTVar responses $ Left err
                            (Route r) -> do
                                body <- BSC.unpack <$> responseBody r
                                atomically . modifyTVar responses $ fmap (M.insert queryPath body)
            complex = RawRouter $ \env req ->
                (flip runRouterEnv env $ route complexProxy context (emptyDelayed (Route $ complexServer env req))) req

-- ref: https://stackoverflow.com/questions/45467648/how-to-read-response-body-in-wai-middleware
responseBody :: Response -> IO ByteString
responseBody res =
  let (_,_,body) = responseToStream res in
  body $ \f -> do
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
