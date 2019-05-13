{-# LANGUAGE
    OverloadedStrings,
    TypeOperators,
    DataKinds,
    FlexibleInstances,
    UndecidableInstances,
    ScopedTypeVariables #-}

import Server.API
import Server.Server
import Test.QuickCheck
import Servant.QuickCheck
import Test.Hspec
import Server.App
import Servant.Server
import Data.User
import Servant.Server.Experimental.Auth
import Network.Wai
import Servant.QuickCheck.Internal.HasGenRequest
import Servant.API
import Server.Query.ComplexQuery
import Orphans
import qualified Network.HTTP.Client as HTTP
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Network.HTTP.Types.Status (statusCode)

main :: IO ()
main = hspec goodAPISpec

mockAuth :: Context (AuthHandler Request UserId ': '[])
mockAuth = mkAuthHandler (const $ return 3) :. EmptyContext

goodAPISpec = describe "my server" $ do

  it "follows best practices" $ do
    cfg' <- getConfig
    withServantServerAndContext serverProxy mockAuth (return $ hoistedServer (return "") cfg') $ \burl ->
        serverSatisfies serverProxy burl stdArgs{chatty=True} (
                                            printPredicate
                                            <%> not500
                                            <%> mempty)

printPredicate = RequestPredicate $ \ req mgr -> do
    let url = HTTP.path req
    let (HTTP.RequestBodyLBS bs) = HTTP.requestBody req
    resp <- HTTP.httpLbs req mgr
    let code = statusCode $ HTTP.responseStatus resp
    putStrLn . ("\t\t" <>) $ B.unpack url
    putStrLn . filter (/= '\a') $ LB.unpack bs
    print code
    putStrLn ""
    return []


instance (HasGenRequest a) => HasGenRequest (Authentication :> a) where
    genRequest (Proxy :: Proxy (Authentication :> a)) = genRequest (Proxy :: Proxy a)

instance HasGenRequest api => HasGenRequest (ComplexQuery :> api) where
    genRequest (Proxy :: Proxy (ComplexQuery :> api)) = genRequest (Proxy :: Proxy api)
