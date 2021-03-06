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
main = do
    cfg <- getCfg

    hspec $ do
        let
            test :: Predicates -> IO ()
            test predicates = withServantServerAndContext serverProxy mockAuth
                (return $ hoistedServer (return "") cfg) $ \burl ->
                    serverSatisfies serverProxy burl testArgs
                        (printPredicate <%> predicates)

        it "Doesn't have errors" $
            test $ not500 <%> mempty

        it "Honours accept headers" $
            test $ honoursAcceptHeader <%> mempty

        it "Doesn't take longer than 1 second" $
            test $ notLongerThan (1 * seconds) <%> mempty

mockAuth :: Context (AuthHandler Request UserId ': '[])
mockAuth = mkAuthHandler (const $ return 3) :. EmptyContext

milliseconds :: Integer
milliseconds = 1000000

seconds :: Integer
seconds = 1000 * milliseconds

getCfg :: IO Config
getCfg = do
    cfg <- getConfig
    return cfg {
        mailConfig = mail
    }

mail :: MailConfig
mail = MailConfig {
    mailHost = "",
    mailPort = 25,
    mailUser = "",
    mailPassword = ""
}

testArgs :: Args
testArgs = Args
      { replay          = Nothing
      , maxSuccess      = 1000
      , maxDiscardRatio = 10
      , maxSize         = 100
      , chatty          = True
      , maxShrinks      = maxBound
      }

printPredicate = RequestPredicate $ \ req mgr -> do
    let url = HTTP.path req
    let (HTTP.RequestBodyLBS bs) = HTTP.requestBody req
    resp <- HTTP.httpLbs req mgr
    let code = statusCode $ HTTP.responseStatus resp
    putStr "\tRequest:\t"
    putStrLn $ B.unpack url
    putStrLn . filter (/= '\a') $ LB.unpack bs
    putStr "\tResponse:\t"
    print code
    putStrLn ""
    return []


instance (HasGenRequest a) => HasGenRequest (Authentication :> a) where
    genRequest (Proxy :: Proxy (Authentication :> a)) = genRequest (Proxy :: Proxy a)

instance HasGenRequest api => HasGenRequest (ComplexQuery :> api) where
    genRequest (Proxy :: Proxy (ComplexQuery :> api)) = genRequest (Proxy :: Proxy api)
