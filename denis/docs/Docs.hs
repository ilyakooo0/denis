{-# LANGUAGE DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables,
    FlexibleInstances #-}

import Server.API
import Servant.Docs
import qualified Data.Text as T
import Servant.Server
import Servant
import Server.Auth
import Data.Aeson
import Data.PostElement
import Data.Proxy
import Data.User
import Data.Int (Int64)
import Data.Word (Word64)
import Server.API.Posts
import Server.Logger
import Server.Auth
import Server.API.Channels
import Data.Aeson
import qualified Data.Text.IO as T
import Servant.Server.Experimental.Auth
import Network.Wai


docsWriter :: HasDocs api => String -> Proxy api -> IO ()
docsWriter file = writeFile ("docs/" ++ file ++ ".md") . normalizer . markdown . docs

main = do
    docsWriter "posts" $ pretty (Proxy :: Proxy ("posts" :> PostApi))
    docsWriter "all" $ pretty serverProxy
    docsWriter "authentication" $ pretty (Proxy :: Proxy ("authentication" :> AuthenticationHandler :<|>
        Authentication :> "authentication" :> "me" :> Post '[JSON] UserId ))
    docsWriter "channels" $ pretty (Proxy :: Proxy ("channels" :> ChannelsApi))
    T.writeFile ("docs/Home.md") . (\t -> "```\n" <> t <> "```\n") $ layoutWithContext serverProxy (undefined :: Context (AuthHandler Request UserId ': '[]))


instance ToSample Char where
    toSamples _ = singleSample 'c'

instance ToSample Int64 where
    toSamples _ = samples [1, 7, 2, 19]

instance ToSample Word64 where
    toSamples _ = samples [1, 7, 2, 19]

instance ToSample Log where
    toSamples _ = noSamples

normalizer :: String -> String
normalizer (' ':' ':' ':' ':'`':'`':'`':xs) = "```" ++ normalizer xs
normalizer (x:xs) = x : normalizer xs
normalizer "" = ""