{-# LANGUAGE DataKinds,
    TypeOperators,
    OverloadedStrings,
    ScopedTypeVariables,
    FlexibleInstances #-}

import Server.API
import Servant.Docs
import Servant.API
import Data.Post
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
import Server.API.Draft
import Server.Logger

main = writeFile "docs.md" . markdown . docs . pretty $ serverProxy 

instance ToSample Char where
    toSamples _ = singleSample $ 'c'
    
instance ToSample Int64 where
    toSamples _ = samples [1, 7, 2, 19]

instance ToSample Word64 where
    toSamples _ = samples [1, 7, 2, 19]
                    
instance ToSample Log where
    toSamples _ = samples ["Log"]
