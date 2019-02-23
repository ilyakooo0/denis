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
import Control.Lens
import Data.Aeson
import Data.PostElement
import Data.Proxy
import Data.User
import Data.Int (Int64)
import Data.Word (Word64)
import Server.API.Posts
import Server.API.Draft

main = writeFile "docs.md" . markdown . docs . pretty $ serverProxy 

instance ToSample AuthenticationCredits where
    toSamples _ = samples $ map AuthenticationCredits [56, 103, 8]

instance ToSample Char where
    toSamples _ = singleSample $ 'c'

instance HasDocs api => HasDocs ((AuthProtect "basicAuth") :> api) where
    docsFor Proxy (endpoint, action) =
        docsFor (Proxy :: Proxy api) (endpoint, action & notes <>~ [DocNote "Authentication" ["This method requires cookies set in the `POST /authenticate` method.\n\nReturns error `401 Unauthorized` if the cookies are invalid or don't premit access to the requested data."]])
    
instance ToSample (PostData a) where
    toSamples _ = samples $ [PostData 8 17] <*> (map snd $ toSamples Proxy)

instance ToSample (PostElement p) where
    toSamples _ = samples [
        Markdown "# This is a markdown title\nThis is body.",
        Markdown "## This is a subtitle\n_hello._"
        ]

instance ToSample Int64 where
    toSamples _ = samples [1, 7, 2, 19]

instance ToSample Word64 where
    toSamples _ = samples [1, 7, 2, 19]
                    
instance ToSample User where
    toSamples _ = samples [User 8 "Vasya" "Pupkin", User 69 "Seva" "Leonidov"]

instance ToSample UserPostsRequest where
    toSamples _ = samples [UserPostsRequest 8 10, UserPostsRequest 42 20]

instance ToSample DraftUpdate where
    toSamples _ = samples $ [DraftUpdate 8] <*> (map snd $ toSamples Proxy)