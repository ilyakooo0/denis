{-# LANGUAGE
    OverloadedStrings,
    TypeOperators,
    DataKinds,
    FlexibleInstances,
    UndecidableInstances #-}

module Orphans where

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
import Test.QuickCheck
import Server.Query.Pagination
import Test.QuickCheck.Arbitrary.Generic
import Data.Post
import Data.Channel.NamedChannel
import Test.QuickCheck.Instances
import Server.Auth
import Data.Query
import Data.User
import Data.PostElement
import Data.Channel.AnonymousChannel
import Data.Channel.NamedChannel
import Data.GroupChat
import Squeal.PostgreSQL.Schema
import Data.Message

instance (Arbitrary i, Arbitrary r) => Arbitrary (PaginatingRequest i r) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PaginationDirection where
    arbitrary = elements [BackPagination, ForwardPagination]

instance (Arbitrary u) => Arbitrary (NamedChannel u) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AuthenticationCredits where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary UserCreation where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary TokenActivationData where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary UserUpdate where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary PostCreation where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary (PostElement p) where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary AnonymousChannel where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary NamedChannelCreationRequest where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GroupChat where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GroupChatPermissions where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary GroupChatCreation where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary MessageCreation where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance (Arbitrary a) => Arbitrary (Jsonb a) where
    arbitrary = genericArbitrary
    shrink = genericShrink
