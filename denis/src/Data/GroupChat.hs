{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    TypeSynonymInstances,
    FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Data.GroupChat (
    GroupChat(..),
    GroupChatPermissions(..),
    GroupChatId,
    UserPermissions,
    GroupChatCreation(..),
    maxChatPermissions,
    minChatPermissions
    ) where

import Data.Aeson
import Data.Int (Int64)
import Data.User
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL.Schema
import Data.Function
import Data.Proxy
import Servant.Docs

instance ToSample GroupChat where
    toSamples _ = samples $ GroupChat <$>
        [5] <*>
        (Jsonb <$> map snd (toSamples Proxy)) <*>
        ["cocos", "kursach"]

instance ToSample GroupChatCreation where
    toSamples _ = samples $ GroupChatCreation <$>
        (Jsonb <$> map snd (toSamples Proxy)) <*>
        ["cocos", "kursach"]

instance ToSample UserPermissions where
    toSamples _ = singleSample $ M.fromList [(69, GroupChatPermissions True), (5051, GroupChatPermissions False)]

type GroupChatId = Int64
type UserPermissions = M.Map UserId GroupChatPermissions

data GroupChat = GroupChat {
    groupChatId :: GroupChatId,
    groupChatUsers :: Jsonb UserPermissions,
    groupChatName :: Text
} deriving (Show, GHC.Generic)

instance SOP.Generic GroupChat
instance SOP.HasDatatypeInfo GroupChat
instance ToJSON GroupChat where
    toJSON (GroupChat gId us n) = object [
        "id" .= gId,
        "users" .= getJsonb us,
        "name" .= n
        ]

instance FromJSON GroupChat where
    parseJSON = withObject "group chat" $ \e ->
        GroupChat <$> e .: "id" <*> (Jsonb <$> e .: "users") <*> e .: "name"

data GroupChatPermissions = GroupChatPermissions {
    -- canInvite :: Bool,
    -- canKick :: Bool,
    isAdmin :: Bool
} deriving (Show, GHC.Generic)

maxChatPermissions :: GroupChatPermissions
maxChatPermissions = GroupChatPermissions True -- True True
minChatPermissions :: GroupChatPermissions
minChatPermissions = GroupChatPermissions False

instance Eq GroupChatPermissions where
    (==) = (==) `on` isAdmin

instance Ord GroupChatPermissions where
    compare = compare `on` isAdmin

instance SOP.Generic GroupChatPermissions
instance SOP.HasDatatypeInfo GroupChatPermissions
instance ToJSON GroupChatPermissions
instance FromJSON GroupChatPermissions

data GroupChatCreation = GroupChatCreation {
    groupChatCreationUsers :: Jsonb UserPermissions,
    groupChatCreationName :: Text
} deriving (Show, GHC.Generic)

instance SOP.Generic GroupChatCreation
instance SOP.HasDatatypeInfo GroupChatCreation
instance ToJSON GroupChatCreation where
    toJSON (GroupChatCreation us n) = object [
        "users" .= getJsonb us,
        "name" .= n
        ]

instance FromJSON GroupChatCreation where
    parseJSON = withObject "group chat creation" $ \e ->
        GroupChatCreation <$> (Jsonb <$> e .: "users") <*> e .: "name"
