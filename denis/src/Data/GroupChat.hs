{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    TypeSynonymInstances,
    FlexibleInstances,
    RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}


module Data.GroupChat where

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
import Data.Limits
import Data.Text.Validator

instance ToSample GroupChat where
    toSamples _ = samples $ GroupChat <$>
        [5] <*>
        (Jsonb <$> map snd (toSamples Proxy)) <*>
        ["cocos", "kursach"]

instance ToSample GroupChatCreation where
    toSamples _ = samples $ GroupChatCreation <$>
        (Jsonb <$> map snd (toSamples Proxy)) <*>
        ["cocos", "kursach"]

instance HasValidatableText GroupChat where
    validateText GroupChat{..} = validateText $ groupChatName ~< maxGroupChatName

instance HasValidatableText GroupChatCreation where
    validateText GroupChatCreation{..} = validateText $ groupChatCreationName ~< maxGroupChatName

instance ToSample UserPermissions where
    toSamples _ = singleSample $ M.fromList [(69, GroupChatPermissions True), (5051, GroupChatPermissions False)]

-- |Идентификатор групповой беседы.
type GroupChatId = Int64
-- |Права участников групповой беседы.
type UserPermissions = M.Map UserId GroupChatPermissions

-- |Объект групповой беседы.
data GroupChat = GroupChat {
    -- |Идентификатор групповой беседы.
    groupChatId :: GroupChatId,
    -- |Права участников беседы.
    groupChatUsers :: Jsonb UserPermissions,
    -- |Имя групповой беседы.
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

-- |Права участника беседы
data GroupChatPermissions = GroupChatPermissions {
    -- canInvite :: Bool,
    -- canKick :: Bool,
    -- |Является ли данный участник администратором
    isAdmin :: Bool
} deriving (Show, GHC.Generic)

-- |Максимально возможные права в групповой беседе.
maxChatPermissions :: GroupChatPermissions
maxChatPermissions = GroupChatPermissions True -- True True
-- |Минимально возможные права в групповой беседе.
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

-- |Объект для создания групповой беседы
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
