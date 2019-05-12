{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    DeriveAnyClass #-}

module Data.User (
    User(..),
    UserId,
    UserEmail,
    UserUpdate(..)
) where

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Text (Text)
import Data.Aeson
import Servant.Docs (ToSample, toSamples, samples)
import Data.Proxy
import Data.Faculty

-- MARK: Documentation

instance (ToSample f) => ToSample (User f) where
    toSamples _ = samples $ [User 8 "Vasya" "Pupkinovuch" "Pupkin", User 69 "Seva" "Algebrovich" "Leonidov"] <*> (map snd $ toSamples Proxy) <*> ["foo@hse.ru"]

instance ToSample (UserUpdate) where
    toSamples _ = samples $ [UserUpdate "Vasya" "Pupkinovuch" "Pupkin", UserUpdate "Seva" "Algebrovich" "Leonidov"] <*> ["cs.hse.ru/dse/"]

-- MARK: Implementation

type UserId = Int64
type UserEmail = Text

data User f = User {
    userId :: UserId,
    firstName :: Text,
    middleName :: Text,
    lastName :: Text,
    userFaculty :: f,
    userEmail :: UserEmail
} deriving (GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance (ToJSON f) => ToJSON (User f) where
    toJSON (User uId fName mName lName faculty email) = object [
        "id" .= uId,
        "firstName" .= fName,
        "middleName" .= mName,
        "lastName" .= lName,
        "faculty" .= faculty,
        "email" .= email
        ]

instance (FromJSON f) => FromJSON (User f) where
    parseJSON = withObject "named channel" $ \e ->
        User <$> e .: "id" <*> e .: "firstName" <*> e .: "middleName" <*> e .: "lastName" <*> e .: "faculty" <*> e .: "email"

data UserUpdate = UserUpdate {
    userUpdateFirstName :: Text,
    userUpdateMiddleName :: Text,
    userUpdateLastName :: Text,
    userUpdateUserFaculty :: FacultyUrl
} deriving (GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance ToJSON UserUpdate where
    toJSON (UserUpdate fName mName lName faculty) = object [
        "firstName" .= fName,
        "middleName" .= mName,
        "lastName" .= lName,
        "faculty" .= faculty
        ]

instance FromJSON UserUpdate where
    parseJSON = withObject "user creation" $ \e ->
        UserUpdate <$> e .: "firstName" <*> e .: "middleName" <*> e .: "lastName" <*> e .: "faculty"

