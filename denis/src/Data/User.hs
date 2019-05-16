{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    DeriveAnyClass,
    RecordWildCards #-}

module Data.User where

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Text (Text)
import Data.Aeson
import Servant.Docs (ToSample, toSamples, samples)
import Data.Proxy
import Data.Faculty
import Data.Limits
import Data.Text.Validator

-- MARK: Documentation

instance (ToSample f) => ToSample (User f) where
    toSamples _ = samples $ [User 8 "Vasya" "Pupkinovuch" "Pupkin"] <*> (map snd $ toSamples Proxy) <*> ["foo@hse.ru"]

instance ToSample (UserUpdate) where
    toSamples _ = samples $ [UserUpdate "Vasya" "Pupkinovuch" "Pupkin"] <*> ["cs.hse.ru/dse/"]

instance HasValidatableText UserUpdate where
    validateText UserUpdate{..} = all (validateText . (~< userFieldLengthLimit)) [userUpdateFirstName, userUpdateMiddleName, userUpdateLastName, userUpdateUserFaculty]

-- MARK: Implementation

-- |Идентификатор пользователя.
type UserId = Int64

-- |Адрес почты пользователя.
type UserEmail = Text

-- |Пользователь.
data User f = User {
    -- |Идентификатор.
    userId :: UserId,
    -- |Имя.
    firstName :: Text,
    -- |Фамилия.
    middleName :: Text,
    -- |Отчество.
    lastName :: Text,
    -- |Факультет.
    userFaculty :: f,
    -- |Адрес почты.
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

-- |Объект для обновления пользователя.
data UserUpdate = UserUpdate {
    -- |Имя.
    userUpdateFirstName :: Text,
    -- |Отчество.
    userUpdateMiddleName :: Text,
    -- |Фамилия.
    userUpdateLastName :: Text,
    -- |Факультет.
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

