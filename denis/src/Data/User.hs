{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}

module Data.User (
    User(..),
    UserId
) where 

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Text (Text)
import Data.Aeson
import Servant.Docs (ToSample, toSamples, samples)

-- MARK: Documentation

instance ToSample User where
    toSamples _ = samples [User 8 "Vasya" "Pupkin", User 69 "Seva" "Leonidov"]

-- MARK: Implementation

type UserId = Int64

data User = User {
    userId :: UserId,
    firstName :: Text,
    secondName :: Text
} deriving (GHC.Generic)

instance SOP.Generic User

instance SOP.HasDatatypeInfo User

instance ToJSON User where
    toJSON (User uId fName sName) = object [
        "id" .= uId,
        "firstName" .= fName,
        "secondName" .= sName
        ]