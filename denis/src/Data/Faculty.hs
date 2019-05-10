{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    DeriveAnyClass #-}

module Data.Faculty (
    Faculty(..),
    FacultyUrl
    ) where

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import Servant.Docs

instance ToSample Faculty where
    toSamples _ = singleSample $ Faculty "name" "cs.hse.ru/bse" "ФКН -> ПИ" "Москва" "193729"
        (V.fromList ["Программирование", "Естсественные науки"])

type FacultyUrl = T.Text

data Faculty = Faculty {
    facultyName :: T.Text,
    facultyUrl :: FacultyUrl,
    facultyPath :: T.Text,
    facultyCampusName :: T.Text,
    facultyCampusCode :: T.Text,
    facultyTags :: V.Vector T.Text
} deriving (GHC.Generic, Show, SOP.Generic, SOP.HasDatatypeInfo)

instance ToJSON Faculty where
    toJSON (Faculty name url p campusName campusCode tags) = object [
        "name" .= name,
        "url" .= url,
        "path" .= p,
        "campusName" .= campusName,
        "campusCode" .= campusCode,
        "tags" .= tags
        ]

instance FromJSON Faculty where
    parseJSON = withObject "faculty" $ \e ->
        Faculty <$>
            e .: "name" <*>
            e .: "url" <*>
            e .: "path" <*>
            e .: "campusName" <*>
            e .: "campusCode" <*>
            e .: "tags"