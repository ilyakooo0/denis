{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    DeriveAnyClass #-}

module Data.Faculty where

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V
import Servant.Docs

instance ToSample Faculty where
    toSamples _ = singleSample $ Faculty "ПИ" "cs.hse.ru/bse" "ФКН -> ПИ" "Москва" "193729"
        (V.fromList ["Программирование", "Естсественные науки"]) "Настоящий адрес факультета"

-- |Адрес сайта департамента
type FacultyUrl = T.Text

-- |Объект департамента
data Faculty = Faculty {
    -- |Название департамента
    facultyName :: T.Text,
    -- |Адрес сайта департамента
    facultyUrl :: FacultyUrl,
    -- |Вспомогательный текст дапартамента с сайта
    facultyPath :: T.Text,
    -- |Название кампуса
    facultyCampusName :: T.Text,
    -- |Код кампуса департамента
    facultyCampusCode :: T.Text,
    -- |Теги департамента
    facultyTags :: V.Vector T.Text,
    -- |Адрес департамента
    facultyAddress :: T.Text
} deriving (GHC.Generic, Show, SOP.Generic, SOP.HasDatatypeInfo)

instance ToJSON Faculty where
    toJSON (Faculty name url p campusName campusCode tags address) = object [
        "name" .= name,
        "url" .= url,
        "path" .= p,
        "campusName" .= campusName,
        "campusCode" .= campusCode,
        "tags" .= tags,
        "address" .= address
        ]

instance FromJSON Faculty where
    parseJSON = withObject "faculty" $ \e ->
        Faculty <$>
            e .: "name" <*>
            e .: "url" <*>
            e .: "path" <*>
            e .: "campusName" <*>
            e .: "campusCode" <*>
            e .: "tags" <*>
            e .: "address"