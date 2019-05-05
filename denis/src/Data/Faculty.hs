{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}

module Data.Faculty (
    Faculty(..)
    ) where

import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Vector as V

data Faculty = Faculty {
    facultyName :: T.Text,
    facultyURL :: T.Text,
    facultyPath :: T.Text,
    facultyCampusName :: T.Text,
    facultyCampusCode :: T.Text,
    facultyTags :: V.Vector T.Text
} deriving (GHC.Generic, Show)

instance ToJSON Faculty where
    toJSON (Faculty name url path campusName campusCode tags) = object [
        "name" .= name,
        "url" .= url,
        "path" .= path,
        "campusName" .= campusName,
        "campusCode" .= campusCode,
        "tags" .= tags
        ]

instance SOP.Generic Faculty
instance SOP.HasDatatypeInfo Faculty

