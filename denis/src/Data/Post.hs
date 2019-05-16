{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}

module Data.Post where

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import Servant.Docs (ToSample, toSamples, samples)
import Data.Proxy
import Data.PostElement
import Data.Text (Text)
import Data.Function (on)
import Data.Time.Clock
import Data.Time.Calendar
import qualified Data.Vector as V
import Data.Tags.Validation
import Data.Text.Validator

-- MARK: Documentation

instance ToSample Post where
    toSamples _ = samples $ [Post 8 17 time $ V.fromList ["hse", "cs", "machineLearning"]] <*> (map snd $ toSamples Proxy)
        where time = UTCTime (ModifiedJulianDay 1000) 8

instance ToSample PostCreation where
    toSamples _ = samples $ PostCreation <$> (map snd $ toSamples Proxy) <*> [(V.fromList ["hse", "cs", "machineLearning"])]

instance HasValidatableText PostCreation where
     validateText (PostCreation body tags) = all validateText body && all (validateText . (~= validateTag')) tags

-- MARK: Actual type

-- |Идентификатор поста.
type PostId = Int64
-- |Идентификатор цитаты.
type QuoteId = Int64

-- |Объект поста
data Post = Post {
    -- |Идентификатор поста.
    postId :: Int64,
    -- |Идентификатор автора поста.
    postAuthorId :: Int64,
    -- |Время последнего обновления поста.
    updated :: UTCTime,
    -- |Теги поста.
    postTags :: V.Vector Text,
    -- |Содержание поста.
    postBody :: [PostElement Post]
} deriving (Show)

instance ToJSON Post where
    toJSON (Post pId aId tm tags pb) = object [
        "id" .= pId,
        "authorId" .= aId,
        "updated" .= tm,
        "tags" .= tags,
        "body" .= pb ]

-- |Объект для создания поста
data PostCreation = PostCreation {
    -- |Содержание поста.
    postCreationBody :: [PostElement Post],
    -- |Теги поста.
    postCreationTags :: V.Vector Text
} deriving GHC.Generic

instance SOP.Generic PostCreation
instance SOP.HasDatatypeInfo PostCreation

instance ToJSON PostCreation where
    toJSON (PostCreation body tags) = object [
        "tags" .= tags,
        "body" .= body ]

instance FromJSON PostCreation where
    parseJSON = withObject "post creation" $ \e ->
        PostCreation <$> e .: "body" <*> e .: "tags"


-- MARK: Rows

-- Need to manually maintain. Future: https://github.com/morphismtech/squeal/issues/96
-- |Объект поста при ответе с базы данных.
data PostRowResponse = PostRowResponse {
    -- PostRow
    -- |Идентификатор поста.
    postRowId :: Int64,
    -- |Идентификатор автора поста.
    postRowAuthorId :: Int64,
    -- |Время последнего обновления поста.
    postRowUpdateTime :: UTCTime,
    -- |Теги поста.
    postRowTags :: V.Vector Text,

    -- ElementRow
    -- |Порядковый номер элемента поста.
    rowElementOrd :: Int64,
    -- |Markdown элемент поста.
    rowElementMarkdown :: Maybe Text,
    -- |Latex элемент поста.
    rowElementLatex :: Maybe Text,
    -- |Изображение.
    rowElementImage :: Maybe Text,
    -- |Цитата.
    rowElementQuote :: Maybe Int64,
    -- |Файл.
    rowElementAttachment :: Maybe Text
} deriving (GHC.Generic)

instance SOP.Generic PostRowResponse
instance SOP.HasDatatypeInfo PostRowResponse

-- |Функция, переводящая объект поста при ответе с базы данных в какой-то поста при помощи данного отображения.
unRow :: MkElementRow x -> PostRowResponse -> x
unRow f' (PostRowResponse _ _ _ _ _ a b c d e) = f' a b c d e

-- |Функция, переводящая объект поста при ответе с базы данных в обычный объект поста.
rowsToPost :: [PostRowResponse] -> Maybe Post
rowsToPost [] = Nothing
rowsToPost (el:els) = if all (((==) `on` postRowId) el) els
    then Post (postRowId el) (postRowAuthorId el) (postRowUpdateTime el) (postRowTags el) <$> traverse (unRow rowsToElement) (el:els)
    else Nothing
