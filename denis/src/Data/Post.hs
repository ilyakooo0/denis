{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}

module Data.Post (
    Post(..),
    PostElement,
    rowsToPost,
    PostId,
    QuoteId,
    PostRowResponse(..),
    PostCreation(..)
) where 

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

-- MARK: Documentation

instance ToSample Post where
    toSamples _ = samples $ [Post 8 17 time $ V.fromList ["hse", "cs", "machineLearning"]] <*> (map snd $ toSamples Proxy)
        where time = UTCTime (ModifiedJulianDay 1000) 8

instance ToSample PostCreation where
    toSamples _ = samples $ PostCreation <$> (map snd $ toSamples Proxy) <*> [(V.fromList ["hse", "cs", "machineLearning"])] 
        

-- MARK: Actual type 

type PostId = Int64
type QuoteId = Int64

data Post = Post {
    postId :: Int64,
    postAuthorId :: Int64,
    updated :: UTCTime,
    postTags :: V.Vector Text,
    postBody :: [PostElement Post]
}

instance ToJSON Post where
    toJSON (Post pId aId tm tags pb) = object [
        "id" .= pId,
        "authorId" .= aId,
        "updated" .= tm,
        "tags" .= tags,
        "body" .= pb ] 

data PostCreation = PostCreation {
    postCreationBody :: [PostElement Post],
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

data PostRowResponse = PostRowResponse {
    -- PostRow
    postRowId :: Int64,
    postRowAuthorId :: Int64,
    postRowUpdateTime :: UTCTime,
    postRowTags :: V.Vector Text,

    -- ElementRow
    rowElementOrd :: Int64,
    rowElementMarkdown :: Maybe Text,
    rowElementLatex :: Maybe Text,
    rowElementImage :: Maybe Text,
    rowElementQuote :: Maybe Int64,
    rowElementAttachment :: Maybe Text
} deriving (GHC.Generic)

instance SOP.Generic PostRowResponse
instance SOP.HasDatatypeInfo PostRowResponse

unRow :: MkElementRow x -> PostRowResponse -> x
unRow f' (PostRowResponse _ _ _ _ _ a b c d e) = f' a b c d e

rowsToPost :: [PostRowResponse] -> Maybe Post
rowsToPost [] = Nothing
rowsToPost (el:els) = if all (((==) `on` postRowId) el) els
    then Post (postRowId el) (postRowAuthorId el) (postRowUpdateTime el) (postRowTags el) <$> traverse (unRow rowsToElement) (el:els)
    else Nothing
