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

-- MARK: Documentation

instance ToSample Post where
    toSamples _ = samples $ [Post 8 17 time] <*> (map snd $ toSamples Proxy)
        where time = UTCTime (ModifiedJulianDay 1000) 8


-- MARK: Actual type 

type PostId = Int64
type QuoteId = Int64

data Post = Post {
    postId :: Int64,
    postAuthorId :: Int64,
    updated :: UTCTime,
    postBody :: [PostElement Post]
}

instance ToJSON Post where
    toJSON (Post pId aId tm pb) = object [
        "id" .= pId,
        "authorId" .= aId,
        "updated" .= tm,
        "body" .= pb ] 


-- MARK: Rows

-- Need to manually maintain. Future: https://github.com/morphismtech/squeal/issues/96

data PostRowResponse = PostRowResponse {
    -- PostRow
    postRowId :: Int64,
    postRowAuthorId :: Int64,
    postRowUpdateTime :: UTCTime,

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
unRow f' (PostRowResponse _ _ _ _ a b c d e) = f' a b c d e

rowsToPost :: [PostRowResponse] -> Maybe Post
rowsToPost [] = Nothing
rowsToPost (el:els) = if all (((==) `on` postRowId) el) els
    then Post (postRowId el) (postRowAuthorId el) (postRowUpdateTime el) <$> traverse (unRow rowsToElement) (el:els)
    else Nothing
