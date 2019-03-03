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
    -- PostQuote(..),
    -- PostRow,
    -- PostQuoteRow,
    -- ElementRow,
    postToRows,
    -- postToElementRows,
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

-- MARK: Documentation

instance ToSample Post where
    toSamples _ = samples $ [Post 8 17] <*> (map snd $ toSamples Proxy)


-- MARK: Actual type 

type PostId = Int64
type QuoteId = Int64

data Post = Post {
    postId :: Int64,
    postAuthorId :: Int64,
    postBody :: [PostElement Post]
}

instance ToJSON Post where
    toJSON (Post pId aId pb) = object [
        "id" .= pId,
        "authorId" .= aId,
        "body" .= pb ] 


-- MARK: Rows

-- Need to manually maintain. Future: https://github.com/morphismtech/squeal/issues/96

data PostRowResponse = PostRowResponse {
    -- PostRow
    postRowId :: Int64,
    postRowAuthorId :: Int64,

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
unRow f' (PostRowResponse _ _ _ a b c d e) = f' a b c d e

postToRows :: Post -> [PostRowResponse]
postToRows (Post pId uId els) = map (\(el, f) -> elemenToRow f el) . zip els . map (PostRowResponse pId uId) $ [0..]

rowsToPost :: [PostRowResponse] -> Maybe Post
rowsToPost [] = Nothing
rowsToPost (el:els) = if all (((==) `on` postRowId) el) els
    then Post (postRowId el) (postRowAuthorId el) <$> traverse (unRow rowsToElement) (el:els)
    else Nothing
