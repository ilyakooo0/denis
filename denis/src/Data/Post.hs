{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    KindSignatures #-}

module Data.Post (
    Post,
    PostElement,
    PostQuote(..),
    PostRow,
    PostQuoteRow,
    ElementRow,
    postToRow,
    postToElementRows,
    rowsToPost,
    PostId,
    QuoteId,
    Draft,
    DraftId,
    mkDraft,
    fromDraft,
    PostData(..)
) where 

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Aeson
import qualified Data.Map.Lazy as M
import Servant.Docs (ToSample, toSamples, samples)
import Data.Proxy

import Data.PostElement

-- MARK: Documentation

instance ToSample (PostData a) where
    toSamples _ = samples $ [PostData 8 17] <*> (map snd $ toSamples Proxy)


-- MARK: Actual type 

type PostId = Int64
type QuoteId = Int64
type DraftId = Int64

data PostType = Draft | NonDraft

data PostData (p :: PostType) = PostData {
    postId :: Int64,
    postAuthorId :: Int64,
    postBody :: [PostElement (PostData p)]
}

type Post = (PostData 'NonDraft)
type Draft = (PostData 'Draft)

mkDraft :: DraftId -> Int64 -> [PostElement Draft] -> Draft
mkDraft = PostData

instance ToJSON (PostData p) where
    toJSON (PostData pId aId pb) = object [
        "id" .= pId,
        "authorId" .= aId,
        "body" .= pb ] 


-- MARK: Rows

data PostRow = PostRow {
    postRowId :: Int64,
    postRowAuthorId :: Int64
} deriving (GHC.Generic)

instance SOP.Generic PostRow
instance SOP.HasDatatypeInfo PostRow

postToRow :: Post -> PostRow
postToRow (PostData pId aId _) = PostRow pId aId

postToElementRows :: PostData p -> [ElementRow (PostData p)]
postToElementRows (PostData pId uId pb) = elementsToRows pId uId pb

rowsToPost :: M.Map Int64 [ElementRow PostQuote] -> M.Map Int64 PostQuoteRow -> [ElementRow (PostData p)] -> Maybe (PostData p)
rowsToPost erqs pqrs [] = Nothing
rowsToPost erqs pqrs elementRows = let 
    pId = rowElementId . head $ elementRows
    uId = rowElementAuthorId . head $ elementRows
    in PostData pId uId <$> rowsToElements erqs pqrs elementRows

fromDraft :: PostId -> ElementRow Draft -> ElementRow Post
fromDraft pId (ElementRow _ uId b c d e f g) = ElementRow pId uId b c d e f g
                  
-- tupleFirst :: (a -> b) -> a -> (b, a)
-- tupleFirst f a = (f a, a)