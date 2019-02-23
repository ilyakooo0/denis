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
    postId,
    postAuthorId,
    postBody,
    PostElement(..),
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

    -- for docs only
    PostData(..),
    PostType(..)
) where 

import Data.Int (Int64)
-- import Squeal.PostgreSQL
-- import Squeal.PostgreSQL.Render
-- import qualified Squeal.PostgreSQL.PQ as P
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
-- import Squeal.PostgreSQL.Schema
import Data.Aeson
import qualified Data.Map.Lazy as M

import Data.PostElement
-- import Data.RowConvertable

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
postToElementRows (PostData pId _ pb) = elementsToRows pId pb

rowsToPost :: M.Map Int64 [ElementRow PostQuote] -> M.Map Int64 PostQuoteRow -> PostRow -> [ElementRow (PostData p)] -> Maybe (PostData p)
rowsToPost erqs pqrs (PostRow pId aId) elementRows = PostData pId aId <$> rowsToElements erqs pqrs elementRows

fromDraft :: PostId -> ElementRow Draft -> ElementRow Post
fromDraft pId (ElementRow _ b c d e f g) = ElementRow pId b c d e f g
                  
-- tupleFirst :: (a -> b) -> a -> (b, a)
-- tupleFirst f a = (f a, a)