{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators #-}

module Data.Query (
    Schema,
    createTables,
    getPost,
    getUser,
    createDraft,
    updateDraft,
    publishDraft,
    getDrafts,
    getLastPosts,
    getPostsForUser
) where

import Squeal.PostgreSQL
import Squeal.PostgreSQL.Expression
import Squeal.PostgreSQL.Schema
import Data.User
import Data.Post
import Data.Schema
import Data.List
import Data.Function (on)
import qualified Data.Map.Lazy as M
import Server.App
import Data.Connection
import Data.PostElement
import Control.Monad.Base
import Control.Monad ((<$!>))
import qualified Servant as S
import Control.Monad.Morph
import Data.Maybe
import Data.Word (Word64)


getQuoteRowsQ :: Query Schema (TuplePG (Only QuoteId)) (RowPG (ElementRow Post))
getQuoteRowsQ = UnsafeQuery "WITH RECURSIVE getQuote AS ( \
    \SELECT * FROM \"quoteElements\" quote WHERE \"rowElementId\" = $1 \
    \UNION ALL \
    \SELECT newQuote.* FROM \"quoteElements\" newQuote \
    \INNER JOIN getQuote q ON q.\"rowElementQuote\" = newQuote.\"rowElementId\" \
    \) \
    \SELECT * FROM getQuote"

getQuoteQ :: Query Schema (TuplePG (Only QuoteId)) (RowPG PostQuoteRow)
getQuoteQ = selectStar $ from (table #quotes) & where_ (#quoteRowId .== param @1)

-- getQuote :: QuoteId -> PQ Quote
-- getQuote qId = do
--     rows' <- runQuery getQuoteQ (Only qId) >>= getRows
--     qRow <- runQuery getQuoteQ (Only qId) >>= getRow 0
--     let rows = M.fromList $ map (tupleBy $ quoteRowId . head) . groupBy ((==) `on` quoteRowId) . sortBy (compare `on` quoteRowId) $ rows'
--     return $ M.lookup qId rows >>= rowsToQuote rows 

getUserQ :: Query Schema (TuplePG (Only UserId)) (RowPG User)
getUserQ = selectStar $ from (table #users) & where_ (#userId .== param @1)

getPostRowQ :: Query Schema (TuplePG (Only PostId)) (RowPG PostRow)
getPostRowQ = selectStar $ from (table #posts) & where_ (#postRowId .== param @1)

getDraftRowQ :: Query Schema (TuplePG (Only DraftId)) (RowPG PostRow)
getDraftRowQ = selectStar $ from (table #drafts) & where_ (#postRowId .== param @1)

getPostElementsQ :: Query Schema (TuplePG (Only PostId)) (RowPG (ElementRow Post))
getPostElementsQ = selectStar $ from (table #postElements) & where_ (#rowElementId .== param @1)

getDraftElementsQ :: Query Schema (TuplePG (Only DraftId)) (RowPG (ElementRow Draft))
getDraftElementsQ = selectStar $ from (table #draftElements) & where_ (#rowElementId .== param @1)

getPost :: PostId -> StaticPQ (Maybe Post)
getPost pId = do 
    postRow <- runQueryParams getPostRowQ (Only pId) >>= getRow 0
    postRowElements <- runQueryParams getPostElementsQ (Only pId) >>= getRows
    return $ rowsToPost M.empty M.empty postRow postRowElements

getLastPostRowsQ :: Word64 -> Query Schema '[] (RowPG (Only PostId))
getLastPostRowsQ n = select (#postRowId `as` #fromOnly) $ 
    (from $ table #posts) &
    orderBy [#postRowId & Desc] &
    limit n

getLastPosts :: Word64 -> StaticPQ [Post]
getLastPosts n = do 
    postIds <- runQuery (getLastPostRowsQ n) >>= getRows
    catMaybes <$> traverse getPost (map fromOnly postIds)

getUser :: UserId -> StaticPQ User
getUser uId = runQueryParams getUserQ (Only uId) >>= getRow 0


createDraftRowQ :: Manipulation Schema (TuplePG (Only UserId)) (RowPG (Only DraftId))
createDraftRowQ = insertRow #drafts 
    (Default `as` #postRowId :* Set (param @1) `as` #postRowAuthorId) 
    OnConflictDoRaise 
    (Returning $ #postRowId `as` #fromOnly)

createPostRowQ :: Manipulation Schema (TuplePG (Only UserId)) (RowPG (Only PostId))
createPostRowQ = insertRow #posts 
    (Default `as` #postRowId :* Set (param @1) `as` #postRowAuthorId) 
    OnConflictDoRaise 
    (Returning $ #postRowId `as` #fromOnly)

createDraftElementRowsQ :: Manipulation Schema (TuplePG (ElementRow Draft)) '[]
createDraftElementRowsQ = insertRow_ #draftElements 
    (Set (param @1) `as` #rowElementId :*
    Set (param @2) `as` #rowElementOrd :*
    Set (param @3) `as` #rowElementMarkdown :*
    Set (param @4) `as` #rowElementLatex :*
    Set (param @5) `as` #rowElementImage :*
    Set (param @6) `as` #rowElementQuote :*
    Set (param @7) `as` #rowElementAttachment)

createPostElementRowsQ :: Manipulation Schema (TuplePG (ElementRow Post)) '[]
createPostElementRowsQ = insertRow_ #postElements 
    (Set (param @1) `as` #rowElementId :*
    Set (param @2) `as` #rowElementOrd :*
    Set (param @3) `as` #rowElementMarkdown :*
    Set (param @4) `as` #rowElementLatex :*
    Set (param @5) `as` #rowElementImage :*
    Set (param @6) `as` #rowElementQuote :*
    Set (param @7) `as` #rowElementAttachment)

createDraft :: UserId -> [PostElement Draft] -> StaticPQ Draft
createDraft uId els = transactionally_ $ do
    dId' <- manipulateParams createDraftRowQ (Only uId)
    dId <- lift $ fromOnly <$> getRow 0 dId'
    traversePrepared_ createDraftElementRowsQ $ elementsToRows dId els
    return $ mkDraft dId uId els

getDraftsForUserQ :: Query Schema (TuplePG (Only UserId)) (RowPG (Only DraftId))
getDraftsForUserQ = select (#postRowId `as` #fromOnly) $
    (from $ table #drafts) &
    where_ (#postRowAuthorId .== param @1)
    
getPostsForUserLimitingQ :: Word64 -> Query Schema (TuplePG (Only UserId)) (RowPG (Only DraftId))
getPostsForUserLimitingQ lim = select (#postRowId `as` #fromOnly) $
    (from $ table #posts) &
    where_ (#postRowAuthorId .== param @1) &
    limit lim
    
getPostsForUser :: Word64 -> UserId -> StaticPQ [Post]
getPostsForUser lim uId = do 
    postIds <- runQueryParams (getPostsForUserLimitingQ lim) (Only uId) >>= getRows
    catMaybes <$> traverse getPost (map fromOnly postIds)
    

getDraft :: DraftId -> StaticPQ (Maybe Draft)
getDraft dId = do 
    draftRow <- runQueryParams getDraftRowQ (Only dId) >>= getRow 0
    draftRowElements <- runQueryParams getDraftElementsQ (Only dId) >>= getRows
    return $ rowsToPost M.empty M.empty draftRow draftRowElements

getDrafts :: UserId -> StaticPQ [Draft]
getDrafts uId = do 
    draftRow <- runQueryParams getDraftsForUserQ (Only uId) >>= getRows
    catMaybes <$> traverse getDraft (map fromOnly draftRow)

getDraftForUserWithIdQ :: Query Schema (TuplePG (UserId, DraftId)) (RowPG PostRow)
getDraftForUserWithIdQ = selectStar $
    (from $ table #drafts) &
    where_ (#postRowAuthorId .== param @1 .&& #postRowId .== param @2)

deleteElementsForIdQ :: Manipulation Schema (TuplePG (Only DraftId)) '[]
deleteElementsForIdQ = deleteFrom_ #draftElements (#rowElementId .== param @1)

deleteDraftQ :: Manipulation Schema (TuplePG (Only DraftId)) '[]
deleteDraftQ = deleteFrom_ #drafts (#postRowId .== param @1)

updateDraft :: UserId -> DraftId -> [PostElement Draft] -> StaticPQ ()
updateDraft uId dId els = transactionally_ $ do
    rs <- runQueryParams getDraftForUserWithIdQ (uId, dId) >>= getRows
    let c = length (rs :: [PostRow])
    if c /= 1 then
        lift $ S.throwError S.err404
        else do 
            manipulateParams deleteElementsForIdQ (Only dId)
            traversePrepared_ createDraftElementRowsQ $ elementsToRows dId els

getDraftElementRowsQ :: Query Schema (TuplePG (Only DraftId)) (RowPG (ElementRow Draft))
getDraftElementRowsQ = selectStar $
    (from $ table #draftElements) &
    where_ (#rowElementId .== param @1) &
    orderBy [#rowElementOrd & Asc]

publishDraft :: UserId -> DraftId -> StaticPQ ()
publishDraft uId dId = transactionally_ $ do
    rs <- runQueryParams getDraftForUserWithIdQ (uId, dId) >>= getRows
    let c = length (rs :: [PostRow])
    if c /= 1 then
        lift $ S.throwError S.err404
        else do 
            els <- runQueryParams getDraftElementRowsQ (Only dId) >>= getRows
            pId' <- manipulateParams createPostRowQ (Only uId)
            pId <- lift $ fromOnly <$> getRow 0 pId'
            traversePrepared_ createPostElementRowsQ $ map (fromDraft pId) els
            manipulateParams deleteDraftQ (Only dId)
            return ()
        

-- tupleFirst :: (a -> b) -> a -> (b, a)
-- tupleFirst f x = (f x, x)

