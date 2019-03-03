{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Data.Query (
    Schema,
    createTables,
    getPost,
    getUser,
    getLastPosts,
    getPostsForUser,
    Limit,
    publishPost
) where

import Squeal.PostgreSQL
import Data.User
import Data.Post
import Data.Schema
import Data.Function (on)
import Data.Connection
import Data.PostElement
import qualified Servant as S
import Control.Monad.Morph
import Data.Word (Word64)
import qualified Data.List as L


-- MARK: Generalized Post Queries

type TableEndo schema params from grouping = TableExpression schema params from grouping ->
    TableExpression schema params from grouping

postsQ :: TableEndo Schema params _ 'Ungrouped -> Query Schema params _
postsQ te = selectStar $ te (from $ table #posts)

postViewT :: TableEndo Schema params _ 'Ungrouped -> TableExpression Schema params _ 'Ungrouped
postViewT te = from $ subquery (postsQ te `As` #posts) & 
    innerJoin (table #postElements)
    (#posts ! #postRowId .== #postElements ! #rowElementId)

getPostQ :: TableEndo Schema params _ 'Ungrouped -> TableEndo Schema params _ 'Ungrouped -> Query Schema params (RowPG PostRowResponse)
getPostQ oTe te = select (
    #posts ! #postRowId :*
    #posts ! #postRowAuthorId :*
    --  #rowElementId :*
    #postElements ! #rowElementOrd :*
    #postElements ! #rowElementMarkdown :*
    #postElements ! #rowElementLatex :*
    #postElements ! #rowElementImage :*
    #postElements ! #rowElementQuote :*
    #postElements ! #rowElementAttachment)  
    (oTe $ postViewT te)


-- MARK: Concrete Queries

-- getQuoteRowsQ :: Query Schema (TuplePG (Only QuoteId)) (RowPG (ElementRow Post))
-- getQuoteRowsQ = UnsafeQuery "WITH RECURSIVE getQuote AS ( \
--     \SELECT * FROM \"quoteElements\" quote WHERE \"rowElementId\" = $1 \
--     \UNION ALL \
--     \SELECT newQuote.* FROM \"quoteElements\" newQuote \
--     \INNER JOIN getQuote q ON q.\"rowElementQuote\" = newQuote.\"rowElementId\" \
--     \) \
--     \SELECT * FROM getQuote"

-- getQuoteQ :: Query Schema (TuplePG (Only QuoteId)) (RowPG PostQuoteRow)
-- getQuoteQ = selectStar $ from (table #quotes) & where_ (#quoteRowId .== param @1)

-- getQuote :: QuoteId -> PQ Quote
-- getQuote qId = do
--     rows' <- runQuery getQuoteQ (Only qId) >>= getRows
--     qRow <- runQuery getQuoteQ (Only qId) >>= getRow 0
--     let rows = M.fromList $ map (tupleBy $ quoteRowId . head) . groupBy ((==) `on` quoteRowId) . sortBy (compare `on` quoteRowId) $ rows'
--     return $ M.lookup qId rows >>= rowsToQuote rows 

getUserQ :: Query Schema (TuplePG (Only UserId)) (RowPG User)
getUserQ = selectStar $ from (table #users) & where_ (#userId .== param @1)

getPostByIdQ :: Query Schema (TuplePG (Only PostId)) (RowPG PostRowResponse)
getPostByIdQ = getPostQ id $ where_ $ #postRowId .== param @1

type Limit = Word64

getLastNPostsQ :: Limit -> Query Schema params (RowPG PostRowResponse)
getLastNPostsQ lim = getPostQ 
    (limit lim . orderBy [#posts ! #postRowId & Desc]) id

getLastNPostsForUserQ :: Limit -> Query Schema (TuplePG (Only UserId)) (RowPG PostRowResponse)
getLastNPostsForUserQ lim = getPostQ 
    (limit lim . orderBy [#posts ! #postRowId & Desc]) 
    (where_ $ #postRowAuthorId .== param @1)
    
createPostRowQ :: Manipulation Schema (TuplePG (Only UserId)) (RowPG (Only PostId))
createPostRowQ = insertRow #posts 
    (Default `as` #postRowId :* Set (param @1) `as` #postRowAuthorId) 
    OnConflictDoRaise 
    (Returning $ #postRowId `as` #fromOnly)

createPostElementRowsQ :: Manipulation Schema (TuplePG (PostElementRow Post)) '[]
createPostElementRowsQ = insertRow_ #postElements 
    (Set (param @1) `as` #rowElementId :*
    Set (param @2) `as` #rowElementAuthorId :*
    Set (param @3) `as` #rowElementOrd :*
    Set (param @4) `as` #rowElementMarkdown :*
    Set (param @5) `as` #rowElementLatex :*
    Set (param @6) `as` #rowElementImage :*
    Set (param @7) `as` #rowElementQuote :*
    Set (param @8) `as` #rowElementAttachment)

-- MARK: FrontEnd functions

getPostsForUser :: Word64 -> UserId -> StaticPQ (Maybe [Post])
getPostsForUser lim uId = do 
    postRows <- runQueryParams (getLastNPostsForUserQ lim) (Only uId) >>= getRows
    return . foldMaybe . map rowsToPost . L.groupBy ((==) `on` postRowId) $ postRows    

getPost :: PostId -> StaticPQ (Maybe Post)
getPost pId = do 
    postRow <- runQueryParams getPostByIdQ (Only pId) >>= getRows
    return $ rowsToPost postRow

getLastPosts :: Word64 -> StaticPQ (Maybe [Post])
getLastPosts n = do 
    postRows <- runQuery (getLastNPostsQ n) >>= getRows
    return . foldMaybe . map rowsToPost . L.groupBy ((==) `on` postRowId) $ postRows

getUser :: UserId -> StaticPQ User
getUser uId = runQueryParams getUserQ (Only uId) >>= getRow 0
   
publishPost :: UserId -> [PostElement Post] -> StaticPQ PostId
publishPost uId els = transactionally_ $ do
    pId' <- manipulateParams createPostRowQ (Only uId) >>= firstRow
    case pId' of
        (Just (Only pId)) -> do
            traversePrepared_ createPostElementRowsQ $ elementsToRows pId els
            return pId
        _ -> lift $ S.throwError S.err404

-- MARK: Utils

foldMaybe :: [Maybe a] -> Maybe [a]
foldMaybe [] = Just []
foldMaybe (Nothing : _) = Nothing
foldMaybe (Just x : xs) = (x :) <$> foldMaybe xs