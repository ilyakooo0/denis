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
    getPosts,
    getUser,
    getUsers,
    getLastPosts,
    getPostsForUser,
    Limit,
    publishPost,
    PostResponse(..)
) where

import Squeal.PostgreSQL
import Data.User
import Data.Post
import Data.Schema
import Data.Function (on)
import Data.Connection
import Data.PostElement
import qualified Servant as S
import Control.Monad
import Control.Monad.Morph
import Data.Word (Word64)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Int (Int64)
import Data.ByteString.Char8 (pack)
import GHC.Generics (Generic)
import Data.Aeson
import Servant.Docs
import Data.Proxy

-- MARK: Documentation

instance ToSample PostResponse where
    toSamples _ = samples $ PostResponse <$> (map snd $ toSamples Proxy) <*> [M.fromList $ map (tupleFirst userId . snd) (toSamples Proxy)]


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
    #posts ! #postRowUpdateTime :*
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

getUsersQ :: [UserId] -> Maybe (Query Schema '[] (RowPG User))
getUsersQ uIds = case idsToColumn uIds of
    Just idQ -> Just $ selectDotStar #users $ from (subquery (idQ `As` #ids) & 
        innerJoin (table #users)
            (#ids ! #id .== #users ! #userId))
    Nothing -> Nothing

getPostsByIdQ :: [PostId] -> Maybe (Query Schema '[] (RowPG PostRowResponse))
getPostsByIdQ pIds = case idsToColumn pIds of
    Just idQ -> Just $ selectDotStar #posts $ from (subquery (idQ `As` #ids) & 
        innerJoin (subquery $ getPostQ id id `As` #posts)
            (#ids ! #id .== #posts ! #postRowId))
    Nothing -> Nothing


type Limit = Word64

getLastNPostsQ :: Limit -> Query Schema params (RowPG PostRowResponse)
getLastNPostsQ lim = getPostQ 
    (limit lim . orderBy [#posts ! #postRowUpdateTime & Desc]) id

getLastNPostsForUserQ :: Limit -> Query Schema (TuplePG (Only UserId)) (RowPG PostRowResponse)
getLastNPostsForUserQ lim = getPostQ 
    (limit lim . orderBy [#posts ! #postRowUpdateTime & Desc]) 
    (where_ $ #postRowAuthorId .== param @1)
    
createPostRowQ :: Manipulation Schema (TuplePG (Only UserId)) (RowPG (Only PostId))
createPostRowQ = insertRow #posts 
    (Default `as` #postRowId :*
     Set (param @1) `as` #postRowAuthorId :*
     Set currentTimestamp `as` #postRowUpdateTime) 
    OnConflictDoRaise 
    (Returning $ #postRowId `as` #fromOnly)

createPostElementRowsQ :: Manipulation Schema (TuplePG (PostElementRow Post)) '[]
createPostElementRowsQ = insertRow_ #postElements 
    (Set (param @1) `as` #rowElementId :*
    Set (param @2) `as` #rowElementOrd :*
    Set (param @3) `as` #rowElementMarkdown :*
    Set (param @4) `as` #rowElementLatex :*
    Set (param @5) `as` #rowElementImage :*
    Set (param @6) `as` #rowElementQuote :*
    Set (param @7) `as` #rowElementAttachment)


-- MARK: FrontEnd Data Structures

data PostResponse = PostResponse {
    posts :: [Post],
    users :: M.Map UserId User
} deriving (Generic)

instance ToJSON PostResponse


-- MARK: FrontEnd functions

getPostsForUser :: Word64 -> UserId -> StaticPQ (Maybe PostResponse)
getPostsForUser lim uId = do 
    postRows <- runQueryParams (getLastNPostsForUserQ lim) (Only uId) >>= getRows
    rowsToPosts postRows `liftMaybe` wrapIntoPostResponse

getPosts :: [PostId] -> StaticPQ (Maybe PostResponse)
getPosts pId = 
    case getPostsByIdQ pId of
        Just psQ -> do
            postRows <- runQuery psQ >>= getRows
            rowsToPosts postRows `liftMaybe` wrapIntoPostResponse
        Nothing -> return Nothing

getLastPosts :: Word64 -> StaticPQ (Maybe PostResponse)
getLastPosts n = do 
    postRows <- runQuery (getLastNPostsQ n) >>= getRows
    rowsToPosts postRows `liftMaybe` wrapIntoPostResponse
    
getUser :: UserId -> StaticPQ User
getUser uId = runQueryParams getUserQ (Only uId) >>= getRow 0

getUsers :: [UserId] -> StaticPQ (Maybe [User])
getUsers ids = getUsersQ ids `liftMaybe` (fmap Just . (runQuery >=> getRows))
           
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

userIdsFromPostResponse :: [Post] -> [UserId]
userIdsFromPostResponse = Set.toList . Set.fromList . map postAuthorId

rowsToPosts :: [PostRowResponse] -> Maybe [Post]
rowsToPosts = foldMaybe . map rowsToPost . L.groupBy ((==) `on` postRowId)

tupleFirst :: (a -> b) -> a -> (b, a)
tupleFirst f a = (f a, a)

mapFirstBy :: (a -> b) -> [a] -> [(b, a)]
mapFirstBy f = map $ tupleFirst f

idsToColumn :: [Int64] -> Maybe (Query schema params '["id" ::: 'NotNull 'PGint8])
idsToColumn [] = Nothing
idsToColumn (i:ii) = Just $ values (unsafeIntToExpr i `as` #id) $ map ((`as` #id) . unsafeIntToExpr) ii

unsafeIntToExpr :: Int64 -> Expression schema from grouping params ty
unsafeIntToExpr = UnsafeExpression . pack . show

wrapIntoPostResponse :: [Post] -> StaticPQ (Maybe PostResponse)
wrapIntoPostResponse ps = do
        pUsers <- getUsers $ userIdsFromPostResponse ps
        return $ PostResponse ps . M.fromList . mapFirstBy userId <$> pUsers 

liftMaybe :: Applicative f => Maybe a -> (a -> f (Maybe b)) -> f (Maybe b)
liftMaybe Nothing _ = pure Nothing
liftMaybe (Just a) f = f a