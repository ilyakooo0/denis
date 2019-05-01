{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    PartialTypeSignatures,
    FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Data.Query (
    Schema,
    createTables,
    getPosts,
    getUser,
    getUsers,
    getAllUsers,
    getLastPosts,
    getPostsForUser,
    Limit,
    publishPost,
    getChannelPosts,
    updateChannel,
    createNewChannel,
    getAllChannels,
    getAnonymousChannelPosts,
    deleteNamedChannel,
    HasUsers(..),
    wrapIntoUsers,
    ResponseWithUsers(..),
    getTags
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
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Int (Int64)
import Data.ByteString.Char8 (pack)
import GHC.Generics (Generic)
import Data.Aeson hiding (Null)
import Servant.Docs
import Data.Proxy
import Data.Vector (Vector, toList, fromList)
import Data.Text (Text)
import Data.Channel.NamedChannel
import Data.Channel.AnonymousChannel
import Data.Maybe (fromMaybe)
import Squeal.PostgreSQL.Render

-- MARK: Documentation

instance (ToSample r) => ToSample (ResponseWithUsers r) where
    toSamples _ = samples $ ResponseWithUsers <$> (map snd $ toSamples Proxy) <*> [M.fromList $ map (tupleFirst userId . snd) (toSamples Proxy)]


-- MARK: Generalized Post Queries

type TableEndo schema params from grouping = TableExpression schema params from grouping ->
    TableExpression schema params from grouping

postsQ :: TableEndo Schema params _ 'Ungrouped -> Query Schema params _
postsQ te = selectStar $ te (from $ table #posts)

postViewT :: TableEndo Schema params _ 'Ungrouped -> TableExpression Schema params _ 'Ungrouped
postViewT te = from $ subquery (postsQ te `As` #posts) &
    innerJoin (table #postElements)
    (#posts ! #postRowId .== #postElements ! #rowElementId)

getPostQ
    :: TableEndo Schema params _ 'Ungrouped
    -- ^ PostRowResponse endo
    -> TableEndo Schema params _ 'Ungrouped
    -- ^ PostRow endo
    -> Query Schema params (RowPG PostRowResponse)
getPostQ oTe te = select (
    #posts ! #postRowId :*
    #posts ! #postRowAuthorId :*
    #posts ! #postRowUpdateTime :*
    #posts ! #postRowTags :*
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

verifiedUserIdsQ :: [UserId] -> Maybe (Query Schema '[] (RowPG (Only UserId)))
verifiedUserIdsQ uIds = case idsToColumn uIds of
    Just idQ -> Just $ select (#users ! #userId `as` #fromOnly) $ from (subquery (idQ `As` #ids) &
        innerJoin (table #users)
            (#ids ! #id .== #users ! #userId))
    Nothing -> Nothing


getUsersQ :: [UserId] -> Maybe (Query Schema '[] (RowPG User))
getUsersQ uIds = case idsToColumn uIds of
    Just idQ -> Just $ selectDotStar #users $ from (subquery (idQ `As` #ids) &
        innerJoin (table #users)
            (#ids ! #id .== #users ! #userId))
    Nothing -> Nothing

getAllUsersQ :: Query Schema '[] (RowPG User)
getAllUsersQ = selectStar $ from $ table #users

getPostsByIdQ :: [PostId] -> Maybe (Query Schema '[] (RowPG PostRowResponse))
getPostsByIdQ pIds = case idsToColumn pIds of
    Just idQ -> Just $ selectDotStar #posts $ from (subquery (idQ `As` #ids) &
        innerJoin (subquery $ getPostQ (orderBy [#postElements ! #rowElementOrd & Asc] . orderBy [#posts ! #postRowId & Desc]) id `As` #posts)
            (#ids ! #id .== #posts ! #postRowId))
    Nothing -> Nothing

type Limit = Word64

isNotFalse
  :: Expression schema from grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition schema from grouping params
isNotFalse x = UnsafeExpression $ renderExpression x <+> "IS NOT FALSE"

getLastPostsQ :: TableEndo _ _ _ _ -> Limit -> Query Schema ('Null (PG Int64) ': params) (RowPG PostRowResponse)
getLastPostsQ te lim = getPostQ
    (orderBy [#postElements ! #rowElementOrd & Asc] . orderBy [#posts ! #postRowId & Desc])
    (limit lim . te . where_ (isNotFalse $ #posts ! #postRowId .< param @1) . orderBy [#posts ! #postRowId & Desc])

getLastNPostsForUserQ :: Limit -> Query Schema (TuplePG (Maybe PostId, UserId)) (RowPG PostRowResponse)
getLastNPostsForUserQ = getLastPostsQ $ where_ (#posts ! #postRowAuthorId .== param @2)

createPostRowQ :: Manipulation Schema (TuplePG (UserId, Vector Text)) (RowPG (Only PostId))
createPostRowQ = insertRow #posts
    (Default `as` #postRowId :*
     Set (param @1) `as` #postRowAuthorId :*
     Set currentTimestamp `as` #postRowUpdateTime :*
     Set (param @2) `as` #postRowTags)
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

getChannelForUserQ :: Query Schema (TuplePG (UserId, NamedChannelId)) (RowPG NamedChannelFull)
getChannelForUserQ = selectStar $
    from (table #channels) &
    where_ (#namedChannelFullOwner .== param @1 .&& #namedChannelFullId .== param @2)

getAllChannelsForUserQ :: Query Schema (TuplePG (Only UserId)) (RowPG NamedChannelFull)
getAllChannelsForUserQ = selectStar $
    from (table #channels) &
    where_ (#namedChannelFullOwner .== param @1) &
    orderBy [#namedChannelFullId & Desc]

createNamedChannelQ :: Manipulation Schema (TuplePG NamedChannelCreation) (RowPG (Only NamedChannelId))
createNamedChannelQ = insertRow #channels
    (
        Default `as` #namedChannelFullId :*
        Set (param @1) `as` #namedChannelFullOwner :*
        Set (param @2) `as` #namedChannelFullName :*
        Set (param @3) `as` #namedChannelFullTags :*
        Set (param @4) `as` #namedChannelFullPeopleIds
    )
    OnConflictDoRaise
    (Returning $ #namedChannelFullId `as` #fromOnly)

deleteNamedChannelQ :: Manipulation Schema (TuplePG (Only NamedChannelId)) '[]
deleteNamedChannelQ = deleteFrom_ #channels (#namedChannelFullId .== param @1)

updateNamedChannelQ :: Manipulation Schema (TuplePG NamedChannelFull) '[]
updateNamedChannelQ = update_ #channels
    (
        Same `as` #namedChannelFullId :*
        Same `as` #namedChannelFullOwner :*
        Set (param @3) `as` #namedChannelFullName :*
        Set (param @4) `as` #namedChannelFullTags :*
        Set (param @5) `as` #namedChannelFullPeopleIds
    )
    (#namedChannelFullOwner .== param @2 .&& #namedChannelFullId .== param @1)

getChannelPostsQ
    :: Limit
    -> [UserId]
    -> Vector Text
    -> Query
        Schema
        (TuplePG (Only (Maybe PostId))) -- ^ post id from which to start
        (RowPG PostRowResponse)
getChannelPostsQ lim uIds tags = selectStar $ from (subquery $ postsQ' `As` #bar) &
    where_ ( isNotFalse $ #postRowId .< param @1) &
    orderBy [#rowElementOrd & Asc] & limit lim . orderBy [#postRowId & Desc]
    where
        unionWithUsers = case idsToColumn uIds of
            Just uIdsQ -> selectDotStar #posts' (from $ subquery (uIdsQ `As` #uIds) &
                    innerJoin (subquery $ getPostQ id id `As` #posts')
                        (#uIds ! #id .== #posts' ! #postRowAuthorId)) &
                    union
            Nothing -> id
        postsQ' = unionWithUsers $ getPostQ id $ where_ $ jsonbLit tags .?| #postRowTags

getTagsQ :: Query Schema '[] (RowPG (Only String))
getTagsQ = selectDistinct (unsafeFunction "unnest" #postRowTags `as` #fromOnly) $
    from (table #posts)

-- MARK: FrontEnd Data Structures

data NamedChannelCreation = NamedChannelCreation {
    namedChannelCreationOwner :: UserId,
    namedChannelCreationName :: Text,
    namedChannelCreationTags :: Vector Text,
    namedChannelCreationPeopleIds :: Vector UserId
} deriving GHC.Generic

instance SOP.Generic NamedChannelCreation
instance SOP.HasDatatypeInfo NamedChannelCreation

addUserToChannelCreate :: UserId -> NamedChannelCreationRequest -> NamedChannelCreation
addUserToChannelCreate uId (NamedChannelCreationRequest cName cTags cPeople) = NamedChannelCreation uId cName cTags cPeople

data NamedChannelFull = NamedChannelFull {
    namedChannelFullId :: NamedChannelId,
    namedChannelFullOwner :: UserId,
    namedChannelFullName :: Text,
    namedChannelFullTags :: Vector Text,
    namedChannelFullPeopleIds :: Vector UserId
} deriving GHC.Generic

instance SOP.Generic NamedChannelFull
instance SOP.HasDatatypeInfo NamedChannelFull

addUserToChannelUpdate :: UserId -> NamedChannel UserId -> NamedChannelFull
addUserToChannelUpdate uId (NamedChannel cId cName cTags cPeople) = NamedChannelFull cId uId cName cTags cPeople

removeUserFromChannel :: NamedChannelFull -> NamedChannel UserId
removeUserFromChannel (NamedChannelFull cId _ cName cTags cPeople) = NamedChannel cId cName cTags cPeople

getChannelForUser :: UserId -> NamedChannelId -> StaticPQ NamedChannelFull
getChannelForUser uId cId = do
    channel <- runQueryParams getChannelForUserQ (uId, cId) >>= firstRow
    case (channel :: Maybe NamedChannelFull) of
        (Just c) -> return c
        Nothing -> lift $ S.throwError S.err404

-- TODO: Reject bad requests. Should be binary.
verifiedUsers :: [UserId] -> StaticPQ [UserId]
verifiedUsers uIds =
    case verifiedUserIdsQ uIds of
        Just uIdsQ -> fmap fromOnly <$> (runQuery uIdsQ >>= getRows)
        Nothing -> return []

-- MARK: FrontEnd functions

-- Nothing <=> no user
getPostsForUser :: Maybe PostId -> Limit -> UserId -> StaticPQ (Maybe (ResponseWithUsers [Post]))
getPostsForUser pId lim uId = do
    postRows <- runQueryParams (getLastNPostsForUserQ lim) (pId, uId) >>= getRows
    wrapIntoResponse (fromMaybe [] . rowsToPosts $ postRows)

getPosts :: [PostId] -> StaticPQ (Maybe (ResponseWithUsers [Post]))
getPosts pId =
    case getPostsByIdQ pId of
        Just psQ -> do
            postRows <- runQuery psQ >>= getRows
            rowsToPosts postRows `liftMaybe` wrapIntoResponse
        Nothing -> return Nothing

getLastPosts :: Maybe PostId -> Limit -> StaticPQ (Maybe (ResponseWithUsers [Post]))
getLastPosts pId lim = do
    postRows <- runQueryParams (getLastPostsQ id lim) (Only pId) >>= getRows
    rowsToPosts postRows `liftMaybe` wrapIntoResponse

getUser :: UserId -> StaticPQ User
getUser uId = runQueryParams getUserQ (Only uId) >>= getRow 0

getUsers :: [UserId] -> StaticPQ (Maybe [User])
getUsers ids = getUsersQ ids `liftMaybe` (fmap Just . (runQuery >=> getRows))

getAllUsers :: StaticPQ [User]
getAllUsers = runQuery getAllUsersQ >>= getRows

publishPost :: UserId -> PostCreation -> StaticPQ PostId
publishPost uId pc  = commitedTransactionallyUpdate $ do
    pId' <- manipulateParams createPostRowQ (uId, postCreationTags pc) >>= firstRow
    case pId' of
        (Just (Only pId)) -> do
            traversePrepared_ createPostElementRowsQ $ elementsToRows pId (postCreationBody pc)
            return pId
        _ -> lift $ S.throwError S.err404

createNewChannel :: UserId -> NamedChannelCreationRequest -> StaticPQ (Maybe NamedChannelId)
createNewChannel uId req = commitedTransactionallyUpdate $ do
    uIds <- verifiedUsers . toList $ namedChannelCreationRequestPeopleIds req
    cId <- manipulateParams createNamedChannelQ (addUserToChannelCreate uId (req {namedChannelCreationRequestPeopleIds = fromList uIds})) >>= firstRow
    return $ fmap fromOnly cId

updateChannel :: UserId -> NamedChannel UserId -> StaticPQ ()
updateChannel uId req = commitedTransactionallyUpdate $ do
    uIds <- fmap fromList . verifiedUsers . toList $ namedChannelPeopleIds req
    _ <- getChannelForUser uId $ namedChannelId req
    _ <- manipulateParams updateNamedChannelQ $ addUserToChannelUpdate uId (req {namedChannelPeopleIds = uIds})
    return ()

getChannelPosts :: UserId -> Maybe PostId -> Limit -> NamedChannelId -> StaticPQ (ResponseWithUsers [Post])
getChannelPosts uId pId lim cId = do
    channel <- getChannelForUser uId cId
    postRows <- runQueryParams (getChannelPostsQ lim (toList $ namedChannelFullPeopleIds channel) (namedChannelFullTags channel)) (Only pId)
        >>= getRows
    fmap (fromMaybe $ ResponseWithUsers mempty mempty) $ rowsToPosts postRows `liftMaybe` wrapIntoResponse

getAnonymousChannelPosts :: Maybe PostId -> Limit -> AnonymousChannel -> StaticPQ (ResponseWithUsers [Post])
getAnonymousChannelPosts pId lim (AnonymousChannel tags people) = do
    postRows <- runQueryParams (getChannelPostsQ lim people tags) (Only pId) >>= getRows
    fmap (fromMaybe $ ResponseWithUsers mempty mempty) $ rowsToPosts postRows `liftMaybe` wrapIntoResponse

getAllChannels :: UserId -> StaticPQ [NamedChannel UserId]
getAllChannels uId = fmap (map removeUserFromChannel) $
    runQueryParams getAllChannelsForUserQ (Only uId) >>= getRows

deleteNamedChannel :: UserId -> NamedChannelId -> StaticPQ ()
deleteNamedChannel uId cId = commitedTransactionallyUpdate $ do
    _ <- getChannelForUser uId cId
    _ <- manipulateParams deleteNamedChannelQ (Only cId)
    return ()

getTags :: StaticPQ [String]
getTags = map fromOnly <$> (runQuery getTagsQ >>= getRows)

-- MARK: Utils

foldMaybe :: [Maybe a] -> Maybe [a]
foldMaybe [] = Just []
foldMaybe (Nothing : _) = Nothing
foldMaybe (Just x : xs) = (x :) <$> foldMaybe xs

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

data ResponseWithUsers r = ResponseWithUsers {
    response :: r,
    users :: M.Map UserId User
} deriving (Generic)

instance (ToJSON r) => ToJSON (ResponseWithUsers r)

instance (FromJSON r) => FromJSON (ResponseWithUsers r)

class HasUsers r where
    userSet :: r -> Set.Set UserId

    wrapIntoResponse :: r -> StaticPQ (Maybe (ResponseWithUsers r))
    wrapIntoResponse r = wrapIntoUsers r $ userSet r

instance (Foldable t, HasUsers r) => HasUsers (t r) where
    userSet = foldMap userSet

instance HasUsers Post where
    userSet = Set.singleton . postAuthorId

wrapIntoUsers :: r -> Set.Set UserId -> StaticPQ (Maybe (ResponseWithUsers r))
wrapIntoUsers r us
    | Set.null us = return . Just $ ResponseWithUsers r M.empty
    | otherwise = do
        pUsers <- getUsers $ Set.toList us
        return $ ResponseWithUsers r . M.fromList . mapFirstBy userId <$> pUsers

liftMaybe :: Applicative f => Maybe a -> (a -> f (Maybe b)) -> f (Maybe b)
liftMaybe Nothing _ = pure Nothing
liftMaybe (Just a) f = f a