{-# LANGUAGE
    DataKinds,
    DeriveGeneric,
    OverloadedLabels,
    OverloadedStrings,
    TypeApplications,
    TypeOperators,
    PartialTypeSignatures,
    FlexibleInstances,
    KindSignatures,
    DeriveAnyClass,
    RecordWildCards #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}


module Data.Query (
    Schema,
    createTables,
    getPosts,
    -- getUser,
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
    getTags,
    getDialogs,
    getMessagesInUserChat,
    getMessagesInGroupChat,
    sendMessage,
    createGroupChat,
    updateGroupChat,
    leaveGroupChat,
    getGroupChatForUser,
    getFaculty,
    updateFaculty,
    getFacultyFromQuery,
    getUserWithEmail,
    createToken,
    validateToken,
    invalidateToken,
    tryVerifyToken,
    getVerifiedToken,
    UserCreation(..),
    createUser,
    tryActivateToken,
    deactivateToken,
    searchUsers,
    searchTags,
    updateUser
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
import qualified Data.Text as T
import Data.Channel.NamedChannel
import Data.Channel.AnonymousChannel
import Data.Maybe (fromMaybe)
import Squeal.PostgreSQL.Render
import Data.GroupChat
import Data.Message
import Server.Query.Dialog
import qualified Server.Query.Pagination as P
import Data.Faculty
import Data.ByteString (ByteString)
import Data.Foldable (fold)
import Data.Char
import qualified Data.Vector as V
import Server.Auth.Token

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

getUserWithEmailQ :: Query Schema (TuplePG (Only UserEmail)) (RowPG (User FacultyUrl))
getUserWithEmailQ = select (
    #userRowId `as` #userId :*
    #userRowFirstName `as` #firstName :*
    #userRowMiddleName `as` #middleName :*
    #userRowLastName `as` #lastName :*
    #userRowFacultyUrl `as` #userFaculty :*
    #userRowEmail `as` #userEmail
    ) (from (table #users) &
    where_ (#userRowEmail .== param @1) &
    where_ (notNull #userRowIsValidated))

verifiedUserIdsQ :: [UserId] -> Maybe (Query Schema '[] (RowPG (Only UserId)))
verifiedUserIdsQ uIds = case idsToColumn uIds of
    Just idQ -> Just $ select (#users ! #userRowId `as` #fromOnly) $ from (subquery (idQ `As` #ids) &
        innerJoin (table #users)
            (#ids ! #id .== #users ! #userRowId))
    Nothing -> Nothing

getUsersQ :: [UserId] -> Maybe (Query Schema '[] (RowPG UserRow))
getUsersQ uIds = case idsToColumn uIds of
    Just idQ -> Just $ select (
            #users ! #userRowId :*
            #users ! #userRowFirstName :*
            #users ! #userRowMiddleName :*
            #users ! #userRowLastName :*
            #users ! #userRowFacultyUrl :*
            #users ! #userRowEmail :*
            #users ! #userRowIsValidated :*
            #faculties ! #facultyName `as` #userRowFacultyName :*
            #faculties ! #facultyUrl `as` #userRowFacultyURL :*
            #faculties ! #facultyPath `as` #userRowFacultyPath :*
            #faculties ! #facultyCampusName `as` #userRowFacultyCampusName :*
            #faculties ! #facultyCampusCode `as` #userRowFacultyCampusCode :*
            #faculties ! #facultyTags `as` #userRowFacultyTags :*
            #faculties ! #facultyAddress `as` #userRowFacultyAddress
        ) (from (subquery (idQ `As` #ids) &
        innerJoin (table #users)
            (#ids ! #id .== #users ! #userRowId) &
        innerJoin (table #faculties)
            (#users ! #userRowFacultyUrl .== #faculties ! #facultyUrl)) &
            where_ (notNull $ #users ! #userRowIsValidated))
    Nothing -> Nothing

getAllUsersQ :: Query Schema '[] (RowPG UserRow)
getAllUsersQ = select (
            #users ! #userRowId :*
            #users ! #userRowFirstName :*
            #users ! #userRowMiddleName :*
            #users ! #userRowLastName :*
            #users ! #userRowFacultyUrl :*
            #users ! #userRowEmail :*
            #users ! #userRowIsValidated :*
            #faculties ! #facultyName `as` #userRowFacultyName :*
            #faculties ! #facultyUrl `as` #userRowFacultyURL :*
            #faculties ! #facultyPath `as` #userRowFacultyPath :*
            #faculties ! #facultyCampusName `as` #userRowFacultyCampusName :*
            #faculties ! #facultyCampusCode `as` #userRowFacultyCampusCode :*
            #faculties ! #facultyTags `as` #userRowFacultyTags :*
            #faculties ! #facultyAddress `as` #userRowFacultyAddress
        ) (from ((table #users) &
        innerJoin (table #faculties)
            (#users ! #userRowFacultyUrl .== #faculties ! #facultyUrl))&
            where_ (notNull $ #users ! #userRowIsValidated))

getPostsByIdQ :: [PostId] -> Maybe (Query Schema '[] (RowPG PostRowResponse))
getPostsByIdQ pIds = case idsToColumn pIds of
    Just idQ -> Just $ selectDotStar #posts $ from (subquery (idQ `As` #ids) &
        innerJoin (subquery $ getPostQ
            (orderBy [#postElements ! #rowElementId & Desc, #postElements ! #rowElementOrd & Asc]) id `As` #posts)
            (#ids ! #id .== #posts ! #postRowId))
    Nothing -> Nothing

type Limit = Word64

isNotFalse
  :: Expression schema from grouping params ('Null ty)
  -- ^ possibly @NULL@
  -> Condition schema from grouping params
isNotFalse x = UnsafeExpression $ renderExpression x <+> "IS NOT FALSE"

getLastPostsQ
    :: TableEndo _ _ _ _
    -> Limit
    -> P.PaginationDirection
    -> Query Schema ('Null (PG Int64) ': params) (RowPG PostRowResponse)
getLastPostsQ te lim dir = getPostQ
    (orderBy [#postElements ! #rowElementId & Desc, #postElements ! #rowElementOrd & Asc])
    (limit lim .
        te .
        where_ (ifThenElse (isNull $ param @1) true $ #posts ! #postRowId .^ param @1) .
        orderBy [#posts ! #postRowId & order])
    where
        ((.^), order) = directionToOperator dir

getLastNPostsForUserQ
    :: Limit
    -> P.PaginationDirection
    -> Query Schema (TuplePG (Maybe PostId, UserId)) (RowPG PostRowResponse)
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

updateUserQ :: Manipulation Schema (TuplePG (User FacultyUrl)) (RowPG (Only UserId))
updateUserQ = update #users (
    Same `as` #userRowId :*
    Set (param @2) `as` #userRowFirstName :*
    Set (param @3) `as` #userRowMiddleName :*
    Set (param @4) `as` #userRowLastName :*
    Set (param @5) `as` #userRowFacultyUrl :*
    Same `as` #userRowEmail :*
    Same `as` #userRowIsValidated)
    (#userRowId .== param @1 .&& #userRowEmail .== param @6)
    (Returning $ #userRowId `as` #fromOnly)

getAllChannelsForUserQ :: Query Schema (TuplePG (Only UserId)) (RowPG NamedChannelFull)
getAllChannelsForUserQ = selectStar $
    from (table #channels) &
    where_ (#namedChannelFullOwner .== param @1) &
    orderBy [#namedChannelFullId & Desc]

updateFacultyQ :: Manipulation Schema (TuplePG Faculty) '[]
updateFacultyQ = insertRow #faculties
    (
    Set (param @1) `as` #facultyName :*
    Set (param @2) `as` #facultyUrl :*
    Set (param @3) `as` #facultyPath :*
    Set (param @4) `as` #facultyCampusName :*
    Set (param @5) `as` #facultyCampusCode :*
    Set (param @6) `as` #facultyTags :*
    Set (param @7) `as` #facultyAddress
    )
    (OnConflictDoNothing)
    (Returning Nil)

getFacultyQ :: Query Schema (TuplePG (Only Text)) (RowPG Faculty)
getFacultyQ = selectStar (from (table #faculties) &
        where_ (#facultyUrl .== param @1))

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
    -> P.PaginationDirection
    -> [UserId]
    -> Vector Text
    -> Query
        Schema
        (TuplePG (Only (Maybe PostId))) -- post id from which to start
        (RowPG PostRowResponse)
getChannelPostsQ lim dir uIds tags = getLastPostsQ
        (where_ $ jsonbLit tags .?| #postRowTags .|| isUser #postRowAuthorId)
        lim
        dir
    where
        isUser = case idsToColumn uIds of
            Just uIdsQ -> (`in_` uIdsQ)
            Nothing -> const $ false

getTagsQ :: Query Schema params (RowPG (Only String))
getTagsQ = selectDistinct (unsafeFunction "unnest" #postRowTags `as` #fromOnly) $
    from (table #posts)

createMessageQ :: Manipulation
    Schema
    (TuplePG (UserId, Maybe GroupChatId, Maybe UserId, Jsonb (PostElement (MessageCreation))))
    (RowPG (Only MessageId))
createMessageQ = insertRow #messages
    (
        Default `as` #messageStorageId :*
        Set (param @1) `as` #messageStorageAuthorId :*
        Set (param @2) `as` #messageStorageDestinationGroupId :*
        Set (param @3) `as` #messageStorageDestinationUserId :*
        Set (param @4) `as` #messageStorageBody :*
        Set currentTimestamp `as` #messageStorageTime
    )
    OnConflictDoRaise
    (Returning $ #messageStorageId `as` #fromOnly)

createGroupChatQ :: Manipulation Schema (TuplePG GroupChatCreation) (RowPG (Only GroupChatId))
createGroupChatQ = insertRow #groupChats
    (
        Default `as` #groupChatId :*
        Set (param @1) `as` #groupChatUsers :*
        Set (param @2) `as` #groupChatName
    )
    OnConflictDoRaise
    (Returning $ #groupChatId `as` #fromOnly)

updateGroupChatQ :: Manipulation Schema (TuplePG GroupChat) '[]
updateGroupChatQ = update_ #groupChats
    (
        Same `as` #groupChatId :*
        Set (param @2) `as` #groupChatUsers :*
        Set (param @3) `as` #groupChatName
    )
    (#groupChatId .== param @1)

type ChatsQueryResponse = '[
    "messageId" ::: 'NotNull (PG UserId),
    "messageGroupId" ::: 'Null (PG UserId),
    "messageUserId" ::: 'Null (PG UserId)
    ]

type ConditionOp schema from grouping params nullity0 nullity1 (ty :: PGType)
    = Expression schema from grouping params (nullity0 ty)
    -> Expression schema from grouping params (nullity1 ty)
    -> Condition schema from grouping params

type ChatQueryParams = TuplePG
    ( UserId
    , Maybe String -- Just UserId as String
    , Maybe MessageId
    )

getChatsQ
    :: Limit
    -> P.PaginationDirection
    -> Query Schema ChatQueryParams (RowPG ChatsResponse)
getChatsQ lim dir = select (
        #chats ! #messageId `as` #chatResponseMessageId :*
        #messages ! #messageStorageAuthorId `as` #chatResponseMessageAuthorId :*
        #messages ! #messageStorageDestinationGroupId `as` #chatResponseMessageDestinationGroupId :*
        #messages ! #messageStorageDestinationUserId `as` #chatResponseMessageDestinationUserId :*
        #messages ! #messageStorageBody `as` #chatResponseMessageBody :*
        #messages ! #messageStorageTime `as` #chatResponseMessageTime :*
        #groupChats ! #groupChatUsers `as` #chatResponseGroupUsers :*
        #groupChats ! #groupChatName `as` #chatResponseGroupName
    ) $ from (
    (subquery $ chatsQ `As` #chats) &
        innerJoin
            (table #messages)
            (#chats ! #messageId .== #messages ! #messageStorageId) &
        leftOuterJoin
            (table #groupChats)
            (#groupChats ! #groupChatId .== #chats ! #messageGroupId)) &
        where_ (isNull (#groupChats ! #groupChatUsers) .||
            (#groupChats ! #groupChatUsers .? param @2)) &
        where_ (isNotFalse $ #chats ! #messageId .^ param @3) &
        orderBy [#chats ! #messageId & order] &
        limit lim &
        orderBy [#chats ! #messageId & Desc]
    where
        ((.^), order) = directionToOperator dir
        chatsQ :: Query Schema _ ChatsQueryResponse
        chatsQ = unionAll groupChats userChats
            where
                allUserMessages :: Query Schema _ ChatsQueryResponse
                allUserMessages = select (
                    #messageStorageId `as` #messageId :*
                    null_ `as` #messageGroupId :*
                    (ifThenElse
                        (#messageStorageAuthorId .== param @1)
                        #messageStorageDestinationUserId
                        (notNull #messageStorageAuthorId)) `as` #messageUserId) $
                    from (table #messages) &
                    where_ (isNotNull #messageStorageDestinationUserId .&&
                        (#messageStorageAuthorId .== param @1 .||
                        #messageStorageDestinationUserId .== param @1))
                userChats :: Query Schema _ ChatsQueryResponse
                userChats = select (
                    max_ #messageId `as` #messageId :*
                    null_ `as` #messageGroupId :*
                    #messageUserId `as` #messageUserId) $
                    from (subquery $ allUserMessages `As` #allUserMessages) &
                    groupBy (#allUserMessages ! #messageUserId)
                groupChats :: Query Schema _ ChatsQueryResponse
                groupChats = select (
                    max_ #messageStorageId `as` #messageId :*
                    #messageStorageDestinationGroupId `as` #messageGroupId :*
                    null_ `as` #messageUserId) $
                    from (table #messages) &
                    where_ (isNotNull #messageStorageDestinationGroupId) &
                    groupBy #messageStorageDestinationGroupId

getMessagesQ
    :: Limit
    -> P.PaginationDirection
    -> Query Schema (TuplePG
    ( UserId
    , UserId
    , Maybe MessageId
    )) (RowPG MessageStorage)
getMessagesQ lim dir = selectStar (from (table #messages) &
    where_ (isNotNull #messageStorageDestinationUserId .&&
        ((#messageStorageAuthorId .== param @1 .&& #messageStorageDestinationUserId .== param @2) .||
        (#messageStorageDestinationUserId .== param @1 .&& #messageStorageAuthorId .== param @2))) &
        where_ (isNotFalse $ #messageStorageId .^ param @3) &
        orderBy [#messageStorageId & order] &
        limit lim &
        orderBy [#messageStorageId & Desc])
    where
        ((.^), order) = directionToOperator dir

getGroupMessagesQ
    :: Limit
    -> P.PaginationDirection
    -> Query Schema (TuplePG
    ( GroupChatId
    , Maybe MessageId
    )) (RowPG MessageStorage)
getGroupMessagesQ lim dir = selectStar (from (table #messages) &
    where_ (isNotNull #messageStorageDestinationGroupId .&&
        (#messageStorageDestinationGroupId .== param @1)) &
        where_ (isNotFalse $ #messageStorageId .^ param @2) &
        orderBy [#messageStorageId & order] &
        limit lim &
        orderBy [#messageStorageId & Desc])
    where
        ((.^), order) = directionToOperator dir

-- String -- user id as String
getGroupChatForUserQ :: Query Schema (TuplePG (String, GroupChatId)) (RowPG GroupChat)
getGroupChatForUserQ = selectStar $
    (from (table #groupChats)) &
    where_ (#groupChatId .== param @2) &
    where_ (#groupChatUsers .? param @1)

getFacultySearchResultQ :: Query Schema (TuplePG (Only Text)) (RowPG Faculty)
getFacultySearchResultQ = selectStar (from (table #faculties) &
    where_ (vector @@ query) &
    orderBy [tsRankCd vector query & Desc])
    where
        vector = tsVector "russian" (textArrayToText [
            #facultyName,
            #facultyPath,
            #facultyCampusName,
            (arrayToText #facultyTags)])
        query = tsQuery "russian" (param @1)

searchUsersQ :: Query Schema (TuplePG (Only Text)) (RowPG UserRow)
searchUsersQ = select (
            #users ! #userRowId :*
            #users ! #userRowFirstName :*
            #users ! #userRowMiddleName :*
            #users ! #userRowLastName :*
            #users ! #userRowFacultyUrl :*
            #users ! #userRowEmail :*
            #users ! #userRowIsValidated :*
            #faculties ! #facultyName `as` #userRowFacultyName :*
            #faculties ! #facultyUrl `as` #userRowFacultyURL :*
            #faculties ! #facultyPath `as` #userRowFacultyPath :*
            #faculties ! #facultyCampusName `as` #userRowFacultyCampusName :*
            #faculties ! #facultyCampusCode `as` #userRowFacultyCampusCode :*
            #faculties ! #facultyTags `as` #userRowFacultyTags :*
            #faculties ! #facultyAddress `as` #userRowFacultyAddress
        ) (from ((table #users) &
            innerJoin (table #faculties)
                (#users ! #userRowFacultyUrl .== #faculties ! #facultyUrl)) &
                where_ (notNull $ #users ! #userRowIsValidated) &
                where_ (vector @@ query) &
                orderBy [tsRankCd vector query & Desc] &
                limit 100)
    where
        vector = tsVector' (textArrayToText [
            #users ! #userRowFirstName,
            #users ! #userRowMiddleName,
            #users ! #userRowLastName
            ])
        query = tsQuery' (param @1)

tagsByFrquencyQ :: Query Schema params '["tag" ::: 'NotNull (PG Text), "count" ::: 'NotNull 'PGint8]
tagsByFrquencyQ = select
            (#fromOnly `as` #tag :* count #fromOnly `as` #count)
            (from (subquery $ getTagsQ `As` #tt) &
            groupBy #fromOnly)

getNpopularTagsQ :: Limit -> Query Schema param (RowPG (Only Text))
getNpopularTagsQ lim = select (#tag `as` #fromOnly)
    (from (subquery $ tagsByFrquencyQ `As` #tt) &
        orderBy [#count & Desc] &
        limit lim)


searchTagsQ :: Query Schema (TuplePG (Only Text)) (RowPG (Only Text))
searchTagsQ = select (#tag `as` #fromOnly) $
    from (subquery (tagsByFrquencyQ `As` #tt)) &
    where_ (vector @@ query) &
    orderBy [#count & Desc] &
    limit 100
    where
        query = tsQuery' (param @1)
        vector = tsVector' #tag

tsRankCd
    :: Expression schema from grouping params ('NotNull 'PGtext)
    -> Expression schema from grouping params ('NotNull 'PGtext)
    -> Expression schema from grouping params ('NotNull 'PGfloat4)
tsRankCd vector query = UnsafeExpression $
    "ts_rank_cd" <> parenthesized (commaSeparated . map renderExpression $ [vector, query])

tsVector
    :: ByteString -- ^ Language
    -> Expression schema from grouping params ('NotNull 'PGtext)
    -> Expression schema from grouping params ('NotNull 'PGtext)
tsVector lang x = UnsafeExpression $
    "to_tsvector" <> parenthesized (commaSeparated  [singleQuotedUtf8 lang, renderExpression x])

tsVector'
    :: Expression schema from grouping params ('NotNull 'PGtext)
    -> Expression schema from grouping params ('NotNull 'PGtext)
tsVector' x = UnsafeExpression $
    "to_tsvector" <> parenthesized (renderExpression x)

tsQuery
    :: ByteString -- ^ Language
    -> Expression schema from grouping params ('NotNull 'PGtext)
    -> Expression schema from grouping params ('NotNull 'PGtext)
tsQuery lang x = UnsafeExpression $
    "to_tsquery" <> parenthesized (commaSeparated  [singleQuotedUtf8 lang, renderExpression x])

tsQuery'
    :: Expression schema from grouping params ('NotNull 'PGtext)
    -> Expression schema from grouping params ('NotNull 'PGtext)
tsQuery' x = UnsafeExpression $
    "to_tsquery" <> parenthesized (renderExpression x)

arrayToText
    :: Expression schema from grouping params ('NotNull  ('PGvararray ('NotNull 'PGtext)))
    -> Expression schema from grouping params ('NotNull  'PGtext)
arrayToText x = UnsafeExpression $
    "array_to_string" <> parenthesized (commaSeparated  [renderExpression x, "' '"])

(@@)
    :: Expression schema from grouping params (nullity0  'PGtext)
    -> Expression schema from grouping params (nullity1  'PGtext)
    -> Expression schema from grouping params (nullity2  'PGbool)
(@@) = unsafeBinaryOp "@@"

space :: Expression schema from grouping params ('NotNull  'PGtext)
space = UnsafeExpression "' '"

textArrayToText
    :: [Expression schema from grouping params ('NotNull  'PGtext)]
    -> Expression schema from grouping params ('NotNull  'PGtext)
textArrayToText = fold . L.intersperse space

createUserQ :: Manipulation Schema (TuplePG UserCreation) (RowPG (Only UserId))
createUserQ = insertRow #users (
    Default `as` #userRowId :*
    Set (param @1) `as` #userRowFirstName :*
    Set (param @2) `as` #userRowMiddleName :*
    Set (param @3) `as` #userRowLastName :*
    Set (param @4) `as` #userRowFacultyUrl :*
    Set (param @5) `as` #userRowEmail :*
    Set false `as` #userRowIsValidated)
    OnConflictDoRaise
    (Returning (#userRowId `as` #fromOnly))

removeUnverifiedUserQ :: Manipulation Schema (TuplePG (Only UserEmail)) '[]
removeUnverifiedUserQ = deleteFrom_ #users
    (#userRowEmail .== param @1 .&&
        (not_ . notNull $ #userRowIsValidated))


createTokenQ :: Manipulation Schema (TuplePG Token) '[]
createTokenQ = insertRow_ #tokens (
        Set (param @1) `as` #tokenUserId :*
        Set (param @2) `as` #tokenValue :*
        Set (param @3) `as` #tokenExpiryDate :*
        Set (param @4) `as` #tokenVerificationCode :*
        Set (param @5) `as` #tokenActivationTriesLeft :*
        Set (param @6) `as` #tokenUserAgent :*
        Set (param @7) `as` #tokenActivationCode :*
        Set (param @8) `as` #tokenDeactivationCode
    )

updateTokenQ :: Manipulation Schema (TuplePG Token) '[]
updateTokenQ = update_ #tokens (
        Same `as` #tokenUserId :*
        Same `as` #tokenValue :*
        Set (param @3) `as` #tokenExpiryDate :*
        Set (param @4) `as` #tokenVerificationCode :*
        Set (param @5) `as` #tokenActivationTriesLeft :*
        Set (param @6) `as` #tokenUserAgent :*
        Set (param @7) `as` #tokenActivationCode :*
        Set (param @8) `as` #tokenDeactivationCode)
    (#tokenUserId .== param @1 .&& #tokenValue .== param @2)

deleteTokenQ :: Manipulation Schema (TuplePG (UserId, ByteString)) '[]
deleteTokenQ = deleteFrom_ #tokens
    (#tokenUserId .== param @1 .&& #tokenValue .== param @2)

validateUserQ :: Manipulation Schema (TuplePG (Only UserId)) '[]
validateUserQ = update_ #users (
    Same `as` #userRowId :*
    Same `as` #userRowFirstName :*
    Same `as` #userRowMiddleName :*
    Same `as` #userRowLastName :*
    Same `as` #userRowFacultyUrl :*
    Same `as` #userRowEmail :*
    Set true `as` #userRowIsValidated)
    (#userRowId .== param @1)

getTokenQ :: Query Schema (TuplePG (UserId, ByteString)) (RowPG Token)
getTokenQ = selectStar (from (table #tokens) &
    where_ (#tokenUserId .== param @1 .&&
        #tokenValue .== param @2))

getTokenActivationQ :: Query Schema (TuplePG (UserId, ByteString)) (RowPG Token)
getTokenActivationQ = selectStar (from (table #tokens) &
    where_ (#tokenUserId .== param @1 .&&
        #tokenActivationCode .== param @2))

deactivateTokenQ :: Manipulation Schema (TuplePG (UserId, ByteString)) (RowPG (Only Text))
deactivateTokenQ = deleteFrom #tokens
    (#tokenUserId .== param @1 .&&
        #tokenDeactivationCode .== param @2)
    (Returning (#tokenUserAgent `as` #fromOnly))

getVerifiedTokenQ :: Query Schema (TuplePG (UserId, ByteString)) (RowPG Token)
getVerifiedTokenQ = selectStar (from (table #tokens) &
    where_ (#tokenUserId .== param @1 .&&
        #tokenValue .== param @2 .&&
        isNull #tokenVerificationCode))

-- MARK: FrontEnd Data Structures

data UserCreation = UserCreation {
    userCreationFirstName :: Text,
    userCreationMiddleName :: Text,
    userCreationLastName :: Text,
    userCreationUserFaculty :: FacultyUrl,
    userCreationUserEmail :: Text
} deriving (GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

instance ToJSON UserCreation where
    toJSON (UserCreation fName mName lName faculty email) = object [
        "firstName" .= fName,
        "middleName" .= mName,
        "lastName" .= lName,
        "faculty" .= faculty,
        "email" .= email
        ]

instance FromJSON UserCreation where
    parseJSON = withObject "named channel" $ \e ->
        UserCreation <$> e .: "firstName" <*> e .: "middleName" <*> e .: "lastName" <*> e .: "faculty" <*> e .: "email"

data NamedChannelCreation = NamedChannelCreation {
    namedChannelCreationOwner :: UserId,
    namedChannelCreationName :: Text,
    namedChannelCreationTags :: Vector Text,
    namedChannelCreationPeopleIds :: Vector UserId
} deriving (GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

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

deleteGroupChat :: GroupChatId -> StaticPQ ()
deleteGroupChat gId = do
    _ <- manipulateParams deleteGroupChatQ (Only gId)
    _ <- manipulateParams deleteGroupChatMessagesQ (Only gId)
    return ()
    where
        deleteGroupChatQ :: Manipulation Schema (TuplePG (Only GroupChatId)) '[]
        deleteGroupChatQ = deleteFrom_ #groupChats
            (#groupChatId .== param @1)
        deleteGroupChatMessagesQ :: Manipulation Schema (TuplePG (Only GroupChatId)) '[]
        deleteGroupChatMessagesQ = deleteFrom_ #messages
            (#messageStorageDestinationGroupId .== param @1)

-- NOTE: Not transactional
createUser :: UserCreation -> StaticPQ UserId
createUser creation = do
    _ <- manipulateParams removeUnverifiedUserQ (Only $ userCreationUserEmail creation)
    rowThing <- manipulateParams createUserQ creation >>= firstRow >>= fromMaybe500
    return (fromOnly rowThing)

-- MARK: FrontEnd functions

-- Nothing <=> no user
getPostsForUser
    :: Maybe PostId
    -> Limit
    -> P.PaginationDirection
    -> UserId
    -> StaticPQ (Maybe (ResponseWithUsers [Post]))
getPostsForUser pId lim dir uId = do
    postRows <- runQueryParams (getLastNPostsForUserQ lim dir) (pId, uId) >>= getRows
    wrapIntoResponse (fromMaybe [] . rowsToPosts $ postRows)

getPosts :: [PostId] -> StaticPQ (Maybe (ResponseWithUsers [Post]))
getPosts pId =
    case getPostsByIdQ pId of
        Just psQ -> do
            postRows <- runQuery psQ >>= getRows
            rowsToPosts postRows `liftMaybe` wrapIntoResponse
        Nothing -> return Nothing

getLastPosts
    :: Maybe PostId
    -> Limit
    -> P.PaginationDirection
    -> StaticPQ (Maybe (ResponseWithUsers [Post]))
getLastPosts pId lim dir = do
    postRows <- runQueryParams (getLastPostsQ id lim dir) (Only pId) >>= getRows
    rowsToPosts postRows `liftMaybe` wrapIntoResponse

getUserWithEmail :: UserEmail -> StaticPQ (Maybe (User FacultyUrl))
getUserWithEmail email = runQueryParams getUserWithEmailQ (Only email) >>= firstRow

getUsers :: [UserId] -> StaticPQ (Maybe [User Faculty])
getUsers ids = getUsersQ ids `liftMaybe` (fmap (Just . map rowToUser) . (runQuery >=> getRows))

getAllUsers :: StaticPQ [User Faculty]
getAllUsers = fmap rowToUser <$> (runQuery getAllUsersQ >>= getRows)

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

getFaculty :: Text -> StaticPQ Faculty
getFaculty t = runQueryParams getFacultyQ (Only t) >>= firstRow >>= fromMaybe404

updateFaculty :: Faculty -> StaticPQ ()
updateFaculty fac = void $ manipulateParams updateFacultyQ fac

getChannelPosts
    :: UserId
    -> Maybe PostId
    -> Limit
    -> P.PaginationDirection
    -> NamedChannelId
    -> StaticPQ (ResponseWithUsers [Post])
getChannelPosts uId pId lim dir cId = do
    channel <- getChannelForUser uId cId
    postRows <- runQueryParams (getChannelPostsQ
                lim
                dir
                (toList $ namedChannelFullPeopleIds channel)
                (namedChannelFullTags channel))
            (Only pId)
        >>= getRows
    fmap (fromMaybe $ ResponseWithUsers mempty mempty) $ rowsToPosts postRows `liftMaybe` wrapIntoResponse

getAnonymousChannelPosts
    :: Maybe PostId
    -> Limit
    -> P.PaginationDirection
    -> AnonymousChannel
    -> StaticPQ (ResponseWithUsers [Post])
getAnonymousChannelPosts pId lim dir (AnonymousChannel tags people) = do
    postRows <- runQueryParams (getChannelPostsQ lim dir people tags) (Only pId) >>= getRows
    fmap (fromMaybe $ ResponseWithUsers mempty mempty) $ rowsToPosts postRows `liftMaybe` wrapIntoResponse

getAllChannels :: UserId -> StaticPQ [NamedChannel UserId]
getAllChannels uId = fmap (map removeUserFromChannel) $
    runQueryParams getAllChannelsForUserQ (Only uId) >>= getRows

deleteNamedChannel :: UserId -> NamedChannelId -> StaticPQ ()
deleteNamedChannel uId cId = commitedTransactionallyUpdate $ do
    _ <- getChannelForUser uId cId
    _ <- manipulateParams deleteNamedChannelQ (Only cId)
    return ()

-- Returns n most popular
getTags :: Limit -> StaticPQ [String]
getTags lim = map fromOnly <$> (runQuery (getNpopularTagsQ lim) >>= getRows)

getDialogs
    :: UserId
    -> Maybe MessageId
    -> Limit
    -> P.PaginationDirection
    -> StaticPQ (ResponseWithUsers [Dialog])
getDialogs uId mId lim dir = do
    resp <- runQueryParams (getChatsQ lim dir) (uId, Just $ show uId, mId) >>= getRows
    (>>= fromMaybe500) $ (responseToDialogs uId resp `liftMaybe` wrapIntoResponse)

getMessagesInUserChat
    :: UserId
    -> UserId
    -> Maybe MessageId
    -> Limit
    -> P.PaginationDirection
    -> StaticPQ [Message]
getMessagesInUserChat selfId uId mId lim dir = do
    resp <- runQueryParams (getMessagesQ lim dir) (selfId, uId, mId) >>= getRows
    fromMaybe500 . sequence . map restoreMessage $ resp

getMessagesInGroupChat
    :: UserId
    -> GroupChatId
    -> Maybe MessageId
    -> Limit
    -> P.PaginationDirection
    -> StaticPQ [Message]
getMessagesInGroupChat selfId gId mId lim dir = do
    _ <- getGroupChatForUser selfId gId
    resp <- runQueryParams (getGroupMessagesQ lim dir) (gId, mId) >>= getRows
    (return . sequence . map restoreMessage $ resp) >>= fromMaybe500

sendMessage :: UserId -> MessageCreation -> StaticPQ MessageId
sendMessage selfId (MessageCreation (Just gId) uId@Nothing body) = do
    _ <- getGroupChatForUser selfId gId
    (Only mId) <- manipulateParams createMessageQ (selfId, Just gId, uId, body)
        >>= firstRow >>= fromMaybe500
    return mId
sendMessage selfId (MessageCreation gId@Nothing (Just uId) body) = do
    (Only mId) <- manipulateParams createMessageQ (selfId, gId, Just uId, body)
        >>= firstRow >>= fromMaybe500
    return mId
sendMessage _ _ = lift $ S.throwError S.err400

createGroupChat :: UserId -> GroupChatCreation -> StaticPQ GroupChatId
createGroupChat uId (GroupChatCreation (Jsonb chatUsers) name) = do
    let chatUsers' = M.insert uId maxChatPermissions chatUsers
    (Only chatId) <- manipulateParams createGroupChatQ (GroupChatCreation (Jsonb chatUsers') name)
        >>= firstRow >>= fromMaybe500
    -- TODO: Do somehting with this
    _ <- sendMessage uId $ MessageCreation (Just chatId) Nothing (Jsonb $ Markdown "henlo")
    return chatId

updateGroupChat :: UserId -> GroupChat -> StaticPQ ()
updateGroupChat uId (GroupChat gId (Jsonb perms) name) = commitedTransactionallyUpdate $ do
    (GroupChat _ (Jsonb perms') _) <- getGroupChatForUser uId gId
    let userPerm = fromMaybe minChatPermissions $ M.lookup uId perms'
    unless (isAdmin userPerm) $ lift $ S.throwError $ S.err401
    let newPerms = M.insert uId userPerm perms
    void $ manipulateParams updateGroupChatQ (GroupChat gId (Jsonb newPerms) name)

leaveGroupChat :: UserId -> GroupChatId -> StaticPQ ()
leaveGroupChat uId gId = commitedTransactionallyUpdate $ do
    gChat@(GroupChat _ (Jsonb perms) _) <- getGroupChatForUser uId gId
    let newPerms = M.delete uId perms
    if M.null newPerms
        then deleteGroupChat gId
        else void $ manipulateParams updateGroupChatQ gChat{groupChatUsers = Jsonb newPerms}

getGroupChatForUser :: UserId -> GroupChatId -> StaticPQ GroupChat
getGroupChatForUser uId gId = do
    chat <- runQueryParams getGroupChatForUserQ (show uId, gId) >>= firstRow
    case (chat) of
        (Just c) -> return c
        Nothing -> lift $ S.throwError S.err404

getFacultyFromQuery :: T.Text -> StaticPQ [Faculty]
getFacultyFromQuery query =
    runQueryParams getFacultySearchResultQ (Only . queryFromTextAbbr $ query) >>= getRows

searchUsers :: T.Text -> StaticPQ [User Faculty]
searchUsers query =
    map rowToUser <$> (runQueryParams searchUsersQ (Only . queryFromText $ query) >>= getRows)

searchTags :: T.Text -> StaticPQ [Text]
searchTags query = map fromOnly <$>
    (runQueryParams searchTagsQ
        (Only . (<> ":*") . T.strip $ query) >>= getRows)

createToken :: Token -> StaticPQ ()
createToken token = void $ manipulateParams createTokenQ token


-- NOTE: Not transactional
validateToken :: Token -> StaticPQ ()
validateToken token = do
    void $ manipulateParams updateTokenQ token{tokenVerificationCode = Nothing, tokenActivationCode = Nothing}
    void $ manipulateParams validateUserQ $ Only $ tokenUserId token

invalidateToken :: UserId -> ByteString -> StaticPQ ()
invalidateToken uId bs = void . manipulateParams deleteTokenQ $ (uId, bs)

tryVerifyToken :: UserId -> ByteString -> Int -> StaticPQ (Maybe Token)
tryVerifyToken uId tokenData code = commitedTransactionallyUpdate $ do
    token' <- runQueryParams getTokenQ (uId, hash tokenData) >>= firstRow
    case token' of
        Nothing -> return Nothing
        Just token -> do
            if (tokenVerificationCode token == Just (hashVerificationCode code))
                then validateToken token >> (return $ Just token)
                else do
                    let triesLeft = tokenActivationTriesLeft token - 1
                    if triesLeft <= 0
                        then invalidateToken (tokenUserId token) (tokenValue token) >> return Nothing
                        else do
                            let newToken = token{tokenActivationTriesLeft = triesLeft}
                            _ <- manipulateParams updateTokenQ newToken
                            return Nothing

tryActivateToken :: UserId -> ByteString -> StaticPQ (Maybe Token)
tryActivateToken uId activationData = do
    let hData = hash activationData
    token' <- runQueryParams getTokenActivationQ (uId, hData) >>= firstRow
    case token' of
        Nothing -> return Nothing
        Just token -> do
            if (tokenActivationCode token == Just hData)
                then commitedTransactionallyUpdate (validateToken token) >> (return $ Just token)
                else return Nothing

deactivateToken :: UserId -> ByteString -> StaticPQ (Maybe Text)
deactivateToken uId dData = fmap fromOnly <$> (manipulateParams deactivateTokenQ (uId, hash dData) >>= firstRow)

getVerifiedToken :: UserId -> ByteString -> StaticPQ (Maybe Token)
getVerifiedToken uId tokenData = runQueryParams getVerifiedTokenQ (uId, hash tokenData) >>= firstRow

updateUser :: (User FacultyUrl) -> StaticPQ (Maybe UserId)
updateUser user = fmap fromOnly <$> (manipulateParams updateUserQ (normalizeUser user) >>= firstRow)

-- MARK: Utils

normalizeUser :: User FacultyUrl -> User FacultyUrl
normalizeUser User{..} = User {
    userId = userId,
    firstName = T.strip $ firstName,
    middleName = T.strip $ middleName,
    lastName = T.strip $ lastName,
    userFaculty = T.toLower . T.strip $ userFaculty,
    userEmail = T.toLower . T.strip $ userEmail
}

data UserRow = UserRow {
    userRowId :: UserId,
    userRowFirstName :: Text,
    userRowMiddleName :: Text,
    userRowLastName :: Text,
    userRowFacultyUrl :: FacultyUrl,
    userRowEmail :: Text,
    userRowIsValidated :: Bool,
    userRowFacultyName :: T.Text,
    userRowFacultyURL :: FacultyUrl,
    userRowFacultyPath :: T.Text,
    userRowFacultyCampusName :: T.Text,
    userRowFacultyCampusCode :: T.Text,
    userRowFacultyTags :: V.Vector T.Text,
    userRowFacultyAddress :: T.Text
} deriving (GHC.Generic, SOP.Generic, SOP.HasDatatypeInfo)

rowToUser :: UserRow -> User Faculty
rowToUser UserRow{..} = User {
        userId = userRowId,
        firstName = userRowFirstName,
        middleName = userRowMiddleName,
        lastName = userRowLastName,
        userFaculty = Faculty {
            facultyName = userRowFacultyName,
            facultyUrl = userRowFacultyURL,
            facultyPath = userRowFacultyPath,
            facultyCampusName = userRowFacultyCampusName,
            facultyCampusCode = userRowFacultyCampusCode,
            facultyTags = userRowFacultyTags,
            facultyAddress = userRowFacultyAddress
        },
        userEmail = userRowEmail
    }

queryFromTextAbbr :: Text -> Text
queryFromTextAbbr = T.unwords . L.intersperse "&" . map processTerm . filter (not . T.null) . T.split (not . isAlphaNum)
    where
        processTerm :: Text -> Text
        processTerm t = "(" <> t <> ":*" <> (
            if T.length t <= 5
                then (" | (" <>) . (<> ")") . T.unwords . L.intersperse "<->" . map (<> ":*") . map T.singleton . T.unpack $ t
                else mempty
            ) <> ")"

queryFromText :: Text -> Text
queryFromText = T.unwords . L.intersperse "&" . map processTerm . filter (not . T.null) . T.split (not . isAlphaNum)
    where
        processTerm :: Text -> Text
        processTerm t = t <> ":*"

directionToOperator
    :: P.PaginationDirection
    -> (ConditionOp schema0 from0 grouping0 params0 nullity0 nullity1 (ty :: PGType),
        (Expression schema2 from2 grouping2 params2 _ -> SortExpression schema2 from2 grouping2 params2))
directionToOperator P.BackPagination = ((.<), Desc)
directionToOperator P.ForwardPagination = ((.>), Asc)

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
    users :: M.Map UserId (User Faculty)
} deriving (Generic)

instance (ToJSON r) => ToJSON (ResponseWithUsers r)

instance (FromJSON r) => FromJSON (ResponseWithUsers r)

class HasUsers r where
    userSet :: r -> Set.Set UserId

    wrapIntoResponse :: r -> StaticPQ (Maybe (ResponseWithUsers r))
    wrapIntoResponse r = wrapIntoUsers r $ userSet r

instance HasUsers Dialog where
    userSet (GroupDialog chat _) = userSet chat
    userSet (UserDialog _ mess) = userSet mess

instance HasUsers GroupChat where
    userSet (GroupChat _ (Jsonb gUsers) _) = Set.fromList . M.keys $ gUsers

instance HasUsers Message where
    userSet (Message _ aId (UserChatDestination uId) _ _) = Set.fromList [aId, uId]
    userSet (Message _ aId (GroupChatDestination _) _ _) = Set.singleton aId

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

fromMaybe500 :: Maybe a -> StaticPQ a
fromMaybe500 Nothing = lift $ S.throwError S.err500
fromMaybe500 (Just a) = return a

fromMaybe404 :: Maybe a -> StaticPQ a
fromMaybe404 Nothing = lift $ S.throwError S.err404
fromMaybe404 (Just a) = return a
