{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings,
    KindSignatures #-}


module Data.Schema (
    Schema,
    createTables
) where

import Squeal.PostgreSQL
import Data.GroupChat
import Data.PostElement
import Data.Message

type Schema =
    '[
        "faculties" ::: 'Table ('[
                "pk_faculties" ::: 'PrimaryKey '["facultyUrl"]] :=>
            '[
                "facultyName" ::: 'NoDef :=> 'NotNull 'PGtext,
                "facultyUrl" ::: 'NoDef :=> 'NotNull 'PGtext,
                "facultyPath" ::: 'NoDef :=> 'NotNull 'PGtext,
                "facultyCampusName" ::: 'NoDef :=> 'NotNull 'PGtext,
                "facultyCampusCode" ::: 'NoDef :=> 'NotNull 'PGtext,
                "facultyTags" ::: 'NoDef :=> 'NotNull ('PGvararray ('NotNull 'PGtext))
            ]),
        "users" ::: 'Table ( '[
            "pk_users" ::: 'PrimaryKey '["userRowId"],
            "user_email" ::: 'Unique '["userRowEmail"],
            "fk_user_faculty" ::: 'ForeignKey '["userRowFacultyUrl"] "faculties" '["facultyUrl"]] :=>
            '[
                "userRowId" ::: 'Def :=> 'NotNull 'PGint8,
                "userRowFirstName" ::: 'NoDef :=> 'NotNull 'PGtext,
                "userRowMiddleName" ::: 'NoDef :=> 'NotNull 'PGtext,
                "userRowLastName" ::: 'NoDef :=> 'NotNull 'PGtext,
                "userRowFacultyUrl" ::: 'NoDef :=> 'NotNull 'PGtext,
                "userRowEmail" ::: 'NoDef :=> 'NotNull 'PGtext,
                "userRowIsValidated" ::: 'NoDef :=> 'NotNull 'PGbool
            ]),
        "posts" ::: 'Table ( '[
            "pk_posts" ::: 'PrimaryKey '["postRowId"],
            "fk_author_id" ::: 'ForeignKey '["postRowAuthorId"] "users" '["userRowId"]] :=>
            '[
                "postRowId" ::: 'Def :=> 'NotNull 'PGint8,
                "postRowAuthorId" ::: 'NoDef :=> 'NotNull 'PGint8,
                "postRowUpdateTime" ::: 'NoDef :=> 'NotNull 'PGtimestamptz,
                "postRowTags" ::: 'NoDef :=> 'NotNull ('PGvararray ('NotNull 'PGtext))
            ]),
        "quotes" ::: 'Table (
            '[
                "pk_post_quotes" ::: 'PrimaryKey '["quoteRowId"],
                "fk_quote_post_id" ::: 'ForeignKey '["quoteRowPostId"] "posts" '["postRowId"] ] :=>
            '[
                "quoteRowId" ::: 'Def :=> 'NotNull 'PGint8,
                "quoteRowPostId" ::: 'NoDef :=> 'NotNull 'PGint8
            ]),
        "postElements" ::: 'Table (
            '[
                "fk_post_quote_self_id" ::: 'ForeignKey '["rowElementId"] "posts" '["postRowId"],
                "fk_post_quote_id" ::: 'ForeignKey '["rowElementQuote"] "quotes" '["quoteRowId"]
                -- ,
                -- "unique_post_quote" ::: 'Check '["rowElementOrd", "rowElementMarkdown", "rowElementLatex", "rowElementImage", "rowElementQuote", "rowElementAttachment"]
                ] :=>
            '[
                "rowElementId" ::: 'NoDef :=> 'NotNull 'PGint8,
                "rowElementOrd" ::: 'NoDef :=> 'NotNull 'PGint8,
                "rowElementMarkdown" ::: 'NoDef :=> 'Null 'PGtext,
                "rowElementLatex" ::: 'NoDef :=> 'Null 'PGtext,
                "rowElementImage" ::: 'NoDef :=> 'Null 'PGtext,
                "rowElementQuote" ::: 'NoDef :=> 'Null 'PGint8,
                "rowElementAttachment" ::: 'NoDef :=> 'Null 'PGtext
            ]),
        "quoteElements" ::: 'Table (
            '[
                "fk_quotes_quote_self_id" ::: 'ForeignKey '["rowElementId"] "quotes" '["quoteRowId"],
                "fk_quote_quote_id" ::: 'ForeignKey '["rowElementQuote"] "quotes" '["quoteRowId"]
                -- ,
                -- "unique_quote_quote" ::: 'Check '["rowElementOrd", "rowElementMarkdown", "rowElementLatex", "rowElementImage", "rowElementQuote", "rowElementAttachment"]
                ] :=>
            '[
                "rowElementId" ::: 'NoDef :=> 'NotNull 'PGint8,
                "rowElementOrd" ::: 'NoDef :=> 'NotNull 'PGint8,
                "rowElementMarkdown" ::: 'NoDef :=> 'Null 'PGtext,
                "rowElementLatex" ::: 'NoDef :=> 'Null 'PGtext,
                "rowElementImage" ::: 'NoDef :=> 'Null 'PGtext,
                "rowElementQuote" ::: 'NoDef :=> 'Null 'PGint8,
                "rowElementAttachment" ::: 'NoDef :=> 'Null 'PGtext
            ]),
        "channels" ::: 'Table (
            '[
                "pk_channels" ::: 'PrimaryKey '["namedChannelFullId"],
                "fk_channels_users_id" ::: 'ForeignKey '["namedChannelFullOwner"] "users" '["userRowId"]
            ] :=>
            '[
                "namedChannelFullId" ::: 'Def :=> 'NotNull 'PGint8,
                "namedChannelFullOwner" ::: 'NoDef :=> 'NotNull 'PGint8,
                "namedChannelFullName" ::: 'NoDef :=> 'NotNull 'PGtext,
                "namedChannelFullTags" ::: 'NoDef :=> 'NotNull ('PGvararray ('NotNull 'PGtext)),
                "namedChannelFullPeopleIds" ::: 'NoDef :=> 'NotNull ('PGvararray ('NotNull 'PGint8))
            ]),
        "groupChats" ::: 'Table (
            '[
                "pk_group_chats" ::: 'PrimaryKey '["groupChatId"]
            ] :=>
            '[
                "groupChatId" ::: 'Def :=> 'NotNull 'PGint8,
                "groupChatUsers" ::: 'NoDef :=> 'NotNull (PG (Jsonb UserPermissions)),
                "groupChatName" ::: 'NoDef :=> 'NotNull 'PGtext
            ]),
        -- "userMessages" ::: 'Table (
        --     '[
        --         "pk_user_messages" ::: 'PrimaryKey '["userMessageId"],
        --         "fk_user_messages_from" ::: 'ForeignKey '["userMessageFrom"] "users" '["userRowId"],
        --         "fk_user_messages_to" ::: 'ForeignKey '["userMessageTo"] "users" '["userRowId"]

        --     ] :=>
        --     '[
        --         "userMessageId" ::: 'Def :=> 'NotNull 'PGint8,
        --         "userMessageFrom" ::: 'NoDef :=> 'NotNull 'PGint8,
        --         "userMessageTo" ::: 'NoDef :=> 'NotNull 'PGint8,
        --         "userMessageBody" ::: 'NoDef :=> 'NotNull (PG (Jsonb (PostElement UserMessage))),
        --         "userMessageTime" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
        --     ]),
        -- "groupChatMessages" ::: 'Table (
        --     '[
        --         "pk_group_messages" ::: 'PrimaryKey '["groupChatMessageId"],
        --         "fk_group_messages_group" ::: 'ForeignKey '["groupChatMessageGroupChatId"] "groupChats" '["groupChatId"],
        --         "fk_group_messages_author" ::: 'ForeignKey '["groupChatMessageAuthorId"] "users" '["userRowId"]
        --     ] :=>
        --     '[
        --         "groupChatMessageId" ::: 'Def :=> 'NotNull 'PGint8,
        --         "groupChatMessageGroupChatId" ::: 'NoDef :=> 'NotNull 'PGint8,
        --         "groupChatMessageAuthorId" ::: 'NoDef :=> 'NotNull 'PGint8,
        --         "groupChatMessageBody" ::: 'NoDef :=> 'NotNull (PG (Jsonb (PostElement GroupChatMessage))),
        --         "groupChatMessageTime" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
        --     ])
        "messages" ::: 'Table (
            '[
                "pk_messages" ::: 'PrimaryKey '["messageStorageId"],
                "fk_messages_author" ::: 'ForeignKey '["messageStorageAuthorId"] "users" '["userRowId"],
                "fk_messages_group" ::: 'ForeignKey '["messageStorageDestinationGroupId"] "groupChats" '["groupChatId"],
                "fk_messages_to_user" ::: 'ForeignKey '["messageStorageDestinationUserId"] "users" '["userRowId"]
            ] :=>
            '[
                "messageStorageId" ::: 'Def :=> 'NotNull 'PGint8,
                "messageStorageAuthorId" ::: 'NoDef :=> 'NotNull 'PGint8,
                "messageStorageDestinationGroupId" ::: 'NoDef :=> 'Null 'PGint8,
                "messageStorageDestinationUserId" ::: 'NoDef :=> 'Null 'PGint8,
                "messageStorageBody" ::: 'NoDef :=> 'NotNull (PG (Jsonb (PostElement Message))),
                "messageStorageTime" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
            ]),
        "tokens" ::: 'Table (
            '[
                -- "pk_token" ::: 'PrimaryKey '["tokenValue"],
                "fk_user_token" ::: 'ForeignKey '["tokenUserId"] "users" '["userRowId"]
            ] :=>
            '[
                "tokenUserId" ::: 'NoDef :=> 'NotNull 'PGint8,
                "tokenValue" ::: 'NoDef :=> 'NotNull 'PGbytea,
                "tokenExpiryDate" ::: 'NoDef :=> 'NotNull 'PGtimestamptz,
                "tokenVerificationCode" ::: 'NoDef :=> 'Null 'PGint4, -- null == verified
                "tokenActivationTriesLeft" ::: 'NoDef :=> 'NotNull 'PGint4
            ])

        -- "postsView" ::: 'View (RowPG PostRowResponse)
            -- ('[
            --     "postRowId" ::: 'NotNull 'PGint8,
            --     "postRowAuthorId" ::: 'NotNull 'PGint8,
            --     "rowElementId" ::: 'NotNull 'PGint8,
            --     "rowElementOrd" ::: 'NotNull 'PGint8,
            --     "rowElementMarkdown" ::: 'Null 'PGtext,
            --     "rowElementLatex" ::: 'Null 'PGtext,
            --     "rowElementImage" ::: 'Null 'PGtext,
            --     "rowElementQuote" ::: 'Null 'PGint8,
            --     "rowElementAttachment" ::: 'Null 'PGtext
            -- ])

    ]


createTables :: Definition '[] Schema
createTables = createTable #faculties (
        notNullable text `as` #facultyName :*
        notNullable text `as` #facultyUrl :*
        notNullable text `as` #facultyPath :*
        notNullable text `as` #facultyCampusName :*
        notNullable text `as` #facultyCampusCode :*
        notNullable (vararray text) `as` #facultyTags
        ) (
            primaryKey #facultyUrl `as` #pk_faculties
        ) >>> createTable #users (
        serial8 `as` #userRowId :*
        notNullable text `as` #userRowFirstName :*
        notNullable text `as` #userRowMiddleName :*
        notNullable text `as` #userRowLastName :*
        notNullable text `as` #userRowFacultyUrl :*
        notNullable text `as` #userRowEmail :*
        notNullable bool `as` #userRowIsValidated
        ) (
        primaryKey #userRowId `as` #pk_users :*
        unique #userRowEmail `as` #user_email :*
        foreignKey #userRowFacultyUrl #faculties #facultyUrl OnDeleteCascade OnUpdateCascade `as` #fk_user_faculty
    ) >>> createTable #posts (
        serial8 `as` #postRowId :*
        notNullable int8 `as` #postRowAuthorId :*
        notNullable timestampWithTimeZone `as` #postRowUpdateTime :*
        notNullable (vararray text) `as` #postRowTags
        ) (
            primaryKey #postRowId `as` #pk_posts :*
            foreignKey #postRowAuthorId #users #userRowId OnDeleteCascade OnUpdateCascade `as` #fk_author_id
    ) >>> createTable #quotes (
        serial8 `as` #quoteRowId :*
        notNullable int8 `as` #quoteRowPostId
        ) (
            primaryKey #quoteRowId `as` #pk_post_quotes :*
            foreignKey #quoteRowPostId #posts #postRowId OnDeleteCascade OnUpdateCascade `as` #fk_quote_post_id
    ) >>> createTable #postElements (
        notNullable int8 `as` #rowElementId :*
        notNullable int8 `as` #rowElementOrd :*
        nullable text `as` #rowElementMarkdown :*
        nullable text `as` #rowElementLatex :*
        nullable text `as` #rowElementImage :*
        nullable int8 `as` #rowElementQuote :*
        nullable text `as` #rowElementAttachment
        ) (
            foreignKey #rowElementId #posts #postRowId OnDeleteCascade OnUpdateCascade `as` #fk_post_quote_self_id :*
            foreignKey #rowElementQuote #quotes #quoteRowId OnDeleteCascade OnUpdateCascade `as` #fk_post_quote_id
    ) >>> createTable #quoteElements (
        notNullable int8 `as` #rowElementId :*
        notNullable int8 `as` #rowElementOrd :*
        nullable text `as` #rowElementMarkdown :*
        nullable text `as` #rowElementLatex :*
        nullable text `as` #rowElementImage :*
        nullable int8 `as` #rowElementQuote :*
        nullable text `as` #rowElementAttachment
        ) (
            foreignKey #rowElementId #quotes #quoteRowId OnDeleteCascade OnUpdateCascade `as` #fk_quotes_quote_self_id :*
            foreignKey #rowElementQuote #quotes #quoteRowId OnDeleteCascade OnUpdateCascade `as` #fk_quote_quote_id
    ) >>> createTable #channels (
        serial8 `as` #namedChannelFullId :*
        notNullable int8 `as` #namedChannelFullOwner :*
        notNullable text `as` #namedChannelFullName :*
        notNullable (vararray text) `as` #namedChannelFullTags :*
        notNullable (vararray int8) `as` #namedChannelFullPeopleIds
        ) (
            primaryKey #namedChannelFullId `as` #pk_channels :*
            foreignKey #namedChannelFullOwner #users #userRowId OnDeleteCascade OnUpdateCascade `as` #fk_channels_users_id
    -- ) >>> createView #postsView (select (
    --     #posts ! #postRowId :*
    --     #posts ! #postRowAuthorId :*
    --     --  #rowElementId :*
    --     #postElements ! #rowElementOrd :*
    --     #postElements ! #rowElementMarkdown :*
    --     #postElements ! #rowElementLatex :*
    --     #postElements ! #rowElementImage :*
    --     #postElements ! #rowElementQuote :*
    --     #postElements ! #rowElementAttachment
    --     ) (
    --     from ((table #posts) &
    --         (innerJoin (table #postElements)
    --             (#posts ! #postRowId .== #postElements ! #rowElementId))))
    ) >>> createTable #groupChats (
        serial8 `as` #groupChatId :*
        notNullable jsonb `as` #groupChatUsers :*
        notNullable text `as` #groupChatName
        ) (
            primaryKey #groupChatId `as` #pk_group_chats
    ) >>> createTable #messages (
        serial8 `as` #messageStorageId :*
        notNullable int8 `as` #messageStorageAuthorId :*
        nullable int8 `as` #messageStorageDestinationGroupId :*
        nullable int8 `as` #messageStorageDestinationUserId :*
        notNullable jsonb `as` #messageStorageBody :*
        notNullable timestampWithTimeZone `as` #messageStorageTime
        ) (
            primaryKey #messageStorageId `as` #pk_messages :*
            foreignKey #messageStorageAuthorId #users #userRowId OnDeleteCascade OnUpdateCascade `as` #fk_messages_author :*
            foreignKey #messageStorageDestinationGroupId #groupChats #groupChatId OnDeleteCascade OnUpdateCascade `as` #fk_messages_group :*
            foreignKey #messageStorageDestinationUserId #users #userRowId OnDeleteCascade OnUpdateCascade `as` #fk_messages_to_user
    ) >>> createTable #tokens (
        notNullable int8 `as` #tokenUserId :*
        notNullable bytea `as` #tokenValue :*
        notNullable timestampWithTimeZone `as` #tokenExpiryDate :*
        nullable int4 `as` #tokenVerificationCode :*
        notNullable int4 `as` #tokenActivationTriesLeft
        ) (
            foreignKey #tokenUserId #users #userRowId OnDeleteCascade OnUpdateCascade `as` #fk_user_token
    )
-- quoteUniquenessCheck :: TableConstraintExpression Schema _ (Check '["rowElementOrd", "rowElementMarkdown", "rowElementLatex", "rowElementImage", "rowElementQuote", "rowElementAttachment"])
-- quoteUniquenessCheck = check (#rowElementOrd :* #rowElementMarkdown :* #rowElementLatex :* #rowElementImage :* #rowElementQuote :* #rowElementAttachment) $
--     ((ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0)) .== 0
