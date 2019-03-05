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

type Schema = 
    '[
        "users" ::: 'Table ( '[
            "pk_users" ::: 'PrimaryKey '["userId"]] :=> 
            '[
                "userId" ::: 'Def :=> 'NotNull 'PGint8,
                "firstName" ::: 'NoDef :=> 'NotNull 'PGtext,
                "secondName" ::: 'NoDef :=> 'NotNull 'PGtext
            ]),
        "posts" ::: 'Table ( '[
            "pk_posts" ::: 'PrimaryKey '["postRowId"],
            "fk_author_id" ::: 'ForeignKey '["postRowAuthorId"] "users" '["userId"]] :=>
            '[
                "postRowId" ::: 'Def :=> 'NotNull 'PGint8,
                "postRowAuthorId" ::: 'NoDef :=> 'NotNull 'PGint8,
                "postRowUpdateTime" ::: 'NoDef :=> 'NotNull 'PGtimestamptz
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
            -- ]),
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
            ])
            
    ]
    

createTables :: Definition '[] Schema
createTables = createTable #users (
        serial8 `as` #userId :* 
        notNullable text `as` #firstName :*
        notNullable text `as` #secondName
        ) (
        primaryKey #userId `as` #pk_users
    ) >>> createTable #posts (
        serial8 `as` #postRowId :*
        notNullable int8 `as` #postRowAuthorId :*
        notNullable timestampWithTimeZone `as` #postRowUpdateTime
        ) (
            primaryKey #postRowId `as` #pk_posts :*
            foreignKey #postRowAuthorId #users #userId OnDeleteCascade OnUpdateCascade `as` #fk_author_id 
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
    )
-- quoteUniquenessCheck :: TableConstraintExpression Schema _ (Check '["rowElementOrd", "rowElementMarkdown", "rowElementLatex", "rowElementImage", "rowElementQuote", "rowElementAttachment"])
-- quoteUniquenessCheck = check (#rowElementOrd :* #rowElementMarkdown :* #rowElementLatex :* #rowElementImage :* #rowElementQuote :* #rowElementAttachment) $
--     ((ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0) +
--     (ifThenElse (isNull #rowElementOrd) 1 0)) .== 0