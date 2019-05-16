{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Data.PostElement where

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Text (Text)
import Data.Aeson
import Control.Applicative ((<|>))
import Servant.Docs (ToSample, toSamples, samples)
import Data.Text.Validator
import Data.Limits

-- MARK: Documentation

instance ToSample (PostElement p) where
    toSamples _ = samples [
        Markdown "# This is a markdown title\nThis is body.",
        Markdown "## This is a subtitle\n_hello._"
        ]

instance HasValidatableText (PostElement p) where
    validateText (Markdown t) = validateText $ t ~< postElementLengthLimit
    validateText (Latex t) = validateText $ t ~< postElementLengthLimit
    validateText (Image t) = validateText $ t ~< postElementLengthLimit
    validateText (Quote _) = False
    validateText (Attachment t) = validateText $ t ~< postElementLengthLimit


-- MARK: PostElement

-- |Обычный элемент поста.
data PostElement a
    -- |Markdown элемент поста.
    = Markdown Text
    -- |Latex элемент поста.
    | Latex Text
    -- |Изорбражение.
    | Image Text
    -- |Цитата.
    | Quote ()
    -- |Документ.
    | Attachment Text
    deriving (Show, GHC.Generic)

instance ToJSON (PostElement t) where
    toJSON (Markdown m) = object ["markdown" .= m]
    toJSON (Latex l) = object ["latex" .= l]
    toJSON (Image i) = object ["image" .= i]
    toJSON (Quote q) = object ["quote" .= q]
    toJSON (Attachment a) = object ["attachment" .= a]

instance FromJSON (PostElement t) where
    parseJSON = withObject "post element" $ \e ->
        Markdown <$> e .: "markdown" <|>
        Latex <$> e .: "latex" <|>
        Image <$> e .: "image" <|>
        -- Quote <$> e .: "quote" <|>
        Attachment <$> e .: "attachment"


-- -- MARK: PostElementRows

-- |Тип отображения из элемента поста с базы данных в другой объект.
type MkElementRow x = Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int64 -> Maybe Text -> x

-- |Функция, переводящая обычный элемент поста в другой объект.
elemenToRow :: MkElementRow x -> PostElement p -> x
elemenToRow f el = case el of
    Markdown m -> f (Just m) Nothing Nothing Nothing Nothing
    Latex l -> f Nothing (Just l) Nothing Nothing Nothing
    Image i -> f Nothing Nothing (Just i) Nothing Nothing
    -- Quote q -> f Nothing Nothing Nothing (Just $ (quoteId q)) Nothing
    Quote _ -> f Nothing Nothing Nothing Nothing Nothing
    Attachment a -> f Nothing Nothing Nothing Nothing (Just a)

-- |Отображение элемента поста с базы данных в обычный элемент поста.
rowsToElement :: MkElementRow (Maybe (PostElement p))
rowsToElement (Just m) Nothing Nothing Nothing Nothing = Just $ Markdown m
rowsToElement Nothing (Just l) Nothing Nothing Nothing = Just $ Latex l
rowsToElement Nothing Nothing (Just i) Nothing Nothing = Just $ Image i
-- rowsToElement Nothing Nothing Nothing (Just q) Nothing =
--     fmap Quote $ M.lookup q pqrs >>= rowsToQuote erqs pqrs
rowsToElement Nothing Nothing Nothing Nothing (Just a) = Just $ Attachment a
rowsToElement _ _ _ _ _ = Nothing

-- |Объект поста с базы данных
data PostElementRow a = PostElementRow {
    -- |Идентификатор элемента поста
    rowElementId :: Int64,
    -- |Порядковый номер элемента поста.
    rowElementOrd :: Int64,
    -- |Markdown элемент.
    rowElementMarkdown :: Maybe Text,
    -- |LaTeX элемент.
    rowElementLatex :: Maybe Text,
    -- |Изорбражение.
    rowElementImage :: Maybe Text,
    -- |Цитата.
    rowElementQuote :: Maybe Int64,
    -- |Приложение.
    rowElementAttachment :: Maybe Text
} deriving (GHC.Generic)

instance SOP.Generic (PostElementRow a)
instance SOP.HasDatatypeInfo (PostElementRow a)

-- |Функция, переводящая обычные элементы постоа в их представления для базы данных.
elementsToRows :: Int64 -> [PostElement a] -> [PostElementRow a]
elementsToRows pId els = map (\(el, f) -> elemenToRow f el) . zip els . map (PostElementRow pId) $ [0..]


-- -- MARK: Quote

-- data PostQuote = PostQuote {
--     quoteId :: Int64,
--     quoteBody :: [PostElement PostQuote],
--     quotePostId :: Int64
-- } deriving (Show, GHC.Generic)

-- instance ToJSON PostQuote where
--     toJSON (PostQuote qId pb pId) = object [
--         "id" .= qId,
--         "postId" .= pId,
--         "quoteBody" .= pb ]


-- -- MARK: PostQuoteRow

-- data PostQuoteRow = PostQuoteRow {
--     quoteRowId :: Int64,
--     quoteRowPostId :: Int64
-- } deriving (GHC.Generic)

-- instance SOP.Generic PostQuoteRow
-- instance SOP.HasDatatypeInfo PostQuoteRow

-- rowsToQuote :: M.Map Int64 [ElementRow PostQuote] -> M.Map Int64 PostQuoteRow -> PostQuoteRow -> Maybe PostQuote
-- rowsToQuote erqs pqrs (PostQuoteRow qId pId) = do
--     elementRows <- M.lookup qId erqs
--     elements <- rowsToElements erqs pqrs elementRows
--     return $ PostQuote qId elements pId
