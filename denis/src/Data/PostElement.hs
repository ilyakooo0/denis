{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}


module Data.PostElement (
    elemenToRow,
    rowsToElement,
    MkElementRow,
    PostElementRow(..),
    PostElement(..),
    elementsToRows,
    PostQuote(..),
    stripPostElement
) where

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Text (Text, strip)
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

data PostElement a = Markdown Text
    | Latex Text
    | Image Text
    | Quote ()
    | Attachment Text
    deriving (Show, GHC.Generic)

stripPostElement :: (PostElement a) -> (PostElement a)
stripPostElement (Markdown t) = Markdown (strip t)
stripPostElement (Latex t) = Latex (strip t)
stripPostElement u = u

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

type MkElementRow x = Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int64 -> Maybe Text -> x

elemenToRow :: MkElementRow x -> PostElement p -> x
elemenToRow f el = case el of
    Markdown m -> f (Just m) Nothing Nothing Nothing Nothing
    Latex l -> f Nothing (Just l) Nothing Nothing Nothing
    Image i -> f Nothing Nothing (Just i) Nothing Nothing
    -- Quote q -> f Nothing Nothing Nothing (Just $ (quoteId q)) Nothing
    Quote _ -> f Nothing Nothing Nothing Nothing Nothing
    Attachment a -> f Nothing Nothing Nothing Nothing (Just a)

rowsToElement :: MkElementRow (Maybe (PostElement p))
rowsToElement (Just m) Nothing Nothing Nothing Nothing = Just $ Markdown m
rowsToElement Nothing (Just l) Nothing Nothing Nothing = Just $ Latex l
rowsToElement Nothing Nothing (Just i) Nothing Nothing = Just $ Image i
-- rowsToElement Nothing Nothing Nothing (Just q) Nothing =
--     fmap Quote $ M.lookup q pqrs >>= rowsToQuote erqs pqrs
rowsToElement Nothing Nothing Nothing Nothing (Just a) = Just $ Attachment a
rowsToElement _ _ _ _ _ = Nothing

data PostElementRow a = PostElementRow {
    rowElementId :: Int64,
    rowElementOrd :: Int64,
    rowElementMarkdown :: Maybe Text,
    rowElementLatex :: Maybe Text,
    rowElementImage :: Maybe Text,
    rowElementQuote :: Maybe Int64,
    rowElementAttachment :: Maybe Text
} deriving (GHC.Generic)

instance SOP.Generic (PostElementRow a)
instance SOP.HasDatatypeInfo (PostElementRow a)


elementsToRows :: Int64 -> [PostElement a] -> [PostElementRow a]
elementsToRows pId els = map (\(el, f) -> elemenToRow f el) . zip els . map (PostElementRow pId) $ [0..]


-- -- MARK: Quote

data PostQuote = PostQuote {
    quoteId :: Int64,
    quoteBody :: [PostElement PostQuote],
    quotePostId :: Int64
} deriving (Show, GHC.Generic)

instance ToJSON PostQuote where
    toJSON (PostQuote qId pb pId) = object [
        "id" .= qId,
        "postId" .= pId,
        "quoteBody" .= pb ]


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
