{-# LANGUAGE
    DataKinds ,
    DeriveGeneric ,
    OverloadedLabels,
    OverloadedStrings ,
    TypeApplications ,
    TypeOperators,
    OverloadedStrings #-}


module Data.PostElement (
    PostElement,
    ElementRow(..),
    elementsToRows,
    PostQuoteRow(..),
    PostQuote(..),
    rowsToElements
) where 

import Data.Int (Int64)
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import Data.Text (Text)
import Data.Aeson
import qualified Data.Map.Lazy as M
import Data.List (sortBy)
import Data.Function (on)
import Control.Applicative ((<|>))
import Servant.Docs (ToSample, toSamples, samples)

-- MARK: Documentation

instance ToSample (PostElement p) where
    toSamples _ = samples [
        Markdown "# This is a markdown title\nThis is body.",
        Markdown "## This is a subtitle\n_hello._"
        ]


-- MARK: PostElement

data PostElement a = Markdown Text
    | Latex Text
    | Image Text
    | Quote PostQuote
    | Attachment Text

    
data ElementRow a = ElementRow {
    rowElementId :: Int64, -- the source id
    rowElementAuthorId :: Int64,
    rowElementOrd :: Int64,
    rowElementMarkdown :: Maybe Text,
    rowElementLatex :: Maybe Text,
    rowElementImage :: Maybe Text,
    rowElementQuote :: Maybe Int64,
    rowElementAttachment :: Maybe Text
} deriving (GHC.Generic)

instance SOP.Generic (ElementRow a)
instance SOP.HasDatatypeInfo (ElementRow a)

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


-- MARK: PostElementRows

-- TODO: After embeding id into quote
rowsToElements :: M.Map Int64 [ElementRow PostQuote] -> M.Map Int64 PostQuoteRow -> [ElementRow p] -> Maybe [PostElement p]
rowsToElements erqs pqrs = traverse elementRowToElement . sortBy (compare `on` rowElementOrd)
    where 
        elementRowToElement :: ElementRow a -> Maybe (PostElement a)
        elementRowToElement (ElementRow _ _ _ (Just m) Nothing Nothing Nothing Nothing) = Just $ Markdown m
        elementRowToElement (ElementRow _ _ _ Nothing (Just l) Nothing Nothing Nothing) = Just $ Latex l
        elementRowToElement (ElementRow _ _ _ Nothing Nothing (Just i) Nothing Nothing) = Just $ Image i
        -- elementRowToElement (ElementRow _ _ Nothing Nothing Nothing (Just q) Nothing) = 
        --     fmap Quote $ M.lookup q pqrs >>= rowsToQuote erqs pqrs
        elementRowToElement (ElementRow _ _ _ Nothing Nothing Nothing Nothing (Just a)) = Just $ Attachment a
        elementRowToElement _ = Nothing

elementsToRows :: Int64 -> Int64 -> [PostElement p] -> [ElementRow p]
elementsToRows pId uId pb = map (uncurry finish) $ flip zip pb $ ElementRow pId uId <$> [0..] 
    where finish :: (Maybe Text -> Maybe Text -> Maybe Text -> Maybe Int64 -> Maybe Text -> ElementRow p) -> PostElement p -> ElementRow p
          finish f pe = case pe of 
            Markdown m -> f (Just m) Nothing Nothing Nothing Nothing
            Latex l -> f Nothing (Just l) Nothing Nothing Nothing
            Image i -> f Nothing Nothing (Just i) Nothing Nothing
            Quote q -> f Nothing Nothing Nothing (Just $ quoteId q) Nothing
            Attachment a -> f Nothing Nothing Nothing Nothing (Just a)


-- MARK: Quote 

data PostQuote = PostQuote {
    quoteId :: Int64,
    quoteBody :: [PostElement PostQuote],
    quotePostId :: Int64
}

instance ToJSON PostQuote where
    toJSON (PostQuote qId pb pId) = object [
        "id" .= qId, 
        "postId" .= pId,
        "quoteBody" .= pb ]

        
-- MARK: PostQuoteRow

data PostQuoteRow = PostQuoteRow {
    quoteRowId :: Int64,
    quoteRowPostId :: Int64
} deriving (GHC.Generic)

instance SOP.Generic PostQuoteRow
instance SOP.HasDatatypeInfo PostQuoteRow
        
rowsToQuote :: M.Map Int64 [ElementRow PostQuote] -> M.Map Int64 PostQuoteRow -> PostQuoteRow -> Maybe PostQuote
rowsToQuote erqs pqrs (PostQuoteRow qId pId) = do
    elementRows <- M.lookup qId erqs
    elements <- rowsToElements erqs pqrs elementRows
    return $ PostQuote qId elements pId
