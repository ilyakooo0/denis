{-# LANGUAGE
    OverloadedStrings,
    TupleSections #-}

module Data.Faculty.Parser (
    getFaculties
    ) where

import Network.HTTP.Req
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding
import Text.Taggy.DOM
import qualified Data.HashMap.Lazy as HM
import Data.Maybe
import Data.Faculty
import Control.Monad.IO.Class
import Data.Default
import qualified Data.Vector as V

getFaculties :: MonadIO m => m [Faculty]
getFaculties = runReq def $ do
    campuses <- getCampuses
    concat <$> mapM (\c -> getFacultiesForCampus c) campuses

sendReq
    :: MonadHttp f
    => Url scheme
    -> f LT.Text
sendReq url = decodeUtf8 . responseBody <$> req GET url NoReqBody lbsResponse mempty

getCampuses :: Req [Campus]
getCampuses = do
    let url = https "www.hse.ru" /: "org" /: "units"
    resp <- sendReq url
    let dom = parseDOM True resp
    return . fromMaybe [] $ do
        let els = elements $ dom
        campuses <- listToMaybe .
            filter (classIs "js-side_filter_content side_filter_content filter_switcher") $ els
        sequence . map processCampus . tail . texts' mempty . eltChildren $ campuses
    where
        processCampus :: (HM.HashMap T.Text T.Text, T.Text) -> Maybe Campus
        processCampus (hm, name) = do
            code <- HM.lookup "hse-value" hm
            return $ Campus name code


getFacultiesForCampus :: Campus -> Req [Faculty]
getFacultiesForCampus (Campus cName cCode) = do
    let url = https "www.hse.ru" /: "org" /: "units" /: "campus" /: cCode
    resp <- decodeUtf8 . responseBody <$> req GET url NoReqBody lbsResponse ("ltr" =: (42 :: Int))
    let dom = parseDOM True resp
    return . catMaybes . map processFaculty . filter (classIs "post person") $ elements dom
    where
        processFaculty :: Element -> Maybe Faculty
        processFaculty el = do
                let els = elements [NodeElement el]
                nameTag <- listToMaybe . texts' mempty . map (NodeElement) . filter (classIs "link large b") $ els
                let name = T.strip . snd $ nameTag
                url <- HM.lookup "href" . fst $ nameTag
                let tags = map (T.strip . snd) . texts' mempty . map (NodeElement) . filter (classIs "with-indent small tag-set") $ els
                let path = T.unwords . map (T.strip . snd) . texts' mempty . map (NodeElement) . filter (classIs "small") $ els
                return $ Faculty name url path cName cCode (V.fromList tags)

classIs :: T.Text -> Element -> Bool
classIs c = (== (Just c)) . HM.lookup "class" . eltAttrs

elements :: [Node] -> [Element]
elements ((NodeElement el):nn) = el : ((elements . eltChildren) el ++ elements nn)
elements _ = []

texts' :: HM.HashMap T.Text T.Text -> [Node] -> [(HM.HashMap T.Text T.Text, T.Text)]
texts' hm (NodeContent t: nn) = (hm, t) : texts' hm nn
texts' hm ((NodeElement el):nn) = ((texts' (HM.union (eltAttrs el) hm) (eltChildren el)) ++ texts' hm nn)
texts' _ _ = []

data Campus = Campus {
    campusName :: T.Text,
    campusCode :: T.Text
} deriving Show