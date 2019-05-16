{-# LANGUAGE
    TupleSections,
    DeriveGeneric,
    FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.Tags.Completions where

import qualified Data.Map as M
import Data.Aeson
import Data.List
import Data.Function
import Data.Either
import Data.Bifunctor
import Data.Maybe
import GHC.Generics
import Servant.Docs
import Data.Char

instance ToSample CompletionTree where
    toSamples _ = singleSample . mkCompletionTree $
        ["aa", "aaab", "ba", "b", "bab"]

-- |Объект дерева подсказок тегов.
data CompletionTree = CompletionTreeNode {
    value :: Maybe String,
    subtree :: M.Map Char CompletionTree
    } deriving (Show, Generic)

instance ToJSON CompletionTree where

-- |Функция, ктороая строит дерево подсказок по данному списку строк.
mkCompletionTree :: [String] -> CompletionTree
mkCompletionTree = mkCompletionTree' . map (\ a -> (map toLower a, a)) . sort
    where
        mkCompletionTree' :: [(String, String)] -> CompletionTree
        mkCompletionTree' = uncurry CompletionTreeNode . bimap listToMaybe (M.map mkCompletionTree' . M.fromList) .
            second (map (\ u@((k,_):_) -> (k, map snd u)) . groupBy ((==) `on` fst)) . partitionEithers . map processGroup
            where
                processGroup :: (String, String) -> Either String (Char, (String, String))
                processGroup ([], b) = Left b
                processGroup ((k:rest), aa) = Right (k, (rest, aa))