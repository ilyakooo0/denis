{-# LANGUAGE
    TupleSections,
    DeriveGeneric,
    FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Data.Completions (
    CompletionTree(..),
    mkCompletionTree
    ) where

import qualified Data.Map as M
import Data.Aeson
import Data.List
import Data.Function
import Data.Either
import Data.Bifunctor
import Data.Maybe
import GHC.Generics
import Servant.Docs

instance ToSample (CompletionTree Char) where
    toSamples _ = singleSample . mkCompletionTree $
        ["aa", "aaab", "ba", "b", "bab"]

data CompletionTree a = CompletionTreeNode {
    value :: Maybe [a],
    subtree :: M.Map a (CompletionTree a)
    } deriving (Show, Generic)

instance ToJSON (CompletionTree Char) where

mkCompletionTree :: (Ord a) => [[a]] -> CompletionTree a
mkCompletionTree = mkCompletionTree' . map (\ a -> (a, a)) . sort
    where
        mkCompletionTree' :: (Ord a) => [([a], [a])] -> CompletionTree a
        mkCompletionTree' = uncurry CompletionTreeNode . bimap listToMaybe (M.map mkCompletionTree' . M.fromList) .
            second (map (\ u@((k,_):_) -> (k, map snd u)) . groupBy ((==) `on` fst)) . partitionEithers . map processGroup
            where
                processGroup :: ([a], [a]) -> Either [a] (a, ([a], [a]))
                processGroup ([], b) = Left b
                processGroup ((k:rest), aa) = Right (k, (rest, aa))