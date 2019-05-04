{-# LANGUAGE
    LambdaCase,
    FlexibleInstances,
    OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Tags.Suggestions (
    suggestTags
    ) where

import qualified Data.Text as T
import Servant.Docs
import Data.List

instance {-# Overlapping #-} ToSample [T.Text] where
    toSamples _ = singleSample ["machineLearning", "cs"]

suggestTags :: T.Text -> [T.Text]
suggestTags t =
        let
            ws = T.words t
            l = length ws
            indecies = take (3 + (l `mod` 3)) . map (`mod` l) $ [8849, 11161, 12647, 14081, 16267]
        in nub . map (ws !!) $ indecies
