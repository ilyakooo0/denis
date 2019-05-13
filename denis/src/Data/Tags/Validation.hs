module Data.Tags.Validation (
    validateTag,
    validateTag'
    ) where

import Data.Text as T
import Data.Char

validateTag :: T.Text -> Maybe T.Text
validateTag tag =
    let
        normalised = T.strip $ tag
    in if validateTag' normalised
        then Just normalised
        else Nothing

validateTag' :: T.Text -> Bool
validateTag' = T.all validChar

validChar :: Char -> Bool
validChar '-' = True
validChar c = isAlphaNum c
