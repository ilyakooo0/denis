module Data.Tags.Validation (
    validateTag,
    validateTag'
    ) where

import Data.Text as T
import Data.Char as C
import qualified Data.Set as S

validateTag :: T.Text -> Maybe T.Text
validateTag tag =
    let
        normalised = T.strip $ tag
    in if validateTag' normalised
        then Just normalised
        else Nothing

validateTag' :: T.Text -> Bool
validateTag' = T.all validChar

validChars :: S.Set Char
validChars = S.fromList "йцукенгшщзхъфывапролджэёячсмитьбюqwertyuiopasdfghjklzxcvbnm1234567890_"

validChar :: Char -> Bool
validChar c = S.member (C.toLower c) validChars
