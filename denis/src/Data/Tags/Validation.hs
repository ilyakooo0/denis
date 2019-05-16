module Data.Tags.Validation where

import Data.Text as T
import Data.Char

-- |Функция, валилирующая хэштег, возвращающая тег в монаде `Maybe`
validateTag :: T.Text -> Maybe T.Text
validateTag tag =
    let
        normalised = T.strip $ tag
    in if validateTag' normalised
        then Just normalised
        else Nothing

-- |Функция, проверяющая, является ли данный тег валидным.
validateTag' :: T.Text -> Bool
validateTag' = T.all validChar

-- |Функция, проверяющая, является ли данный символ валидным внутри тега.
validChar :: Char -> Bool
validChar '-' = True
validChar c = isAlphaNum c
