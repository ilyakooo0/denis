{-# LANGUAGE FlexibleInstances #-}

module Data.Text.Validator where

import qualified Data.Text as T
import Data.Char
import Data.Limits

-- |Функция, проверяющая, является ли данный символ валидным символом текста.
isValidChar :: Char -> Bool
isValidChar '\n' = True
isValidChar '\r' = True
isValidChar c = isPrint c || generalCategory c == ModifierLetter

-- |Класс объектов, имеющих возможность проверить содержащийся в них текст на валидность.
class HasValidatableText t where
    validateText :: t -> Bool

-- |Текст с предикат.
data ExtraPredicate t = ExtraPredicate t (t -> Bool)

infixl 5 ~=
-- |Опертор для добавления предиката к тексту.
(~=) :: t -> (t -> Bool) -> ExtraPredicate t
(~=) = ExtraPredicate

instance (HasValidatableText t) => HasValidatableText (ExtraPredicate t) where
    validateText (t `ExtraPredicate` p) = validateText t && p t


infixl 5 ~<
-- |Опертор для добавления ограничений на длину сверху к тексту.
(~<) :: t -> Int -> RangeableText t
t ~< n = t <~> (0, n)

infixl 5 <~>
-- |Оператор для добавления ограничения на длину текста.
(<~>) :: t -> (Int, Int) -> RangeableText t
(<~>) = RangeableText

-- |Текст с ограничением по длине.
data RangeableText t = RangeableText t (Int, Int)

instance HasValidatableText (RangeableText String) where
    validateText (t `RangeableText` (lower, upper)) = len >= lower && len <= upper && validateText t
        where
            len = length t

instance HasValidatableText (RangeableText T.Text) where
    validateText (t `RangeableText` (lower, upper)) = len >= lower && len <= upper && validateText t
        where
            len = T.length t

instance HasValidatableText T.Text where
    validateText t = T.all isValidChar t && (not . T.null) t && T.length t < globalTextLimit

instance HasValidatableText String where
    validateText t = all isValidChar t && length t > 0 && length t < globalTextLimit