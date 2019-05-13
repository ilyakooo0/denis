{-# LANGUAGE FlexibleInstances #-}

module Data.Text.Validator (
    HasValidatableText(..),
    isValidChar,
    ExtraPredicate(..),
    (~=),
    (~<),
    (<~>),
    RangeableText(..)
    ) where

import qualified Data.Text as T
import Data.Char

isValidChar :: Char -> Bool
isValidChar = isPrint

class HasValidatableText t where
    validateText :: t -> Bool

data ExtraPredicate t = ExtraPredicate t (t -> Bool)

infixl 5 ~=
(~=) :: t -> (t -> Bool) -> ExtraPredicate t
(~=) = ExtraPredicate

instance (HasValidatableText t) => HasValidatableText (ExtraPredicate t) where
    validateText (t `ExtraPredicate` p) = validateText t && p t



-- instance (Traversable f) => HasValidatableText (RangeableText (ExtraPredicate (f String))) where
--     validateText ((t `ExtraPredicate` p) `RangeableText` lims) = p t && validateText (t `RangeableText` lims)

-- instance HasValidatableText (RangeableText (ExtraPredicate (f T.Text))) where
--     validateText ((t `ExtraPredicate` p) `RangeableText` lims) = p t && validateText (t `RangeableText` lims)

-- instance (Foldable f) => HasValidatableText (RangeableText (ExtraPredicate (f T.Text))) where
--     validateText ((t `ExtraPredicate` p) `RangeableText` lims) = p t && validateText (t `RangeableText` lims)

infixl 5 ~<
(~<) :: t -> Int -> RangeableText t
t ~< n = t <~> (0, n)

infixl 5 <~>
(<~>) :: t -> (Int, Int) -> RangeableText t
(<~>) = RangeableText

data RangeableText t = RangeableText t (Int, Int)

instance HasValidatableText (RangeableText String) where
    validateText (t `RangeableText` (lower, upper)) = len >= lower && len <= upper && validateText t
        where
            len = length t

instance HasValidatableText (RangeableText T.Text) where
    validateText (t `RangeableText` (lower, upper)) = len >= lower && len <= upper && validateText t
        where
            len = T.length t

-- instance HasValidatableText (RangeableText T.Text) where
--     validateText (t `RangeableText` (lower, upper)) = len >= lower && len <= upper
--         where
--             len = T.length t

-- instance (Traversable t, HasValidatableText v) => HasValidatableText (t v) where
--     validateText = all validateText

instance HasValidatableText T.Text where
    validateText t = T.all isValidChar t && (not . T.null) t

instance HasValidatableText String where
    validateText t = all isValidChar t && length t > 0