{-# LANGUAGE OverloadedStrings,
    FlexibleInstances #-}

module Data.Sanitizer (
    sanitizeHtml,
    sanitizeHtml',
    Sanitizable(..)
    ) where

import qualified Data.Text as T

sanitizeHtml :: Sanitizable s => s -> s
sanitizeHtml = sanitize sanitizeHtml'

sanitizeHtml' :: T.Text -> T.Text
sanitizeHtml' = foldl1 (.) $ map (uncurry T.replace) htmlCharMap

htmlCharMap :: [(T.Text, T.Text)]
htmlCharMap = [
    ("<", "&lt;"),
    (">", "&gt;"),
    ("&", "&amp;")
    ]

class Sanitizable s where
    sanitize :: (T.Text -> T.Text) -> s -> s

instance Sanitizable T.Text where
    sanitize = id

instance (Sanitizable e, Functor f) => Sanitizable (f e) where
    sanitize = fmap . sanitize