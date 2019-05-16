{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Limits where

import Data.Int (Int64)

-- |Максимально возможное количество количество символов в нвазвании канала.
maxChannelName :: Int
maxChannelName = 60

-- |Максимально возможное количество количество символов в нвазвании групповой беседы.
maxGroupChatName :: Int
maxGroupChatName = 60

-- |Максимальное количество элементов в элементе поста
postElementLengthLimit :: Int
postElementLengthLimit = 35000

-- |Максимальное число символов в поле пользователя
userFieldLengthLimit :: Int
userFieldLengthLimit = 50

-- |максимальное число тегов
maxNumberOfTags :: Int
maxNumberOfTags = 20

-- |максимальное число именнованных каналов у пользователя
channelCountLimit :: Int64
channelCountLimit = 128

-- |Максимальное число элементов, возвращаемых в списке запроса
listLimit :: Int
listLimit = 100

-- |глобальное ограничение на количество символов в пользовательском тексте.
globalTextLimit :: Int
globalTextLimit = 50000

-- |Максимальное число пользователей в групповой беседе
groupChatSizeLimit :: Int
groupChatSizeLimit = 256