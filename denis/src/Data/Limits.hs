{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Data.Limits where

import Data.Int (Int64)

maxChannelName :: Int
maxChannelName = 60

maxGroupChatName :: Int
maxGroupChatName = 60

maxMessageLimitName :: Int
maxMessageLimitName = 60

postElementLengthLimit :: Int
postElementLengthLimit = 35000

userFieldLengthLimit :: Int
userFieldLengthLimit = 50

maxNumberOfTags :: Int
maxNumberOfTags = 20

channelCountLimit :: Int64
channelCountLimit = 128

listLimit :: Int
listLimit = 100

globalTextLimit :: Int
globalTextLimit = 50000

groupChatSizeLimit :: Int
groupChatSizeLimit = 256