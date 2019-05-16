{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Data.CyclicBuffer where

-- |Объект циклического буфера
data CyclicBuffer a = CyclicBuffer {
    -- |Элементы в циклическом буфере
    elems :: [a],
    -- |Количество элементов в циклическом буфере
    limit :: Int
}

-- |Функция, создающая циклический буфер с данным размером
mkCyclicBuffer :: Int -> CyclicBuffer a
mkCyclicBuffer lim = CyclicBuffer [] lim

-- |Функция, добавлющая эоемент в циклический буфер
insertIntoCyclicBuffer :: CyclicBuffer a -> a -> CyclicBuffer a
insertIntoCyclicBuffer (CyclicBuffer els lim) el = CyclicBuffer (take lim $ el:els) lim
