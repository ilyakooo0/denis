{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Data.CyclicBuffer (
    CyclicBuffer(elems),
    mkCyclicBuffer,
    insertIntoCyclicBuffer
) where

data CyclicBuffer a = CyclicBuffer {
    elems :: [a],
    limit :: Int
}

mkCyclicBuffer :: Int -> CyclicBuffer a
mkCyclicBuffer lim = CyclicBuffer [] lim

insertIntoCyclicBuffer :: CyclicBuffer a -> a -> CyclicBuffer a
insertIntoCyclicBuffer (CyclicBuffer els lim) el = CyclicBuffer (take lim $ el:els) lim
