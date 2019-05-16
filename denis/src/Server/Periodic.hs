module Server.Periodic where

import Control.Monad.IO.Class
import Control.Concurrent
import System.Random
import Control.Monad

seconds :: Int
seconds = 1000000

minutes :: Int
minutes = 60 * seconds

hours :: Int
hours = 60 * minutes

periodically :: (MonadIO m) => Int -> IO () -> m ThreadId
periodically delay action = liftIO $ do
    forkIO $ do
        initialDelay <- randomRIO (0, delay)
        threadDelay initialDelay
        forever $ do
            action
            threadDelay delay
